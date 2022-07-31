# ------------------------------------------------------------------------- #
#            Limpieza y generación de ofertas de servicio de salud          #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rlang)
library(haven)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library(pins)
library(labelled)
library(fuzzyjoin) 

# Carpeta PINS ------------------------------------------------------------

path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

# Funciones auxiliares ----------------------------------------------------

read_funciton <- function(file){
  
  if(str_detect(file,".sav$|.SAV$")){
    tabla <- read_spss(file) 
  }else{
    tabla <- read_csv2(file)
  }
  
  return(tabla)
  
  glimpse(tabla)
}

# Archivos y lectura ------------------------------------------------------

archivos_ras <- list.files(path = str_c("../nuevos_hospitales/ras/"),full.names = T)

anios <- str_extract(archivos_ras,"[:digit:]{4}")


ras <- map(archivos_ras,read_funciton)

# Empezamos con el diccionario --------------------------------------------


dicc <- pin_read(carpeta,"diccionario_RAS") %>% 
  mutate(label = str_extract(str_to_lower(descripcion),".* [:digit:]{1}"),
         label = str_remove_all(label,"[:punct:]"),
         label = str_remove(label," o de llamada y menos de 4"),
         label = str_replace_all(label,"[:space:]","_"),
         label = str_replace_all(label,"_+","_"),
         bloque_var = str_remove_all(label,"_[46]"),
         bloque_var = str_remove_all(bloque_var,"_eventual"))

# Excluimos los totales, para evitar contar dos veces ---------------------

excluidos <- dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"uso inec")) %>% 
  pull(codigo_de_la_variable) %>% 
  str_to_lower()

  
# Identificadores basicos de la base del RAS ------------------------------

identificaciones <- c("parr_ubi", "clase","sector","area")

# Diccionario de clase de establecimiento ---------------------------------

dicc_clase <- tribble(
  ~clase,~descripcion,
  1 , "Hospital Básico",
  2 , "Hospital General",
  3 , "Infectología",
  4 , "Gineco-Obstétrico",
  5 , "Pediátrico",
  6 , "Psiquiátrico y Sanatorio de Alcohólicos",
  7 , "Dermatológico (leprocomios)",
  8 , "Oncológico",
  9 , "Neumológico (Antituberculoso)",
  10 , "Geriátrico",
  11 , "Hospital de Especialidades",
  12 , "Clínica General (sin especialidad) (Privada)",
  13 , "Gineco-Obstetricia",
  14 , "Pediatría",
  15 , "Traumatología",
  16 , "Psiquiatría",
  17 , "Otras Clínicas especializadas",
  20 , "Puesto de Salud",
  21 , "Subcentro de Salud",
  22 , "Centro de Salud A",
  23 , "Centro de Salud B",
  24 , "Centro de Salud C",
  25 , "Dispensario Médico (Policlínico)",
  26 , "Consultorio General",
  27 , "Consultorio de Especialidad(es) Clínico -Quirúrgico",
  28 , "Centro de Especialidades",
  29 , "Centro Clínico-Quirúrgico Ambulatorio (Hospital del Día)",
  30 , "Centros especializados",
  31 , "Otros establecimientos sin internación"
)


# Doctores y tiempos ------------------------------------------------------

# Variables de doctores a tiempo completo

dicc <- dicc %>%
  mutate(bloque_var = str_replace(bloque_var,"_8","_completos"),
         codigo_de_la_variable = str_to_lower(codigo_de_la_variable)) 

tiempos_completos <-  dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"8 horas")) 
  

# Variables de doctores a tiempo parcial

tiempos_parti <- dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"6 horas|4 horas|eventual"))


tiempos_parti_list <- tiempos_parti %>% 
  split(.$bloque_var)


dicc_clase <- 
  dicc_clase %>% 
  mutate(
    clase = if_else(str_count(clase) == 1,str_c("0",clase),as.character(clase)),
    clase = str_c("clase",clase,sep = "_"))


# Crear la oferta de doctores ---------------------------------------------


ras_1 <- 
  map2(
    .x = ras,
    .y = anios,
       
    ~.x %>% 
      rename_with(str_to_lower) %>% 
      mutate(parr_ubi = if_else(condition = str_count(parr_ubi) != 6,
                                true = str_c(prov_ubi,cant_ubi,parr_ubi),
                                false = as.character(parr_ubi))) %>% 
      select( one_of(identificaciones), # Seleccion de variables de identifiación de la Unidad mínima de análisis
              -one_of(excluidos),
              everything()) %>% 
      mutate(
        parroquia_factor = to_factor(parr_ubi),
        clase_factor = to_factor(clase),
        across(.cols = everything(),.fns = as.character),
             anio = .y)
  ) %>% 
  reduce(bind_rows)


# ras[[1]]$cant_ubi %>% to_factor()

# dicc_clase %>% View


# Oferta de unidades de salud por clase -----------------------------------

write_rds(ras_1,file = "codam_2022/bases_rds/ras_procesando.rds")

ras_1 <- read_rds("codam_2022/bases_rds/ras_procesando.rds")

ras_1 %>% 
  rename_with(.cols = tiempos_completos$codigo_de_la_variable,
              .fn = ~tiempos_completos$bloque_var)


# Asignamos el secuencial del estabecimiento ------------------------------


ras_1 <- ras_1 %>% 
  group_by(anio) %>% 
  mutate(estab_id_secuencial = row_number()) 


# Creamos las categorias de hospitales de acuerdo a su oferta de ---------
# médicos o de personal de salud

oferta_medicos <- bind_rows(tiempos_completos,
                            tiempos_parti) %>% 
  mutate(bloque_var = if_else(str_detect(bloque_var,"completos$",negate = TRUE), 
                              true = str_c(bloque_var ,"_parcial"),
                              false = bloque_var))

full_completos <- oferta_medicos %>% 
  pull(codigo_de_la_variable)

especialidades <- oferta_medicos %>% 
  split(.$bloque_var) %>% 
  map(pull,codigo_de_la_variable) 


equipamientos_por_especialidad <- imap(.x = especialidades,
     .f = ~{
       
       nombre_bloque <- .y
       
       variables_filtro <- .x
       
       equipados <- ras_1 %>% 
         ungroup() %>% 
         filter(if_all(.cols = all_of(variables_filtro),
                       .fns = ~.x > 0))  %>% 
         mutate(equipado = 1)
       
       
       no_equipados <- ras_1 %>% 
         ungroup() %>% 
         anti_join(equipados, by = c("estab_id_secuencial","anio")) %>% 
         mutate(equipado = 0)
       
       tag_establecimiento <- bind_rows(
         equipados,
         no_equipados
       ) %>% 
         select(estab_id_secuencial,anio,equipado) %>% 
         rename_with(.cols =  equipado,~str_c("equip", nombre_bloque,sep = "_")) %>% 
       
       return(tag_establecimiento)
       
     }) %>% 
  reduce(full_join,
         by = c("estab_id_secuencial","anio"))



# Clasificaciones en función del equipamiento -----------------------------


hospitales_full_equipados <-  equipamientos_por_especialidad %>% 
  ungroup() %>% 
  filter(if_any(.cols = 3:20,
                .fns = ~.x > 0)) %>% 
  pull(estab_id_secuencial)

# No hay hospitales que tengan tooooooodas las categorias

hospitales_full_tiempo_c <-  equipamientos_por_especialidad %>% 
  ungroup() %>% 
  filter(if_any(.cols = matches("completo"),
                .fns = ~.x > 0)) %>% 
  pull(estab_id_secuencial)

hospitales_full_tiempo_p <-  equipamientos_por_especialidad %>% 
  ungroup() %>% 
  filter(if_any(.cols = matches("parcial"),
                .fns = ~.x > 0)) %>% 
  pull(estab_id_secuencial)

equipamientos_por_especialidad <- equipamientos_por_especialidad %>% 
  mutate(
    suma_completos = rowSums(across(.cols = matches("completo"),sum)),
    suma_parciales = rowSums(across(.cols = matches("parcial"),sum)),
    full_equip_t_c = as.numeric(estab_id_secuencial %in% hospitales_full_tiempo_c),
    full_equip_t_p = as.numeric(estab_id_secuencial %in% hospitales_full_tiempo_p),
    my3_equip_t_c = as.numeric(suma_completos > 3),
    my3_equip_t_p = as.numeric(suma_parciales > 3),
  ) %>% 
  inner_join(ras_1 %>% 
               select(estab_id_secuencial,parr_ubi,anio))




total_oferta_medica <- equipamientos_por_especialidad %>%
  select(estab_id_secuencial,parr_ubi,anio,everything()) %>% 
  group_by(parr_ubi,anio) %>% 
  summarise(across(.cols = where(is.numeric),
                   .fns = sum,na.rm = T)) 


total_oferta_medica %>% 
  select(-estab_id_secuencial) %>% 
  pin_write(board = carpeta,x = .,name = "bdd_establecimientos_equipamiento")


# pin_read(board = carpeta,name = "bdd_establecimientos_equipamiento")

# 
# parr_clase_estab <- ras_1 %>% 
#   # mutate(clase = if_else(str_count(clase) == 1,str_c("0",clase),as.character(clase))) %>% 
#   count(parr_ubi,clase_factor,anio) %>% 
#   # mutate(
#   #   clase = str_c("clase",clase,sep = "_")) %>% 
#   pivot_wider(names_from = "clase_factor",values_from = "n") %>% 
#   mutate(across(.cols = where(is.numeric),.fns = replace_na,replace = 0)) 
# 
# 
# parr_clase_estab <- ras_1 %>% 
#   # mutate(clase = if_else(str_count(clase) == 1,str_c("0",clase),as.character(clase))) %>%
#   count(parr_ubi,clase_factor,anio)
# 
# # 
# # read_rds("codam_2022/bases_rds/homol_hospitales.rds") %>% 
# #   write_tsv(file = "codam_2022/bases_rds/homol_estab.txt")
# 
# 
# # Doctores ----------------------------------------------------------------
# 
# tiempos_parti_df <- tiempos_parti %>% 
#   map(~{
#     
#     vars <- .x %>% 
#       pull(codigo_de_la_variable) %>% 
#       str_to_lower()
#     
#     name <- .x %>% 
#       pull(bloque_var) %>% 
#       unique() %>% 
#       str_c(.,"_parcial")
#     
#     ras_1 %>% 
#         mutate(across(.cols = one_of(vars),as.numeric)) %>% 
#       group_by(parr_ubi,anio) %>% 
#       summarise(across(.cols = one_of(vars),sum,na.rm = T)) %>% 
#       ungroup() %>% 
#       rowwise() %>% 
#       mutate(
#         suma = rowSums(across(.cols = one_of(vars)))) %>% 
#       select(parr_ubi,suma,anio) %>% 
#       rename_with(.cols = "suma",~name) 
#     
#   }) %>% 
#   reduce(full_join)
# 
# # dir.create("codam_2022/bases_rds")
# 
# write_rds(tiempos_parti_df,"codam_2022/bases_rds/doctors_tiempo_parcial.rds")
# write_rds(medicos_completos,"codam_2022/bases_rds/doctors_tiempo_complet.rds")
# write_rds(parr_clase_estab,"codam_2022/bases_rds/clase_establecimientos.rds")
# 
# 
# # write de los pins -------------------------------------------------------
# 
# 
# tiempos_parti_df <-  read_rds("codam_2022/bases_rds/doctors_tiempo_parcial.rds")
# 
# medicos_completos <- read_rds("codam_2022/bases_rds/doctors_tiempo_complet.rds")
# 
# 
# 
# parr_clase_estab <-  read_rds("codam_2022/bases_rds/clase_establecimientos.rds")

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

parr_clase_estab <- ras_1 %>% 
  # mutate(clase = if_else(str_count(clase) == 1,str_c("0",clase),as.character(clase))) %>% 
  count(parr_ubi,clase_factor,anio) %>% 
  # mutate(
  #   clase = str_c("clase",clase,sep = "_")) %>% 
  pivot_wider(names_from = "clase_factor",values_from = "n") %>% 
  mutate(across(.cols = where(is.numeric),.fns = replace_na,replace = 0)) 


long_stab <- parr_clase_estab %>% 
  pivot_longer(cols = -c(1:2),names_to = "clase",values_to = "establecimientos")


count(long_stab,clase)

long_stab %>%
  group_by(anio,clase) %>% 
  summarise(across(establecimientos,sum,na.rm = T)) %>% 
  ggplot() +
  geom_tile(aes(x = anio,
                y = clase,
                fill = log(establecimientos)))
  


# Doctores ----------------------------------------------------------------

tiempos_completos <- dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"8 horas")) %>% 
  mutate(bloque_var = str_replace(bloque_var,"_8","_completos"),
         codigo_de_la_variable = str_to_lower(codigo_de_la_variable))


medicos_completos <- ras_1 %>% 
  rename_with(.cols = tiempos_completos$codigo_de_la_variable,~tiempos_completos$bloque_var) 

tiempos_parti <- dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"6 horas|4 horas|eventual")) %>% 
  split(.$bloque_var)


tiempos_parti_df <- tiempos_parti %>% 
  map(~{
    
    vars <- .x %>% 
      pull(codigo_de_la_variable) %>% 
      str_to_lower()
    
    name <- .x %>% 
      pull(bloque_var) %>% 
      unique() %>% 
      str_c(.,"_parcial")
    
    ras_1 %>% 
        mutate(across(.cols = one_of(vars),as.numeric)) %>% 
      group_by(parr_ubi,anio) %>% 
      summarise(across(.cols = one_of(vars),sum,na.rm = T)) %>% 
      ungroup() %>% 
      rowwise() %>% 
      mutate(
        suma = rowSums(across(.cols = one_of(vars)))) %>% 
      select(parr_ubi,suma,anio) %>% 
      rename_with(.cols = "suma",~name) 
    
  }) %>% 
  reduce(full_join)

# dir.create("codam_2022/bases_rds")

write_rds(tiempos_parti_df,"codam_2022/bases_rds/doctors_tiempo_parcial.rds")
write_rds(medicos_completos,"codam_2022/bases_rds/doctors_tiempo_complet.rds")
write_rds(parr_clase_estab,"codam_2022/bases_rds/clase_establecimientos.rds")


# write de los pins -------------------------------------------------------


tiempos_parti_df <-  read_rds("codam_2022/bases_rds/doctors_tiempo_parcial.rds")

medicos_completos <- read_rds("codam_2022/bases_rds/doctors_tiempo_complet.rds")



parr_clase_estab <-  read_rds("codam_2022/bases_rds/clase_establecimientos.rds")


# -------------------------------------------------------------------------
# Pinche indicador que te he procrastinado #
# -------------------------------------------------------------------------


# library -----------------------------------------------------------------

library(tidyverse)
library(pins)
library(haven)
library(labelled)


# lectura -----------------------------------------------------------------

archivos_egr <- list.files(path = str_c("../nuevos_hospitales/egresos/"),full.names = T)

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



tasas_mov_mor <- function(panel,group_var){
  
  tabla_1 <- panel %>% 
    group_by_at(group_var) %>% 
    summarise(across(c(numero_movilizados,
                       numero_fallecidos,
                       egresos),sum)) %>% 
    ungroup() %>% 
    mutate(tasa_movilidad = numero_movilizados/egresos,
           tasa_morbilidad = numero_fallecidos/egresos)
  
  return(tabla_1)
  
}

graph_mapa <- function(tabla,fill_var,dpa,centroides,year){
  
  
  mapa_plot <- tabla %>% 
    split(.[[year]]) %>% 
    map(~{
      
      mapa_pichincha %>% 
        left_join(.x,by = c("DPA_PARROQ" = dpa)) %>% 
        fill({{year}},.direction = "downup")
      
    }) %>%
    keep(~length(.x)!= 0) %>% 
    reduce(bind_rows) %>% 
    filter(!is.na(anio_egr))
  
  # browser()
  
  
  plot <- ggplot(data = mapa_plot) +
    geom_polygon(aes_string(x = "long",
                            y = "lat",
                            group = "group",
                            fill = fill_var),
                 color = "white",size = 0.5)+
    geom_text(data = centroides,
              aes(x = x, y = y,label = DPA_PARROQ),size = 2,
              color = "gray") +
    # scale_fill_viridis_c(option = "inferno")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(plot)
  
}

# Lectura de los datos ----------------------------------------------------

# Egresos hospitalarios: SAV para mantener las etiquetas

egresos <- map(archivos_egr,read_funciton)


# En el manuscript se indica la la formula para el indice de demanda satisfecha:
# IDS = A - E / (R + A) * 100


# Primero identificamos las emfermedades ----------------------------------

egresos <- egresos %>% 
  map(mutate,
      genito =  str_detect(cau_cie10,"^N+"),
      partos = str_detect(cau_cie10,"^O+"),
      perina = str_detect(cau_cie10,"^P+"),
      compli = str_detect(cau_cie10,"^P00|^P01|^P02|^P03|^P04"),
      across(.cols = c(genito,
                       partos,
                       perina,
                       compli),.fns = as.numeric))


# Filtramos para las edades de interes ------------------------------------


egresos <-
  egresos %>% 
  map(filter,
      between(x = edad,left = 10,right = 49))


egresos %>% 
  map(count,sexo) %>% 
  map(mutate,across(sexo,to_factor)) %>% 
  map2(.y = 2015:2019,~.x %>% mutate(anio = .y)) %>% 
  pin_write(carpeta,x = .,
            name = "ehh_res_1",
            title = "Conteo de personas entre 10 y 49 años")

# Filtramos para mujeres unicamente ---------------------------------------

egresos <-
  egresos %>% 
  map(filter,
      sexo == 2)


# Panel temporal ----------------------------------------------------------

panel_egresos <-
  egresos %>%
  map(mutate,
      across(.cols = c(parr_ubi,parr_res),as.character),
      locales = as.numeric(parr_ubi == parr_res),
      movilidad = 1 - locales,
      fallecido = as.numeric(con_egrpa %in% c(2,3))) 

# Atraidos ----------------------------------------------------------------

panel_atraidos <- panel_egresos %>%
  map(mutate,
      across(.cols = c(fallecido,
                       genito,
                       partos,
                       perina,
                       compli),
             .fns = list(mov = ~ as.numeric(.x == 1 & movilidad)))) %>% 
  map(group_by,parr_ubi) %>% 
  map(summarise,
      total_atraidos = sum(movilidad,na.rm = T),
      total_locales_ubi = sum(locales,na.rm = T),
      atraidos_fallecidos = sum(fallecido_mov,na.rm = T),
      atraidos_genito = sum(genito_mov,na.rm = T),
      atraidos_partos = sum(partos_mov,na.rm = T),
      atraidos_perina = sum(perina_mov,na.rm = T),
      atraidos_compli = sum(compli_mov,na.rm = T),
      egresos = n())

panel_expulsados <- panel_egresos %>%
  map(mutate,
      across(.cols = c(fallecido,
                       genito,
                       partos,
                       perina,
                       compli),
             .fns = list(mov = ~ as.numeric(.x == 1 & movilidad)))) %>% 
  map(group_by,parr_res) %>% 
  map(summarise,
      total_expulsados = sum(movilidad,na.rm = T),
      total_locales_res = sum(locales,na.rm = T),
      expulsados_fallecidos = sum(fallecido_mov,na.rm = T),
      expulsados_genito = sum(genito_mov,na.rm = T),
      expulsados_partos = sum(partos_mov,na.rm = T),
      expulsados_perina = sum(perina_mov,na.rm = T),
      expulsados_compli = sum(compli_mov,na.rm = T),
      egresos = n())

diccionario <- tribble(
  ~Variable,            ~ `Territorio de agregación`   ,  ~Detalle,
  "total_atraidos" ,       "parr_ubi",                     "Variable A en el paper. Número de movilizados que ingrean en la parroquia donde está el establecimiento", 
  "total_locales_ubi",     "parr_ubi",                     "Variable R en el paper. Número de atenciones en la parroquia que se atendieron con recursos locales (Toales menos movilizados)", 
  "atraidos_fallecidos",   "parr_ubi",                     "Número de fallecidos en un establecimiento de una parroquia distinta de la residencia del paciente. Inmigrantes falleciods.", 
  "atraidos_genito",       "parr_ubi",                     "Número de egresos (inmigrantes) por enfermedades del sistema genitourinario en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "atraidos_partos",       "parr_ubi",                     "Número de egresos (inmigrantes) por embarazos, partos y puerperios en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "atraidos_perina",       "parr_ubi",                     "Número de egresos (inmigrantes) por ciertas afecciones originadas en el período perinatal en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "atraidos_compli",       "parr_ubi",                     "Número de egresos (inmigrantes) por complicaciones del embarazo, del trabajo de parto y del parto en un establecimiento de una parroquia distinta de la residencia del paciente",
  "total_expulsados",      "parr_res",                     "Variable E en el paper. Número de movilzados fue de la parroquia de residencia del paciente.",
  "total_locales_res",     "parr_res",                     "En teoria debería se igual a R. Número de personas que se atendieron en su parroquia de residencia (Todos los pacientes residentes menes movilizados)",
  "expulsados_fallecidos", "parr_res",                     "Número de fallecidos residentes de la parroquia que muerieron en una parroquia distinta a la parroquia de residencia.",
  "expulsados_genito",     "parr_res",                     "Número de residentes (emigrantes) que tuvieron un egreso por enfermedades del sistema genitourinario en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "expulsados_partos",     "parr_res",                     "Número de residentes (emigrantes) que tuvieron un egreso por embarazos, partos y puerperios en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "expulsados_perina",     "parr_res",                     "Número de residentes (emigrantes) que tuvieron un egreso por ciertas afecciones originadas en el período perinatal en un establecimiento de una parroquia distinta de la residencia del paciente", 
  "expulsados_compli",     "parr_res",                     "Número de residentes (emigrantes) que tuvieron un egreso por complicaciones del embarazo, del trabajo de parto y del parto en un establecimiento de una parroquia distinta de la residencia del paciente"
)


# Escritura en el pins ----------------------------------------------------

panel_expulsados <- panel_expulsados %>% 
  map2(.y = 2015:2019,~.x %>% mutate(anio = .y)) %>% 
  reduce(bind_rows)      


panel_atraidos <- panel_atraidos %>% 
  map2(.y = 2015:2019,~.x %>% mutate(anio = .y)) %>% 
  reduce(bind_rows)    

# Checkpoint --------------------------------------------------------------


pin_write(board = carpeta,x = panel_expulsados,name = "bdd_egresos_expulsados")

pin_write(board = carpeta,x = panel_atraidos,name = "bdd_egresos_atraidos")

pin_write(board = carpeta,x = diccionario,name = "diccionario_egresos")

write_rds(panel_egresos,"../nuevos_hospitales/egresos_temp.rds")


# calculo del indice ------------------------------------------------------

panel_expulsados <- pin_read(board = carpeta,name = "bdd_egresos_expulsados")

panel_atraidos <- pin_read(board = carpeta,name = "bdd_egresos_atraidos")

panel_egresos_parr <- full_join(panel_expulsados,
          panel_atraidos,
          by = c("parr_res" = "parr_ubi",
                 "anio"))

panel_egresos_parr <- panel_egresos_parr %>% 
  rename(egresos_res = egresos.x,egresos_ubi = egresos.y)

ids_first <- panel_egresos_parr %>% 
  select(anio,parr_res,
         matches("egresos"),
         matches("total")) %>% 
  mutate(
    across(.cols = c(matches("egresos"),
                     matches("total")),
           .fns = ~replace_na(.x,0)
    ),
    numerador = total_atraidos - total_expulsados,
    denominador = total_atraidos + total_locales_res,
    ids_index = case_when(
      numerador < 0 & denominador == 0 ~ -100,
      numerador == 0 & denominador == 0 ~ 0,
      numerador > 0 & denominador == 0 ~ 100,
      TRUE ~ (numerador/denominador)*100),
    ids_index_2 = case_when(
      numerador < 0 & egresos_res == 0 ~ -1,
      numerador == 0 & egresos_res == 0 ~ 0,
      numerador > 0 & egresos_res == 0 ~ 1,
      TRUE ~ (numerador/egresos_res)*100),
    
    ids_index = case_when(
      ids_index > 100  ~  101,
      ids_index < -100 ~ -101,
      TRUE ~ ids_index),
    ids_index_factor = case_when(
      ids_index > 100 ~ "Superior a 100",
      ids_index == 100 ~ "Exactamente 100",
      between(ids_index ,50,100) ~ "Entre 50 y 100",
      between(ids_index ,0 ,50) ~ "Entre 0 y 50",
      ids_index == 0 ~ "Exactamente 0",
      between(ids_index ,0 ,-50) ~ "Entre 0 y -50",
      ids_index == -100 ~ "Exactamente -100", # Primero queremos esta clasificación 
      between(ids_index ,-50,-100) ~ "Entre -50 y -100", #Luego esta
      ids_index == -101 ~ "Inferior a -100"
    ),
    ids_index_factor = factor(ids_index_factor,levels = c("Superior a 100",
                                                          "Exactamente 100",
                                                          "Entre 50 y 100",
                                                          "Entre 0 y 50",
                                                          "Exactamente 0",
                                                          "Entre 0 y -50",
                                                          "Entre -50 y -100",
                                                          "Exactamente -100", # Ojo con el orden
                                                          "Inferior a -100"))
  )




# 
data_ind <- ids_first %>%
  # filter(between(ids_index,-1,1)) %>%
  filter(ids_index>-100)


  ggplot(data = data_ind,
         aes(x = ids_index,
             group = anio)) +
  geom_histogram() +
  facet_grid(anio~ .)

ids_first %>% 
  ggplot() + 
  geom_bar(aes(x = anio,fill = ids_index_factor),position = position_stack())


list(panel_expulsados,
          panel_atraidos) %>% 
  map(names)

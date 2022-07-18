library(tidyverse)
library(rlang)
library(haven)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library("pins")
library("data.table")

carpeta <- board_folder("C:/Users/Esteban/OneDrive/RAR")
# Diccionario RAS
# diccionario_RAS <- fread("C:/Users/Esteban/Documents/alejandria_repo/codam_2022/Diccionario_RAS - Diccionario_RAS.csv") 

ras_data <-  pin_read(board = carpeta, name="ras_listas") 


diccionario_RAS <- pin_read(board = carpeta, name="diccionario_RAS") 

var_indicador <- diccionario_RAS %>% pull(var = 1) %>% str_to_lower()

vars <- c("secuencial", "prov_ubi", "cant_ubi", "parr_ubi", "clase", "entidad", "sector", "tipo", "area", "region")

ras_data <- ras_data %>% 
  map(~.x %>% select(one_of(c(vars, var_indicador)))) # Crear función anónima para pasar el select por cada elemento de la lista

# Matriz de equivalencia - variables disponibles en el tiempo

matriz <- map2(.x=ras_data, 
     .y=2015:2019,
     ~{
       year  <- .y
       tabla <- tibble(vars=names(.x)) %>% 
         mutate(ind=1) %>% 
         rename_with(.cols=ind, .fn = ~str_c(.x, year, sep = "_"))
     }) %>% 
  reduce(full_join)

# Identificar variables incompletas
matriz_incompletos <- matriz %>% 
  filter(if_any(.cols = everything(), is.na)) 

vars_incompletas <- matriz_incompletos %>%  pull(vars) 

ras_incompleto <- diccionario_RAS %>% 
  mutate(across(codigo_de_la_variable,str_to_upper)) %>% 
  filter(codigo_de_la_variable %in% str_to_upper(vars_incompletas)) %>% 
  inner_join(matriz_incompletos%>%
               mutate(across(vars,str_to_upper)), by=c("codigo_de_la_variable"="vars")) 

pin_write(carpeta, x=ras_incompleto, name="ras_incompleto")

# Matriz completa

ras_completo <- diccionario_RAS %>% 
  mutate(across(codigo_de_la_variable,str_to_upper)) %>% 
  filter(!codigo_de_la_variable %in% str_to_upper(vars_incompletas)) %>% 
  inner_join(matriz%>%
               mutate(across(vars,str_to_upper)), by=c("codigo_de_la_variable"="vars")) 

pin_write(carpeta, x=ras_completo, name="ras_completo")




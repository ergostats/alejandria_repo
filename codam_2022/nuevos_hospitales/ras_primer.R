library(pins)
library(tidyverse)
library(janitor)


path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

diccionario_ras <- pin_read(board = carpeta,name = "diccionario_RAS") 

diccionario_ras %>% 
  tibble() %>% 
  clean_names() %>% 
  separate(col = nombre_de_la_variable ,sep = ":",into = c("bloque","descripcion")) %>% 
  mutate(bloque = str_remove(bloque,pattern = "K.*"),
         across(.cols = c(bloque,descripcion),.fns = ~str_trim(.x,side = "both"))) %>% 
  # across(.cols = c(bloque,descripcion),.fns = function(loqueasea)str_trim(loqueasea,side = "both"))) %>%
  pin_write(board = carpeta,x = .,name = "diccionario_RAS")


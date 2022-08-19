library(pins)
library(tidyverse)

path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

mapa_parroquia <- read_rds("C:/Users/Alex/Documents/vital_strategies/6 Entrega final (3)/app_desnut/dpa/shapefiles_simplificados_dpa.rds")[[3]]

centroides <- read_rds("C:/Users/Alex/Documents/vital_strategies/6 Entrega final (3)/app_desnut/dpa/centroides_simp.rds")$parroquias

pin_write(carpeta,x = mapa_parroquia,"shape_parroquia")

pin_write(carpeta,x = centroides,"shape_parroquia_centroides")

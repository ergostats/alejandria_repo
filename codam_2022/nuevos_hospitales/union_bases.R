
# ------------------------------------------------------------------------- #
#            Creación de la base completa y primeras estadisticas           #
# ------------------------------------------------------------------------- #

library(tidyverse)
library(pins)
library(ggforce)


# Lectura -----------------------------------------------------------------

# Path para la carpeta {pins}

path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

# Bases de datos

bdd_establecimientos_clase <- pin_read(board = carpeta,
                                       name = "bdd_establecimientos_clase")

bdd_establecimientos_equipamiento <- pin_read(board = carpeta,
                                              name = "bdd_establecimientos_equipamiento" )

bdd_index_ids <- pin_read(board = carpeta,
                          name = "bdd_index_ids")



# Unión de las bases de datos ---------------------------------------------

# Calculo del total de establecmientos:

bdd_establecimientos_clase <- bdd_establecimientos_clase %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(total = sum(across(.cols = -c(dpa_parroq,anio)),na.rm = TRUE))
  
# Año base 2017:

estab_2017 <- bdd_establecimientos_clase %>% 
  group_by(dpa_parroq) %>% 
  filter(anio == 2017) %>% 
  select(dpa_parroq,base = total)

# Variación respecto al año base:

establecimientos <- left_join(bdd_establecimientos_clase,estab_2017) %>% 
  mutate(variacion = if_else(base != 0,total/base,1)) %>% 
  select(dpa_parroq,anio, total, variacion)

# Unión con la base del indicador:

tabla_indicador_hospitales <- bdd_index_ids %>% 
  select(dpa_parroq,anio,ids_index,ids_index_2) %>% 
  mutate(anio = as.character(anio)) %>% 
  left_join(establecimientos)



  

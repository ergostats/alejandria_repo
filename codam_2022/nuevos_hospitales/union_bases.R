
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
  mutate(total_unidades = sum(across(.cols = -c(dpa_parroq,anio)),na.rm = TRUE))
  
# Año base 2017:

estab_2017 <- bdd_establecimientos_clase %>% 
  group_by(dpa_parroq) %>% 
  filter(anio == 2017) %>% 
  select(dpa_parroq,base = total_unidades)

# Variación respecto al año base:

establecimientos <- left_join(bdd_establecimientos_clase,estab_2017) %>% 
  mutate(variacion = if_else(base != 0,total_unidades/base,1)) %>% 
  select(dpa_parroq,anio, total_unidades, variacion)

# Unión con la base del indicador:

tabla_indicador_hospitales <- bdd_index_ids %>% 
  select(dpa_parroq,anio,ids_index,ids_index_2) %>% 
  mutate(anio = as.character(anio)) %>% 
  left_join(establecimientos) 


# Primer histograma -------------------------------------------------------
# La tabla está agrupada por parroquias:

tabla_indicador_hospitales %>% 
  ungroup %>% 
  count(anio)

tabla_indicador_hospitales <- tabla_indicador_hospitales %>% 
  arrange(anio) %>% 
  mutate(
    total_unidades_lag = lag(total_unidades),
    variacion = case_when(
      is.na(total_unidades_lag) ~ NA_character_,
      total_unidades_lag > total_unidades ~ "disminuye",
      total_unidades_lag == total_unidades ~ "mantiene",
      total_unidades_lag < total_unidades ~ "incrementa"
    ))

pin_write(board = carpeta,
          x = tabla_indicador_hospitales,
          name = "tabla_indicador_hospitales",
          versioned = TRUE)


# Cálculo de las distancias -----------------------------------------------

pin_list(board = carpeta)

centroides <- pin_read(board = carpeta, 
                       name = "shape_parroquia_centroides",
                       version = "20220812T215126Z-21f85")

cabeceras <- centroides %>% 
  rename_with(str_to_lower) %>% 
  filter(str_detect(dpa_parroq, "0150$")) %>% 
  mutate(DPA_PROVIN = str_sub(dpa_parroq,1,2)) %>% 
  select(DPA_PROVIN,x_c = x,y_c = y)

tabla_indicador_hospitales %>% 
  ungroup() %>% 
  mutate(indicador = str_detect(dpa_parroq, "0150$")) %>% 
  group_by(indicador,anio) %>% 
  summarise(mean(total_unidades,na.rm = TRUE))

centroides <- centroides %>% 
  left_join(cabeceras)

centroides <- centroides %>% 
  mutate(distancia_cuadrado = (x - x_c)^2 + (y - y_c)^2,
         distancia = sqrt(distancia_cuadrado))


pin_write(board = carpeta,
          x = centroides,
          name =  "shape_parroquia_centroides",
          versioned = TRUE)


# ------------------------------------------------------------------------- #
#             m Creación de series de tiempo de migración 2022              #
# ------------------------------------------------------------------------- #

# install.packages("gganimate")


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(haven)
library(gganimate)


# Lectura de la base ------------------------------------------------------

esi <- read_dta("../migracion/esi_2021.dta")

# Revisamos la base

glimpse(esi)


# Transformaciones iniciales ----------------------------------------------

serie_migracion <- esi %>% 
  filter(mot_viam == 5,
         tip_naci == 1,  
         between(edad, left = 18, right = 65)) %>% 
  count(anio_movi,
        mes_movi,
        pais_prod)


# Cálculo de la migración acumulada ---------------------------------------


serie_migracion <- serie_migracion %>% 
  group_by(pais_prod) %>% 
  arrange(pais_prod,
          anio_movi,
          mes_movi) %>% 
  mutate(acum = cumsum(n)) %>% 
  ungroup()


# Llenar los meses que faltan para cada pais ------------------------------

meses <- serie_migracion %>% 
  distinct(mes_movi)


lista_migracion <- serie_migracion %>% 
  split(.$pais_prod)

# Llamar los elementos de la lista:
# lista_migracion$`233`
# lista_migracion[["233"]]


# Operación deseada por cada elemento de la lista -------------------------

lista_migracion[["233"]] %>% 
  right_join(meses)


# Uso del for -------------------------------------------------------------

ejemplo <- list()

for(i in names(lista_migracion)){
  
  ejemplo[[i]] <- lista_migracion[[i]] %>% 
    right_join(meses)
  
}
# Podemos mejorar esa sintaxis con map:

lista_migracion <- map(.x = lista_migracion,
    .f = ~ .x %>% 
      right_join(meses))

# Otra alternativa más explicita:

# map(.x = lista_migracion,
#     .f = function(df){
#       df %>% 
#         right_join(meses)
#     })


# Llenamos las variables en los meses posteriores a la primera movilidad --------


lista_migracion <- lista_migracion %>% 
  map(.f = ~ .x %>% 
        arrange(mes_movi) %>% 
        fill(acum, n, pais_prod, anio_movi,
             .direction = "down"))

# Reducimos todos los data frames a uno solo ------------------------------


migracion_mensual <- reduce(.x = lista_migracion,
                            .f = bind_rows)

# Saco el top 10 mensual:

top_10_mensual <- migracion_mensual %>% 
  group_by(mes_movi) %>% 
  top_n(acum,n = 10) %>% 
  arrange(mes_movi,desc(acum)) 


# Escribo la data para la animacion ---------------------------------------


write_tsv(x = top_10_mensual,
          file = "post/01062023_top10_paises_migracion/top_10_mensual.txt")

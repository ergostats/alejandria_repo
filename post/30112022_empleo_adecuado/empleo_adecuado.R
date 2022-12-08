
# Instalacion del paquete webshot2  ---------------------------------------
#toma screenshots de paginas web

install.packages("webshot2")


# Instalación de Librerías ------------------------------------------------

library(haven)
library(tidyverse)
library(srvyr)
library(scales)
library(gt)
library(gtExtras)


# Lectura de la base de datos ---------------------------------------------

# 1. Crear un proyecto con la base de datos
# 2. Guardar la base de datos comprimida en una carpeta data/ dentro del proyecto
# 3. Con la carpeta data/ simplificamos la ruta:

enemdu_persona_2021 <- read_sav("data/enemdu_persona_2021_12.sav")

# 4. A la hora de hacer `git add` evitar subir la carpeta data/

# Diseño muestral ---------------------------------------------------------

tabla_svy <-  enemdu_persona_2021 %>%  
  as_survey_design(ids = upm, 
                   strata = estrato,
                   weights = fexp)



# Creación de Variables ---------------------------------------------------

# 1, La PEA es la variable condact desde el 1 hasta el 8,
# si se usar condact == 1 | condact == 8 estamos quedandonos 
# con el empleo adecuado o el desempleo no remunerado
# Si la sintaxis del INEC es complicada preguntar a Alex.

# 2. Usar numeros en las variables u objetos cuando se traten de versiones 
# distintas de una misma variable u objeto, en esta caso son dos variables u
# oibjetos distintos

# Creación de variables dicotomicas para:
# pea: población económicamente activa
# pea_adecuado: población económicamente activa con empleo adecuado

tabla_svy <- tabla_svy %>% 
  mutate(pea = if_else(condact %in% 1:8,
                        true = 1,
                        false = 0),
         pea_adecuado = if_else(condact == 1, 
                                 true = 1, 
                                 false = 0))



# Creacion de tablas ------------------------------------------------------

# Población económicamente activa por nivel de instrucción

tabla_pea <- tabla_svy %>% 
  group_by(p10a) %>% 
  summarise(pea = survey_total(pea))

# Población con empleo adecuado por nivel de instrucción

tabla_pea_adecuado <- tabla_svy %>% 
  group_by(p10a) %>% 
  summarise(pea_adecuado = survey_total(pea_adecuado))


# Unión de las tablas -----------------------------------------------------

tabla_union <- inner_join (tabla_pea, 
                           tabla_pea_adecuado, 
                           by = "p10a")  

# No es posible usar svy_ratio por que ya los valores de 
# `pea` y `pea_adecuado` ya están expandidos por ende solo
# nos resta hacer la división entre ambos valores:



# Creación del primer indicador de la PEA ---------------------------------

#El ratio muestra la proporción de la población económicamente activa con empleo 
#adecuado sobre la población económica mente activa segun su nivel de instrucción

resultado <- tabla_union %>% 
  mutate(ratio = pea_adecuado/pea) %>% 

  
  

# Editando la tabla resultado ---------------------------------------------


#Se excluye las variables que tienen información sobre la desviacion estandar("_se")
#Renombrar la variable p10a a niv_inst (nivel de instrucción)
#Se eliminan los valores vacios de la variable niv_inst
#Se trata como factor a la variable niv_inst

  select(p10a, pea, pea_adecuado, ratio) %>% 
  rename(niv_inst = p10a) %>% 
  filter(!is.na(niv_inst)) %>% 
  mutate(niv_inst = as_factor(niv_inst))




# Edición de la tabla resultante ------------------------------------------

#Utilización de algunas funciones del paquete "gt" para mejorar la visualizacion de la tabla

tabla_impresion <- resultado %>% 
  gt() %>% 
  
  #renombrar a las columnas de la tabla
  
  cols_label(niv_inst = "Nivel instrucción", 
             pea = "PEA", 
             pea_adecuado = "PEA con empleo adecuado", 
             ratio = "Proporción de empleo adecuado") %>% 
  
  #fmt_number permite mejorar la presentacion de cifras y colocar como separador de miles
  # un espacio vacio
  
  fmt_number(columns = c(pea, pea_adecuado), 
             decimals = 0, 
             use_seps = TRUE,
             sep_mark = " ") %>% 
  
  # fmt_percent toma los valores y los multiplicaa por 100 y lo pone en formato porcentaje
  
  fmt_percent(columns = ratio, 
              decimals = 2) %>% 
  
  # colocación de un titulo y subtitulo a la tabla 
  
  tab_header(
    title = md("Empleo Adecuado"),
    subtitle = md("Considerando el nivel de instrucción de la Población Económicamente Activa ")) %>%
  
  #colocación de un pie de página para la columna proporcion de empleo adecuado
  
  tab_footnote(
    locations = cells_column_labels(
      columns = ratio 
    ),
    footnote = "La proporcion dentro de cada nivel de instrucción que tiene un empleo adecuado" )



# Temas diferentes de presentacion para la Tabla impresión ----------------

tabla_impresion_1 <- tabla_impresion %>% 
  gt_theme_guardian()
  
gtsave(data = tabla_impresion_1,
       filename = "post/30112022_empleo_adecuado/tabla_impresion_1.png")


tabla_impresion_2 <- tabla_impresion %>% 
  gt_theme_excel()

gtsave(data = tabla_impresion_2,
       filename = "post/30112022_empleo_adecuado/tabla_impresion_2.png")


tabla_impresion_3 <- tabla_impresion %>% 
  gt_theme_nytimes()

gtsave(data = tabla_impresion_3,
       filename = "post/30112022_empleo_adecuado/tabla_impresion_3.png")



################################################################################
# Tarea específica del post -----------------------------------------------

#1.- Cuál es el nivel de instrucción con mayor empleo adecuado?
#R: Postgrado (86.17%)

#2.- Cuál es el nivel con menor empleo adecuado? 
#R: Centro de Alfabetización (1.52%)
################################################################################



# Indicador 2 -------------------------------------------------------------

# Muestra el porcentaje de la población económicamente activa con empleo adecuado 
#sobre el total de la población economicamente activa.



tabla_pea_total <- tabla_svy %>% 
  group_by(p10a) %>% 
  summarise(pea = sum(pea, na.rm = T),
            pea_adecuado = sum(pea_adecuado, na.rm = T))


tabla_pea_total %>% 
  ungroup() %>% 
  summarise(pea = sum(pea, na.rm = T),
            pea_adecuado = sum(pea_adecuado, na.rm = T),
            pea_total = sum(pea + pea_adecuado)) %>% 
  mutate(indicador = pea_adecuado/pea_total)


##################################################################################################
#En sintesis, la proporción total de la PEA con empleo adecuado sobre la PEA total es del 27.8%

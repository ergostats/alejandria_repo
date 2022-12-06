

# Librerías ---------------------------------------------------------------

library(haven)
library(tidyverse)
library(srvyr)


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

resultado <- tabla_union %>% 
  mutate(ratio = (pea_adecuado/pea)*100)

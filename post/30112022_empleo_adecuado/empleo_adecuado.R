

# Declaracion de las Librerías --------------------------------------------

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


# Diseño muestral de la base de datos  ------------------------------------

svy <-  enemdu_persona_2021 %>%  
  as_survey_design(ids = upm, 
                   strata = estrato,
                   weights = fexp)



# Creación de Variables pea y pea adecuado --------------------------------

base1 <- svy %>% 
  mutate(pea1 = if_else(condact == 1 | condact == 8, true = 1, false = 0),
         pea_adecuado1 = if_else(condact == 1, true = 1, false = 0),
         proporcion = pea_adecuado1/pea1)



# Creacion de tablas para las variables pea y pea adecuado ----------------

pea <- base1 %>% 
  group_by(p10a) %>% 
  summarise(pea1 = survey_total(pea1))

pea_adecuado <- base1 %>% 
  group_by(p10a) %>% 
  summarise(pea_adecuado1 = survey_total(pea_adecuado1))


# Unión de las tablas anteriores en una sola tabla ------------------------

inner_join (pea, pea_adecuado, by = "p10a")  


survey::svyratio(~pea_adecuado1, ~pea1, base1)                                          

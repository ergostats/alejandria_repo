

# Librerías ---------------------------------------------------------------

library(haven)
library(magrittr)
library(tidyverse)
library(srvyr)


# Lectura de la base de datos ---------------------------------------------

enemdu_persona_2021 <- read_sav("C:/Users/andre/Downloads/1_BDD_ENEMDU_2021_12_SPSS/enemdu_persona_2021_12.sav")


# Diseño muestral ---------------------------------------------------------

svy <-  enemdu_persona_2021 %>%  
  as_survey_design(ids = upm, 
                   strata = estrato,
                   weights = fexp)



# Creación de Variables ---------------------------------------------------

base1 <- svy %>% 
  mutate(pea1 = if_else(condact == 1 | condact == 8, true = 1, false = 0),
         pea_adecuado1 = if_else(condact == 1, true = 1, false = 0),
         proporcion = pea_adecuado1/pea1)



# Creacion de tablas ------------------------------------------------------

pea <- base1 %>% 
  group_by(p10a) %>% 
  summarise(pea1 = survey_total(pea1))

pea_adecuado <- base1 %>% 
  group_by(p10a) %>% 
  summarise(pea_adecuado1 = survey_total(pea_adecuado1))


# Unión de las tablas -----------------------------------------------------

inner_join (pea, pea_adecuado, by = "p10a")  


survey::svyratio(~pea_adecuado1, ~pea1, base1)                                          


# Librerias ---------------------------------------------------------------

install.packages("tidyverse")

# Librerías ---------------------------------------------------------------

library(haven)
library(magrittr)
library(tidyverse)
library(srvyr)


# Lectura de la base de datos ---------------------------------------------

multibdd_salud <- read_sav("201912_multibdd_salud.sav")


# Diseño muestral ---------------------------------------------------------

svy <-  multibdd_salud %>%  
  as_survey_design(ids = upm, 
                   strata = estrato5,
                   weights = fexp5)


# Descripción de las nuevas variables a crear--------------------------------------

#caso 1 –> Hogares donde alguno de sus miembros tuvo alguna enfermedad o necesidad de atención de su salud y recibió atención.
#caso 2 -> Hogares donde alguno de sus miembros recibió tuvo una consulta de control
#proc_instal -> Trato en el establecimiento de salud (1: si, 2: no)
#servicio -> Calificación del servicio de salud
#espera -> tiempo de espera en horas y minutos
#x2 -> numerador del ratio
#y2 -> denominador del ratio




# Creación de nuevas variables --------------------------------------------

base1 <- svy %>% 
  mutate(caso1 = if_else(s102p1 == 1 & s102p2 <= 2, true = 1, false = 0 ),
         caso2 = if_else((s102p1 == 1 & s102p2 >= 3 & s102p4 == 1) | (s102p1 == 2 & s102p4 == 1), true = 1, false = 0),
         proc_instal = if_else((s102p91 == 1 & s102p92 == 1 & s102p93 == 1 & s102p94 == 1 & s102p95 == 1), true = 1, false = 0),
         servicio = if_else(s102p10 <= 2, true = 1, false = 0),
         espera = if_else(s102p6a == 0 & s102p6b <= 30, true = 1, false = 0),
         x2 = if_else((caso1 == 1 | caso2 == 1) & s102p5 <= 5 & proc_instal == 1  & servicio == 1 & espera == 1, true = 1, false = 0),
         y2 = if_else((caso1 == 1 | caso2 == 1) & s102p5 <= 5, true = 1, false = 0),
         ratio = x2/y2
  )


# Construcción del indicador atención en el centro de salud---------------------

base1 %>% 
  group_by(area) %>% 
  summarise(ratio = survey_mean(ratio))

survey::svyratio(~x2, ~y2, base1)                                          

```

<!-- ::: -->
  
  <!-- ::: {} -->
  
  
  El ratio calculado comprende: 
  
  X2:ser atendido por enfermedad, accidente, dolor, etc.  y de ser curado en un centro de salud público o con medicina ancestral, además de recibir una atención respetuosa, a tiempo, con una explicación clara del diagnostico con buenas instalaciones, que brinde consejos para el cuidado de la salud y un tiempo de espera menor a 30 minutos lo que implicaría un buen servicio de salud; 

Y2:ser atendido en un centro de salud publico por accidente, dolor, etc.

Entonces, la probabilidad de que suceda el X2 frente a Y2 es de 38. 83%

Lo que implicaría una deficiencia en la atención en un centro de salud pública.


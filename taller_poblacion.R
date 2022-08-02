#install.packages("tidyverse")
#install.packages('lubridate')
#install.packages("nycflights13")

library(tidyverse)
library(readxl)
library(lubridate)
library(nycflights13)


dataset <- read_excel("202112_Tabulados_Mercado_Laboral_EXCEL.xlsx", sheet = "1. Poblaciones", range = "A3:H939")
dataset = rename(dataset, c(Encuesta="...1", Periodo="...2", Indicadores="...3", Nacional="Total" ))

Base = select(dataset, Periodo, Indicadores, Hombre, Mujer)
Base = pivot_longer(Base, c("Hombre", "Mujer"), names_to = "Sexo",values_to = "Total"  )

#formato que tiene el campo periodo(character)
#m-y      pero diciembre esta en español--->
Base <- Base %>%
  mutate(Periodo = my(Periodo))
head(Base)
#Filtrar por Empleo Adecuado/Pleno
Base = subset(Base, Indicadores == "Empleo Adecuado/Pleno")

#Dos ejes y
# el segundo eje tiene un rango menor a el primero

ggplot(Base, aes(Periodo, Total)) +
  geom_point() +
  scale_y_continuous(
    "Hombre", 
    sec.axis = sec_axis(~ . * 10, name="Mujer")
  )


  
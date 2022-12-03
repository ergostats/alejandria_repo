
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)


# Análisis de datos -------------------------------------------------------

años <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
salario_basico_unificado <- c(264, 292, 318, 340, 354, 366, 375, 386, 394, 400, 400, 425)
tasa_variacion <- c(0.1000, 0.1060, 0.0890, 0.0692, 0.0412, 0.0339, 0.0244, 0.0293, 0.0207, 0.0152, 0, 0.0625)

salary <- data.frame(años, salario_basico_unificado, tasa_variacion)


# Gráfico -----------------------------------------------------------------

plotsalary <- salary %>% 
  ggplot() +
  geom_line(aes(x = años,
                y = tasa_variacion)) +
  #geom_text(aes(label = tasa_variacion))+
  labs(title = "Salario Básico",
       subtitle = "Se presentan las tasas de variación",
       caption = "Fuente: Consejo Nacional de Trabajo y Salario | Elaboración: Alex Bajaña",
  )



# Guardar el gráfico ------------------------------------------------------

ggsave(plot = plotsalary,filename = "post_sal_1.png",
       width = 	8*4496/2400,height = 4*4496/2400) # Dimensiones fijas

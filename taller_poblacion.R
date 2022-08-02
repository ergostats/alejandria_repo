# ------------------------------------------------------------------------- #
#            Análisis del empleo pleno por genero en el Ecuador             #
# ------------------------------------------------------------------------- #
# Autor: Andrea Sanchez
# Revisión: Alex Bajaña

# Librerias ---------------------------------------------------------------



#install.packages("tidyverse")
#install.packages('lubridate')
# library(scales)

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)


# Lectura de la tabla de mercado laboral por sexo -------------------------

dataset <- read_excel(path = "202112_Tabulados_Mercado_Laboral_EXCEL.xlsx", 
                      sheet = "1. Poblaciones", 
                      range = "A3:H939")

# Renombrar las columnas de la tabla --------------------------------------

dataset <- rename(dataset, c(Encuesta = "...1", 
                             Periodo = "...2", 
                             Indicadores = "...3", 
                             Nacional = "Total" ))

# Una alternativa para no usar textos

# dataset <- rename(dataset, Encuesta =  `...1`, 
#                              Periodo =   `...2`, 
#                              Indicadores =   `...3`, 
#                              Nacional = Total )


# Reducir la base, con las columnas de interes ----------------------------

base_reduced <- select(dataset, Periodo, Indicadores, Hombre, Mujer)

# Refortear la tabla para el análisis -------------------------------------

base_reduced <-  pivot_longer(data = base_reduced,
                              cols =  c("Hombre", "Mujer"), 
                              names_to = "Sexo",values_to = "Total"  )


# Revisión de las fechas --------------------------------------------------

#formato que tiene el campo periodo(character)
#m-y      pero diciembre esta en español--->

base_reduced <- mutate(base_reduced,Periodo = my(Periodo))

base_reduced %>% 
  count(Periodo) 

#Filtrar por Empleo Adecuado/Pleno

base_reduced <-
  base_reduced %>% 
  mutate(mes = month(Periodo)) %>% 
  filter(Indicadores == "Empleo Adecuado/Pleno",
         mes == 12) 



# Reescalar para mujeres --------------------------------------------------

base_reduced <-
  base_reduced %>% 
  mutate(transformado = if_else(condition = Sexo == "Mujer",
                                true = Total*2.2,
                                false = Total),
         transformado = transformado/1000)

#Dos ejes y
# el segundo eje tiene un rango menor a el primero

grafico <- ggplot(base_reduced, aes(x = Periodo,
                         y = transformado,
                         color = Sexo)) +
  geom_point() +
  geom_line() +
  scale_color_manual(values = c("#C70039","#0036A6")) +
  scale_y_continuous(
    name = "Miles de hombres",
    labels = number,
    sec.axis = sec_axis(~ . / 2.2, 
                        name=" Miles de mujeres",
                        labels = number)
  ) +
  labs(title = "Empleo pleno por sexo",
       subtitle = "Se presentan en los ejes miles de personas",
       caption = "Elaborado por: Andrea Sanchez, Alex Bajaña") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y.left = element_text(vjust = 4,colour = "#C70039"),
        axis.title.y.right = element_text(vjust = 4,colour = "#0036A6"),
        legend.position = "bottom")

# Tamaño de letra para impresion
# base_size = 16

# Margenes
# margenes t = 0.25,r = 2.25,b = 0.25,l = 2.25, "cm"

# Impresion del gráfico con ggplot 

# ancho 8*4496/2400
# altura  4*4496/2400

  
# ------------------------------------------------------------------------- #
#            Análisis del empleo pleno por genero en el Ecuador             #
# ------------------------------------------------------------------------- #
# Autor: Andrea Sanchez
# Revisión: Alex Bajaña

# Librerias ---------------------------------------------------------------



#install.packages("tidyverse")
# install.packages('lubridate')
# install.packages("scales")

library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(ggrepel)


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
         transformado = transformado/1000,
         label = round(Total/1000,0),
         label = number(label),
         label = if_else(Periodo == "2021-12-01",label,NA_character_))

#Dos ejes y
# el segundo eje tiene un rango menor a el primero

grafico <- ggplot(base_reduced, aes(x = Periodo,
                         y = transformado,
                         color = Sexo)) +
  geom_point(size = 2,alpha = 0.6) +
  geom_line(size = 1,alpha = 0.6) +
  geom_text_repel(aes(label = label),size = 7,show.legend = FALSE) +
  scale_color_manual(values = c("#C70039","#0036A6")) +
  scale_y_continuous(
    name = "Miles de hombres",
    labels = number,
    sec.axis = sec_axis(~ . / 2.2, 
                        name=" Miles de mujeres",
                        labels = number)
  ) +
  labs(title = "Número de ecuatorianos en condición de pleno empleo por sexo",
       subtitle = "En los ejes se presentan miles de personas",
       caption = "Fuente: INEC | Elaborado por: Andrea Sanchez, Alex Bajaña") +
  theme_minimal(base_size = 16) +
  theme(axis.title.x = element_blank(),
        axis.title.y.left = element_text(vjust = 4,colour = "#C70039"),
        axis.title.y.right = element_text(vjust = 4,colour = "#0036A6"),
        legend.position = "bottom",
        plot.margin = margin(t = 0.25,r = 2.25,b = 0.25,l = 2.25, "cm"),
        plot.background = element_rect(fill = "white",colour = "white")
        )


grafico

# Tamaño de letra para impresion
# base_size = 16

# Margenes
# margenes t = 0.25,r = 2.25,b = 0.25,l = 2.25, "cm"

# Impresion del gráfico con ggplot 

# ancho 8*4496/2400
# altura  4*4496/2400

  ggsave("pleno_empleo_sexo.png", plot = grafico, width = 8*4496/2400, height = 4*4496/2400 )
  
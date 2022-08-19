# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rlang)
library(haven)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library("pins")
library("data.table")
library(gganimate)

# install.packages("remotes")
# remotes::install_github("thomasp85/transformr")
# library("transformr")

Esteban> carpeta <- board_folder("C:/Users/Esteban/OneDrive/RAR")
# carpeta <- board_folder("C:/Users/Alex/OneDrive/Documentos/RAR/") # Alex >

establecimientos_clase <- pin_read(carpeta, "bdd_establecimientos_clase")


## Egresos
egresos <- pin_read(carpeta, name = "ehh_res_1") 
atraidos <- pin_read(carpeta, name = "bdd_egresos_atraidos") 



# Indicadores Modelo
bdd_index_ids <- pin_read(carpeta, name = "bdd_index_ids")
shape_parroquia <- pin_read(carpeta, name = "shape_parroquia")
shape_centroides<- pin_read(carpeta, name = "shape_parroquia_centroides")


bdd_index_ids_19 <- bdd_index_ids %>% filter(anio==2019)


## Mapa
graph_mapa <- function(tabla,fill_var,dpa,centroides,year,shape){
  
  mapa_plot <- tabla %>% 
    split(.[[year]]) %>% 
    map(~{
      
      shape %>% 
        left_join(.x,by = c("DPA_PARROQ" = dpa)) %>% 
        fill({{year}},.direction = "downup")
      
    }) %>%
    keep(~length(.x)!= 0) %>% 
    reduce(bind_rows) %>% 
    filter(!is.na(anio))
  
  # browser()
  
  plot <- ggplot(data = mapa_plot) +
    geom_polygon(aes_string(x = "long",
                            y = "lat",
                            group = "group",
                            fill = fill_var),
                 color = "white",size = 0.5)+
    # geom_text(data = shape_centroides,
    #           aes(x = x, y = y,label = DPA_PARROQ),size = 2,
    #           color = "gray") +
    # scale_fill_viridis_c(option = "inferno")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(plot)
  
}

pichincha_index <- bdd_index_ids %>% 
  filter(str_detect(dpa_parroq,"^1701"))

# 
EC <- graph_mapa(tabla = bdd_index_ids ,
                      fill_var = "ids_index",
                      dpa = "dpa_parroq",
                      centroides = shape_centroides,
                      year = "anio",shape = shape_parroquia) +
  scale_fill_viridis_c(option = "inferno")+
  facet_grid(rows = vars(anio)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "b) Egresos hospitalarios",
       subtitle = "Logaritmo del número total de egresos")
EC




PICH <- graph_mapa(tabla = pichincha_index ,
                 fill_var = "ids_index",
                 dpa = "dpa_parroq",
                 centroides = shape_centroides,
                 year = "anio",
                 shape = shape_parroquia %>%   
                   filter(str_detect(DPA_PARROQ,"^1701"))) +
  scale_fill_viridis_c(option = "inferno")+
  facet_grid(rows = vars(anio)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "b) Egresos hospitalarios",
       subtitle = "Logaritmo del número total de egresos")
PICH


guayas_index <- bdd_index_ids %>% 
  filter(str_detect(dpa_parroq,"^09"))

pin_write(carpeta, guayas_index)

GUAY <- graph_mapa(tabla = guayas_index ,
                   fill_var = "ids_index",
                   dpa = "dpa_parroq",
                   centroides = shape_centroides,
                   year = "anio",
                   shape = shape_parroquia %>%   
                     filter(str_detect(DPA_PARROQ,"^09"))) +
  scale_fill_viridis_c(option = "inferno")+
  # facet_grid(rows = vars(anio)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "b) Egresos hospitalarios - Año: {frame_time}'",
       subtitle = "Logaritmo del número total de egresos")+
  transition_states(anio,
                    transition_length = 1,
                    state_length = 1)

GUAY


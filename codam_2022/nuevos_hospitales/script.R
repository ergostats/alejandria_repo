# ------------------------------------------------------------------------- #
#   Panel de egresos hospitalarios con movilidad por clasificacion general  #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rlang)
library(haven)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library(pins)

path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

# Archivo plano:

# archivos_egr <- list.files(path = str_c(path_temporal,"nuevos_hospitales/egresos/"),full.names = T)
# 
# archivos_ras <- list.files(path = str_c(path_temporal,"nuevos_hospitales/ras/"),full.names = T)

archivos_egr <- list.files(path = str_c("../nuevos_hospitales/egresos/"),full.names = T)

archivos_ras <- list.files(path = str_c("../nuevos_hospitales/ras/"),full.names = T)


# Funciones auxiliares ----------------------------------------------------


read_funciton <- function(file){
  
  if(str_detect(file,".sav$|.SAV$")){
    tabla <- read_spss(file) 
  }else{
    tabla <- read_csv2(file)
  }
  
  return(tabla)
  
  glimpse(tabla)
}



tasas_mov_mor <- function(panel,group_var){
  
  tabla_1 <- panel %>% 
    group_by_at(group_var) %>% 
    summarise(across(c(numero_movilizados,
                       numero_fallecidos,
                       egresos),sum)) %>% 
    ungroup() %>% 
    mutate(tasa_movilidad = numero_movilizados/egresos,
           tasa_morbilidad = numero_fallecidos/egresos)
  
  return(tabla_1)
  
}

graph_mapa <- function(tabla,fill_var,dpa,centroides,year){
  
  
  mapa_plot <- tabla %>% 
    split(.[[year]]) %>% 
    map(~{
      
      mapa_pichincha %>% 
        left_join(.x,by = c("DPA_PARROQ" = dpa)) %>% 
        fill({{year}},.direction = "downup")
                 
    }) %>%
    keep(~length(.x)!= 0) %>% 
    reduce(bind_rows) %>% 
    filter(!is.na(anio_egr))
  
  # browser()
  
  
  plot <- ggplot(data = mapa_plot) +
    geom_polygon(aes_string(x = "long",
                            y = "lat",
                            group = "group",
                            fill = fill_var),
                 color = "white",size = 0.5)+
    geom_text(data = centroides,
              aes(x = x, y = y,label = DPA_PARROQ),size = 2,
    color = "gray") +
    # scale_fill_viridis_c(option = "inferno")+
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(plot)
  
}

# Lectura de los datos ----------------------------------------------------

# Egresos hospitalarios: SAV para mantener las etiquetas

egresos <- map(archivos_egr,read_funciton)

ras <- map(archivos_ras,read_funciton)
# 
#   pin_write(board = carpeta,x = egresos %>% map(slice,1:10),name = "egresos_listas")
# 
# pin_write(board = carpeta,x = ras %>% map(slice,1:10),name = "ras_listas")

# Shapefile para los mapas:

mapa_parroquia <- read_rds("../ergostats_analysis/general/mapas/shapefiles_simplificados_dpa.rds")[[3]]

centroides <- read_rds("../ergostats_analysis/general/mapas/centroides_simp.rds")$parroquias

# Generación tabla establecimientos ---------------------------------------------------------------

panel_egresos <- egresos %>%
  map(mutate,
      movilidad = 1 - as.numeric(parr_ubi == parr_res),
      fallecido = as.numeric(con_egrpa %in% c(2,3))) %>%
  map(group_by,parr_ubi,area_ubi,                                 # Ubicación de los establecimientos de salud
               clase ,tipo,entidad,sector,parr_res,area_res,      # Características del establecimiento
               sexo,edad,cod_edad,anio_egr,                       # Características del paciente
               esp_egrpa) %>%                                     # Condiciones del paciente
  map(
    summarise,
    numero_movilizados = sum(movilidad),
    numero_fallecidos = sum(fallecido),
    egresos = n()
  ) 



panel_ras <- ras %>%
  map_at(2,mutate,parr_ubi = str_c(prov_ubi,cant_ubi,parr_ubi)) %>% 
  map(select,parr_ubi, clase ,tipo,sector,           # Ubicación de los establecimientos de salud
      k505:k508,k549:k551,                           # Consultas de morbilidad
      k580,k603,                                     # Consultas de morbilidad ambulatoria
      k722:k725,k890:k893                            # Consultas prevención
      ) %>% 
  map(~{
    list(
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k505:k508),
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k549:k551),
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k580),
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k603),
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k722:k725),
      .x %>% 
        select(parr_ubi, clase ,tipo,entidad,sector,k890:k893)
    ) %>% 
      map2(.x = .,
           .y= c("morbilidad_mujeres",
                 "morbilidad_hombres",
                 "morb_ambul_mujeres",
                 "morb_ambul_hombres",
                 "primer_preventivas",
                 "subsec_preventivas"),
           ~ names(.x) %>% 
             str_subset("^k[:digit:]{3}") %>% 
             str_c(collapse =  ",") %>% 
             str_c("consultas_",.y," = sum(",.,",na.rm = T)") 
      ) %>% 
      unlist %>% 
      str_c(collapse = ",") %>% 
      str_c(".x %>% rowwise %>% mutate(",.,")")  %>%
      parse_expr(.) %>%
      eval()  %>%
      select(parr_ubi, clase ,tipo,entidad,sector,
             consultas_morbilidad_mujeres,
             consultas_morbilidad_hombres,
             consultas_morb_ambul_mujeres,
             consultas_morb_ambul_hombres,
             consultas_primer_preventivas,
             consultas_subsec_preventivas) %>%
      rowwise() %>%
      mutate(total_consultas = sum(consultas_morbilidad_mujeres,
                                   consultas_morbilidad_hombres,
                                   consultas_morb_ambul_mujeres,
                                   consultas_morb_ambul_hombres,
                                   consultas_primer_preventivas,
                                   consultas_subsec_preventivas,na.rm = T))
    
    
  }) %>%
  map2(.y = 2015:2019,~.x %>% mutate(anio_egr = .y,   # Para hacer el merge con egresos
                                     across(c(clase,tipo,entidad,sector),as.character))) %>%
  reduce(bind_rows)

# Mapa para Pichincha:


mapa_pichincha <- mapa_parroquia %>% 
  filter(str_detect(DPA_PARROQ,"^1701"))
 
centroides_pichincha <- centroides %>% 
  filter(str_detect(DPA_PARROQ,"^1701"))


# Cambiar aqui la ruta  a la versión personal de la carpeta RAR:

# write_rds(x = panel_egresos,file = "C:/Users/Alex/OneDrive/Documentos/RAR/egresos_totales.rds")

# write_rds(x = panel_ras,file = "C:/Users/Alex/OneDrive/Documentos/RAR/ras_consul_totales.rds")

# Lectura cuando ya ha sido creada la tabla a nivel de establecimiento:

panel_egresos <- read_rds(file = "C:/Users/Alex/OneDrive/Documentos/RAR/egresos_totales.rds")

panel_ras <- read_rds(file = "C:/Users/Alex/OneDrive/Documentos/RAR/egresos_totales.rds")



# Gráfico 1: número de movilizaciones por unidad de salud -------------------

# Población: lugar de residencia de la persona en los limites de Pichincha:

pichincha_egresos <- tasas_mov_mor(panel = panel_egresos,
                           group_var = c("parr_res","anio_egr")) %>% 
  filter(str_detect(parr_res,"^1701")) 

panel <- panel_egresos %>% 
  ungroup() %>% 
  mutate(across(c(parr_ubi,clase,tipo,entidad,sector),as.character)) %>% 
  group_by(anio_egr,parr_ubi,clase,tipo,entidad,sector) %>% 
  summarise(across(c(numero_movilizados,numero_fallecidos ,egresos),sum,na.rm = T)) %>% 
  ungroup() %>% 
  full_join(panel_ras)

diccionario_parroquias <- attr(pichincha$parr_res,"labels") %>% 
  enframe(name = "parr_res_des",value = "parr_res")

diccionario_pichincha <- diccionario_parroquias %>% 
  filter(str_detect(parr_res,"^1701")) 


pichincha <- pichincha_egresos %>% 
  left_join(diccionario_parroquias)

pichincha <-pichincha %>% 
  mutate(diff = ((tasa_movilidad/lag(tasa_movilidad)) - 1)) %>% 
  ungroup() %>% 
  filter(!is.na(diff)) %>% 
  mutate(diff = if_else(diff>1,1,diff)) %>% 
  filter(anio_egr > 2016)  


parte_1 <- graph_mapa(tabla = pichincha,
           fill_var = "diff",
           dpa = "parr_res",
           centroides = centroides_pichincha,
           year = "anio_egr") +
  scale_fill_viridis_c(option = "inferno",labels = percent)+
  facet_grid(rows = vars(anio_egr)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "a) Tasa de movilidad",
       subtitle = "Número de movilizados sobre total de egresos")

pichincha_estab <- panel %>% 
                      group_by( parr_ubi,anio_egr)  %>% 
  summarise(across(c(egresos,total_consultas),sum,na.rm = T)) %>% 
  filter(str_detect(parr_ubi,"^1701"),
         anio_egr >= 2016) %>% 
  rowwise %>% 
  mutate(total_demanda = sum(egresos,total_consultas,na.rm = T))

pichincha_estab <- pichincha_estab %>% 
  group_by(parr_ubi) %>%
  arrange(anio_egr) %>% 
  mutate(diff = ((total_demanda/lag(total_demanda)) - 1)) %>% 
  ungroup() %>% 
  filter(!is.na(diff)) %>% 
  mutate(diff = if_else(diff>1,1,diff))

parte_2 <- graph_mapa(tabla = pichincha_estab ,
                      fill_var = "diff",
                      dpa = "parr_ubi",
                      centroides = centroides_pichincha,
                      year = "anio_egr") +
  scale_fill_viridis_c(option = "inferno",labels = percent)+
  facet_grid(rows = vars(anio_egr)) +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "b) Egresos hospitalarios",
       subtitle = "Logaritmo del número total de egresos")
  


figura_1 <- (parte_1 + parte_2) + 
  plot_annotation(title = "Figura 1. Movilidad por servicios de salud en la provincia de Guayas")


ggsave(figura_1,filename = "nuevos_hospitales/guayas_1.png",dpi = 300,height = 10,width = 7)

# pichincha %>% 
#   filter(area_res == 1) %>% 
# graph_mapa(tabla = .,fill_var = "tasa_movilidad","parr_res")
# 
# pichincha_area <- tasas_mov_mor(c("parr_ubi","area_ubi")) %>% 
#   split(.$area_ubi)
# 

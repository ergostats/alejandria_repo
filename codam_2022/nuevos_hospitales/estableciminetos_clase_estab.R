
# Homologación de clasificación de los establecimientos -------------------

# long_stab <- parr_clase_estab %>% 
#   pivot_longer(cols = -c(1:2),names_to = "clase",values_to = "establecimientos")
# 
# 
# 
#   
# matches <- count(long_stab,clase) %>% 
#   stringdist_full_join(dicc_clase,
#                        method = c("osa"
#                                   # , "lv", "dl", "hamming", "lcs", "qgram", "cosine", "jaccard", "jw","soundex"
#                                   ),
#                        by = c("clase" = "descripcion")) 
# El match fue un poco manual, y realizado con DataEditR


# Clasficación de establecimientos de salud -------------------------------


estab_homol <- read_excel("codam_2022/bases_rds/diccionario_homologado_establecimientos.xlsx",
                          sheet = "diccionario_homologado_establec") %>% 
  select(original = clase.x,clasificacion_a = descripcion ,clasificacion_b = estab_agrupado) 

# Matriz de equivalencia clase de establecimiento

estab_homol %>% 
  pin_write(board = carpeta,
            name = "clasificacion_establecimientos")

# Formato largo de la tabla de establecimeintos

establecimientos_long <-  ras_1 %>%  
  right_join(estab_homol,by = c("clase_factor" = "original")) %>% 
  count(clasificacion_b,anio,parroquia_factor) 

# Formato ancho de la tabla de establecimeintos

establecimientos_wide <- establecimientos_long %>% 
  pivot_wider(names_from = "clasificacion_b",values_from = "n") %>% 
  mutate(across(.cols = where(is.numeric),.fns = replace_na,replace = 0)) 

# Guardamos en {pins}

establecimientos_wide %>% 
  pin_write(board = carpeta, 
            name = "bdd_establecimientos_clase")


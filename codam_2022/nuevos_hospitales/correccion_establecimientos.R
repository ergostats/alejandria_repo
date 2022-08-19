library(pins)
library(tidyverse)
library(glue)
library(rlang)
library(psych)


path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)

pin_list(carpeta)


tablas <- c("bdd_establecimientos_clase",
  "bdd_establecimientos_equipamiento") %>% 
  set_names() %>% 
  map(pin_read,board = carpeta)


parroquias_diccionario <- read_excel("codam_2022/nuevos_hospitales/CODIFICACIÓN_2022.xlsx",sheet = "PARROQUIAS") %>% 
  janitor::clean_names() %>% 
  mutate(dpa_parurb = if_else(!is.na(dpa_parurb), dpa_parurb, dpa_parroq)) %>% 
  select(dpa_parurb, dpa_parroq)


  map2(.x = tablas,
       .y = c("parroquia_factor",
              "parr_ubi"),
       .f = ~{
    
         var_parr <- .y
         
    .x %>% 
      rename_with(.cols = var_parr,~"parr_ubi") %>% 
      left_join(parroquias_diccionario,
                by = c("parr_ubi" = "dpa_parurb")) %>% 
      group_by(dpa_parroq,anio) %>% 
      summarise(across(where(is.numeric),sum))
    
  }) %>% 
    imap(~{
      
      objeto <- .x
      
      names_1 <- .y
      
      glue(
        "pin_write(board = carpeta,
                x = objeto,
                name = '{names_1}')") %>% 
        parse_expr() %>% 
        eval()
      
      
      
    })
  
  bdd_establecimientos_equipamiento <-  pin_read(carpeta,name = "bdd_establecimientos_equipamiento") %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(suma_completos = sum(across(.cols = matches("equip_.+completos"))),
              suma_parciales = sum(across(.cols = matches("equip_.+parcial"))))
  
  
  pin_write(board = carpeta,
            x = bdd_establecimientos_equipamiento, 
            name = "bdd_establecimientos_equipamiento")
  
  bdd_res <- bdd_establecimientos_equipamiento %>% 
    group_by(anio) %>% 
    nest(data = -anio) %>% 
    mutate(data = map(data,describe),
           data = map(data,rownames_to_column)) 

  
  pin_write(board = carpeta,x = bdd_res,name = "resumen_estadisticos_equipamiento")
  
  bdd_res <- bdd_res %>% 
    unnest()
    ggplot(mapping = aes(x = var))
    
  
  View(bdd_establecimientos_equipamiento)  
  
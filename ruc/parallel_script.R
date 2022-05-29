# ------------------------------------------------------------------------- # 
#                     Parallel processing para geocoding                    #
# ------------------------------------------------------------------------- # 

library(furrr)
library(tidyverse)
library(lubridate)
library(rlang)
library(tictoc)
library(bench)


data <- tibble(ind = 1:20000000,
               var_2 = sample(letters,20000000,replace = T),
               var_1 = sample(letters,20000000,replace = T),
               var_3 = sample(letters,20000000,replace = T),
               var_4 = sample(letters,20000000,replace = T))



processing_function <-  function(id,inicio,final){
  
  temp <- data %>% 
    filter(ind %in% inicio:final) %>% 
    mutate(cadena = str_c(var_1,var_2,var_3,var_4),
           n_a = str_count(cadena,pattern = "a"),
           n_s = str_count(cadena,pattern = "s"),
           n_d = str_count(cadena,pattern = "d"),
           n_f = str_count(cadena,pattern = "f")) %>% 
    mutate(total_busqueda = n_a + n_s + n_d + n_f)
  
  write_lines(x = temp,file = str_c("ruc/pruebas_parallel/",id,".txt"),append = T)
  
}



argument_list <- list(
  id = rep(c("a","b","c","d"),100),
  inicio = seq(1,20000000,by = 50000) ,
  final = c(seq(50000,20000000,by = 50000)))


no_cores <- availableCores() - 1

plan(multicore, workers = no_cores)

(comparison <- bench::mark(
  iterations = 10000,
  pmap(
    .l = argument_list,
    .f = processing_function
  ),
  future_pmap(
    .l = argument_list,
    .f = processing_function
    )
))

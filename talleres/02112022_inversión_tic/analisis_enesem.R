
# Librerias ---------------------------------------------------------------

library(tidyverse)
library(srvyr)
library(survey)
library(haven)
library(broom)


# Lectura -----------------------------------------------------------------

enesem_raw <- read_sav("talleres_externos/encuestas_tidyvverse_23092022/2020_ENESEM_Modulo_de_TIC/2020_ENESEM_BDD_SPSS_TIC/2020_ENESEM_BDD_TIC.sav")

glimpse(enesem_raw)


# Diseño muestral ---------------------------------------------------------

objeto_survey <- as_survey_design(.data = enesem_raw,
                                  strata = c(cod_tamano,
                                             cod_sector),
                                  weights = f_exp)

# Inversion promedio en TIC -----------------------------------------------


objeto_survey %>% 
  summarise(tic_promedio = mean(tic1_2,na.rm = T)) 

# Error: No estamos ponderando con el f_exp

objeto_survey %>% 
  summarise(tic_promedio = survey_mean(tic1_2,
                                       na.rm =T,
                                       level = 0.9,
                                       vartype = c("se","ci")))


# inversion promedio en TIC por sector ------------------------------------

resumen_tic_sector <- objeto_survey %>% 
  group_by(des_sector) %>% 
  summarise(tic_promedio = survey_mean(tic1_2,
                                       na.rm =T,
                                       level = 0.9,
                                       vartype = c("se","ci")))


# Grafico para los intervalos de confianza para la media ------------------

resumen_tic_sector %>% 
  ggplot() +
  geom_segment(aes(x = des_sector,
                   xend = des_sector,
                   y = tic_promedio_low,
                   yend = tic_promedio_upp)) +
  geom_point(aes(x = des_sector,
                 y = tic_promedio),
             color = "red")


# Variable total empleados ------------------------------------------------


objeto_survey <- objeto_survey %>% 
  mutate(total_empleados = tic2_5_pers_ocup_h + tic2_5_pers_ocup_m)


# Total de empleados a nivel agregado -------------------------------------


objeto_survey %>% 
  summarise(total_empleados = survey_total(x = total_empleados,
                                           deff = T,
                                           na.rm = T
                                           ))

# Total empleados por tamaño ----------------------------------------------

objeto_survey %>% 
  group_by(des_tamano) %>% 
  summarise(total_empleados = survey_total(x = total_empleados,
                                           deff = T,
                                           na.rm = T
  ))


# Primer y tercer cuartil de la inversión en TIC --------------------------


objeto_survey %>% 
  summarise(inversion_tic = survey_quantile(x = tic1_2,
                                              quantiles = c(0.25,.75,0.99),
                                              na.rm = T))


objeto_survey %>% 
  group_by(des_sector) %>% 
  summarise(inversion_tic = survey_quantile(x = tic1_2,
                                              quantiles = c(0.5),
                                              na.rm = T),
            inversion_tic_alt = survey_median(x = tic1_2,
                                              na.rm = T)
            )


# Ventas promedio por cada dolar invertido en TIC -------------------------

objeto_survey %>% 
  summarise(ventas_per_tic = survey_ratio(numerator = tic3_1_1_ventas,
                                          denominator = tic1_2,
                                          na.rm = T))

# por tamaño de empresa


objeto_survey %>% 
  group_by(des_tamano) %>% 
  summarise(ventas_per_tic = survey_ratio(numerator = tic3_1_1_ventas,
                                          denominator = tic1_2,
                                          na.rm = T))


# Estimación condicional vs estimación conjunta ---------------------------


objeto_survey %>% 
  group_by(des_tamano,
           des_sector) %>% 
  summarise(inversion_prom = survey_mean(tic1_2,na.rm =T),
            total_inversion = survey_total(tic1_2,na.rm =T),
            n = unweighted(n())) 


# Proporciones ------------------------------------------------------------

# Condicional:

objeto_survey %>% 
  group_by(des_tamano,
           des_sector) %>% 
  summarise(proporcion = survey_mean(),
            poblacion = survey_total(),
            n = unweighted(n())) 

# Conjunta:

objeto_survey %>% 
  group_by(interaction(des_tamano,
                       des_sector)) %>% 
  summarise(proporcion = survey_mean(),
            poblacion = survey_total(),
            n = unweighted(n()))


# Prueba T para la media --------------------------------------------------

# Las ventas de empresas que invierten en TIC son mayores a las 
# ventas de las empresas que no invierten en TIC

objeto_survey %>% 
  survey_count(tic1_1)

objeto_survey %>% 
  svyttest(formula = tic3_1_1_ventas ~ tic1_1  )


# Prueba chi cuadrado -----------------------------------------------------

objeto_survey %>% 
  svychisq(formula =  ~ des_tamano + des_sector)



# Regresión lineal --------------------------------------------------------

objeto_survey %>% 
  svyglm(formula = tic3_1_1_ventas ~ tic1_2* des_sector + total_empleados ) %>% 
  summary() 


resumen <- objeto_survey %>% 
  mutate(tic1_1 = as_factor(tic1_1)) %>% 
  group_by(tic1_1,des_sector,des_tamano) %>% 
  summarise(ventas = survey_mean(tic3_1_1_ventas)) %>% 
  select(-ventas_se) %>% 
  pivot_wider(names_from = c(des_tamano,tic1_1),values_from = ventas)

resumen %>% 
  mutate(across(where(is.numeric),scales::dollar)) %>% 
  ungroup() %>% 
  gt() %>% 
  cols_label(
    des_sector = "Sector económico",
    `Grande Empresa_Si` = "Grande",
    `Mediana Empresa A_Si` = "Mediana A",
    `Mediana Empresa B_Si` = "Mediana B",
    `Grande Empresa_No`= "Grande",
    `Mediana Empresa A_No` = "Mediana A",
    `Mediana Empresa B_No`= "Mediana B",
    `Grande Empresa_NA` = "Vacios"
  ) %>% 
  tab_spanner(label = "Con inversión en TIC",
              columns = c(`Grande Empresa_Si`,
                          `Mediana Empresa A_Si`,
                          `Mediana Empresa B_Si`)
              
              ) %>% 
  tab_spanner(label = "Sin inversión en TIC",
              columns = c(`Grande Empresa_No`,
                          `Mediana Empresa A_No`,
                          `Mediana Empresa B_No`)
              
  )

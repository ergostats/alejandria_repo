
# ------------------------------------------------------------------------- #
#                       Creación de tabla de análisis                       #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(gt)
library(gtExtras)
library(broom)
library(scales)
library(haven)
library(gtsummary)


# Lectura de las bases de datos -------------------------------------------


enesem_tic <- read_sav(file = file.path("talleres_externos",
                                        "taller_tablas_gt_19112022",
                                        "data",
                                        "2020_ENESEM_BDD_TIC.sav"))

enesem_mo1 <- read_sav(file = file.path("talleres_externos",
                                        "taller_tablas_gt_19112022",
                                        "data",
                                        "2020_ESTRUCTURAL_EMPRESARIAL_BDD.sav"))


glimpse(enesem_mo1)
# Variables del modulo 1: -------------------------------------------------

enesem_mo1 <- enesem_mo1 %>% 
  select(id_empresa,
         provincia,
         total_cg_pub = v1124,
         total_ingresos = v1001,
         total_ventas = v1003,
         gastos_operacionales = totgadop,
         personal_ocupado = totalpeoc,
         fbkf = fbkf_1,
         total_adquisiciones = totadquisi
  )

enesem_tic <- enesem_tic %>% 
  select(
    id_empresa,
    provincia,
    cod_letra,
    cod_sector,
    cod_tamano,
    cod_sector_tic,
    cod_tamano_tic,
    inversion_tic = tic1_2,
    ventas_online = tic3_1_1_ventas_x_internet,
    tic2_5_pers_ocup_internet_h,
    tic2_5_pers_ocup_internet_m,
    tic4_1_1_pers_ocup_usa_disp_h,
    tic4_1_1_pers_ocup_usa_disp_m,
    tic6_1_1_pers_ocup_especialista_h,
    tic6_1_1_pers_ocup_especialista_m
    )


# Unión de las bases ------------------------------------------------------

datos_tic <- enesem_mo1 %>% 
  inner_join(y = enesem_tic)

rm(enesem_mo1,enesem_tic)

# Para el análisis emplearemos la variable `personal_ocupado`
# Para el análisis emplearemos la variable `personal_ocupado_en_tic`


# Construcción de indicadores ---------------------------------------------

# A nivel de empresa

datos_tic <- datos_tic %>% 
  rowwise() %>% 
  mutate(
    personal_ocupado_tic = sum(tic2_5_pers_ocup_internet_h,
                               tic2_5_pers_ocup_internet_m,
                               tic4_1_1_pers_ocup_usa_disp_h,
                               tic4_1_1_pers_ocup_usa_disp_m,
                               tic6_1_1_pers_ocup_especialista_h,
                               tic6_1_1_pers_ocup_especialista_m,
                               na.rm = T)) %>% 
  select(-c(tic2_5_pers_ocup_internet_h,
            tic2_5_pers_ocup_internet_m,
            tic4_1_1_pers_ocup_usa_disp_h,
            tic4_1_1_pers_ocup_usa_disp_m,
            tic6_1_1_pers_ocup_especialista_h,
            tic6_1_1_pers_ocup_especialista_m)) %>% 
  ungroup()


# A nivel de industria:

industry_index <- datos_tic %>% 
  mutate(valor_add = total_ventas - total_adquisiciones) %>% 
  group_by(cod_letra) %>% 
  mutate(
    participacion = total_ventas/sum(total_ventas,na.rm = T),
    participacion_2 = participacion^2,
    participacion_on = ventas_online/sum(ventas_online,na.rm = T),
    participacion_on_2 = participacion_on^2) %>% 
  summarise(
    herfindal = sum(participacion_2,na.rm = T)*10000,
    herfindal_on = sum(participacion_on_2,na.rm = T)*10000,
    sum_vadd = sum(valor_add),
    sum_capital = sum(fbkf)
  ) %>% 
  ungroup() %>% 
  mutate(capital_intensity = sum_capital/sum_vadd) %>% 
  select(-sum_vadd,
         -sum_capital)


# Calcular los per employee
# Calcular el herfindal de industria
# Modelo con efectos fijos, sin efectos fijos, instrumental variable

datos_tic <- datos_tic %>% 
  inner_join(industry_index)


# Valores per capita: -----------------------------------------------------

datos_tic <- datos_tic %>% 
  mutate(across(c(total_cg_pub,
                  total_ingresos,
                  total_ventas,
                  gastos_operacionales,
                  inversion_tic),
                .fns = list(per_capita = ~if_else(personal_ocupado == 0,0,.x/personal_ocupado))))


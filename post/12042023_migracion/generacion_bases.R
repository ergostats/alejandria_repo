library(haven)
library(gt)
library(gtExtras)
library(tidyverse)

esi <- read_dta("../migracion/esi_2021.dta")

# Vistazo a la tabla

glimpse(esi)

# Diccionario de variables

diccionario <- esi %>% 
  map(attributes) %>% 
  map_chr("label") %>% 
  enframe(name = "variable",value = "etiqueta")


diccionario_class <- esi %>% 
  map_chr(typeof) %>% 
  enframe(name = "variable",value = "clase") %>% 
  mutate(clase = if_else(variable %in% c("anio_movi","dia_movi"),  "double","factor" ))

diccionario <- diccionario %>% 
  inner_join(diccionario_class)


var_diccionario <- esi %>% 
  map(attributes) %>% 
  map("labels") %>% 
  map(enframe,name = "etiqueta",value = "codigo") %>% 
  imap(~mutate(.x,variable = .y,
               across(everything(),as.character))) %>% 
  reduce(bind_rows)


primera_base <- esi %>% 
  count(anio_movi,
        mes_movi,
        sex_migr,
        tip_naci,
        tip_movi,
        edad,
        via_tran) %>% 
  mutate(across(c(sex_migr,
                  tip_naci,
                  tip_movi,
                  via_tran),
                as_factor))


# Por sexo

primera_base %>% 
  group_by(tip_movi,sex_migr,tip_naci) %>% 
  summarise(mig = sum(n)) %>% 
  pivot_wider(names_from = tip_movi,values_from = mig) %>% 
  ungroup() %>% 
  rename(Entradas_Número = Entrada,
         Salidas_Número = Salida) %>% 
  mutate(Entradas_Porcentaje = Entradas_Número/sum(Entradas_Número),
         Salidas_Porcentaje = Salidas_Número/sum(Salidas_Número)) %>%
  select(sex_migr, tip_naci,matches("Entrada"),matches("Salida")) %>% 
  group_by(tip_naci) %>%
  gt() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    sex_migr = ""
  ) %>% 
  tab_spanner_delim(columns = matches("Entrada|Salida"),delim = "_") %>% 
  gt::fmt_percent(columns = matches("Porcent")) %>% 
  gt::fmt_number(columns = matches("Númer"),decimals = 0,sep_mark = ".") %>% 
  cols_width(everything() ~ px(100)) %>% 
  data_color(columns = matches("Porcent"),
             colors = "lightblue") %>% 
  gt::tab_header(
    title = "Flujo migratorio del Ecuador por sexo",
    subtitle = "Número y porcentaje de ecuatorianos y extranjeros"
  ) %>%
  # gt::summary_rows(
  #   columns = c(Entradas_Número,
  #               Salidas_Número,
  #               Entradas_Porcentaje,
  #               Salidas_Porcentaje),
  #   fns = list(Total = ~sum(.)),
  #   formatter = fmt_number,
  #   decimals = 0,
  #   sep_mark = ".") %>%
  # gt::summary_rows(
  #   groups = c("Ecuatoriano","Extranjero"),
  #   columns = matches("Núm|Porcent"),
  #   fns = list(Total = ~sum(.)),
  #   formatter = fmt_percent,
  #   decimals = 0,
  #   sep_mark = ".") %>%
  gt::tab_footnote(
    md("**Fuente:** Registro de Entradas y Salidas Internacionales, INEC")
  )



# Por rangos de edad

primera_base %>% 
  mutate(grupo_edad = cut(edad,
                          breaks = c(-Inf,18,30,65,Inf),
                          labels = c("Entre 0 y 18",
                                     "Entre 18 y 30",
                                     "Entre 30 y 65",
                                     "Mayores de 65"))) %>% 
  group_by(tip_movi,grupo_edad,tip_naci) %>% 
  summarise(mig = sum(n)) %>% 
  pivot_wider(names_from = tip_movi,values_from = mig) %>% 
  ungroup() %>% 
  rename(Entradas_Número = Entrada,
         Salidas_Número = Salida) %>% 
  mutate(Entradas_Porcentaje = Entradas_Número/sum(Entradas_Número),
         Salidas_Porcentaje = Salidas_Número/sum(Salidas_Número)) %>%
  select(grupo_edad, tip_naci,matches("Entrada"),matches("Salida")) %>% 
  group_by(tip_naci) %>%
  gt() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    grupo_edad = "Edad (Años)"
  ) %>% 
  tab_spanner_delim(columns = matches("Entrada|Salida"),delim = "_") %>% 
  gt::fmt_percent(columns = matches("Porcent")) %>% 
  gt::fmt_number(columns = matches("Númer"),decimals = 0,sep_mark = ".") %>% 
  cols_width(
    everything() ~ px(130)) %>% 
  data_color(columns = matches("Porcent"),
             colors = "lightblue") %>% 
  gt::tab_header(
    title = "Flujo migratorio del Ecuador por grupos de edad",
    subtitle = "Número y porcentaje de ecuatorianos y extranjeros"
  ) %>% 
  # gt::summary_rows(
  #   groups = c("Ecuatoriano","Extranjero"),
  #   columns = matches("Núm"),
  #   fns = list(Total = ~sum(.)),
  #   formatter = fmt_number,
  #   decimals = 0,
  #   sep_mark = ".") %>% 
  gt::tab_footnote(
    md("**Fuente:** Registro de Entradas y Salidas Internacionales, INEC")
  )


# Por transporte

primera_base %>% 
  group_by(tip_movi,via_tran,tip_naci) %>% 
  summarise(mig = sum(n)) %>% 
  pivot_wider(names_from = tip_movi,values_from = mig) %>% 
  ungroup() %>% 
  rename(Entradas_Número = Entrada,
         Salidas_Número = Salida) %>% 
  mutate(Entradas_Porcentaje = Entradas_Número/sum(Entradas_Número,na.rm = T),
         Salidas_Porcentaje = Salidas_Número/sum(Salidas_Número,na.rm = T)) %>%
  select(via_tran, tip_naci,matches("Entrada"),matches("Salida")) %>% 
  group_by(tip_naci) %>%
  gt() %>% 
  gtExtras::gt_theme_538() %>% 
  cols_label(
    via_tran = "Medio de transporte"
  ) %>% 
  tab_spanner_delim(columns = matches("Entrada|Salida"),delim = "_") %>% 
  gt::fmt_percent(columns = matches("Porcent")) %>% 
  gt::fmt_number(columns = matches("Númer"),decimals = 0,sep_mark = ".") %>% 
  cols_width(
    everything() ~ px(130)) %>% 
  data_color(columns = matches("Porcent"),
             colors = "lightblue") %>% 
  gt::tab_header(
    title = "Flujo migratorio del Ecuador de acuerdo al medio de transporte",
    subtitle = "Número y porcentaje de ecuatorianos y extranjeros"
  ) %>% 
  # gt::summary_rows(
  #   groups = c("Ecuatoriano","Extranjero"),
  #   columns = matches("Núm"),
  #   fns = list(Total = ~sum(.)),
  #   formatter = fmt_number,
  #   decimals = 0,
  #   sep_mark = ".") %>% 
  gt::tab_footnote(
    md("**Fuente:** Registro de Entradas y Salidas Internacionales, INEC")
  ) %>% 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = ""
  )


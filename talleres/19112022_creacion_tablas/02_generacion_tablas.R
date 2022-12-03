
# Llamamos al script de procesamiento -------------------------------------

source("talleres_externos/taller_tablas_gt_19112022/scripts/01_procesamiento_enesem.R")


# 1. Resumen de las industrias --------------------------------------------

primer_resumen <- datos_tic %>% 
  mutate(dummy_tic = as.numeric(!is.na(inversion_tic) )) %>% 
  # mutate(across(c(cod_sector,cod_letra),as_factor)) %>% 
  group_by(cod_sector,cod_letra) %>% 
  summarise(across(.cols = c(herfindal,
                     herfindal_on,
                     capital_intensity),
                   .fns = unique),
            across(.cols = c(total_cg_pub_per_capita,
                             total_ingresos_per_capita,
                             inversion_tic_per_capita),
                   .fns = mean,
                   na.rm = T),
            proporcion = sum(dummy_tic,na.rm = T)/n())

write_rds(primer_resumen,"talleres_externos/taller_tablas_gt_19112022/data/industrias_resumen.rds")

primer_resumen %>% 
  gt()

primer_resumen %>%
  ungroup() %>% 
  gt() %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FFC300")
    ),
    locations = cells_body(
      columns = herfindal,
      rows = herfindal <= 1500
    )
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#FF5733")
    ),
    locations = cells_body(
      columns = herfindal,
      rows = herfindal > 1500 & herfindal <= 2500
    )
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "#C70039"),
      cell_text(color = "white")
    ),
    locations = cells_body(
      columns = herfindal,
      rows = herfindal > 2500 
    )
  )  %>%
  data_color(proporcion,
             scales::col_numeric(palette = c("#6EFCD7",
                                             "#108164"),
                                 domain = c(0.2,1)))



# Con gtsummary -----------------------------------------------------------

tabla_reducida <- datos_tic %>% 
  mutate(dummy_tic = as.numeric(!is.na(inversion_tic))) %>% 
  select(dummy_tic,
         total_ingresos,
         total_cg_pub,
         personal_ocupado,
         cod_sector)

tbl_summary(tabla_reducida,by = "dummy_tic",) %>% 
  ass_p()


tbl_summary(
  tabla_reducida,
  by = "dummy_tic",
  statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  )
) %>%
  add_p(test = all_continuous() ~ "t.test")



# Modelo ------------------------------------------------------------------

m1 <- glm( ~ trt + grade + age, data = data_tic, family = binomial)

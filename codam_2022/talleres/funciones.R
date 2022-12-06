 generar_mapa <- function(tabla_plazas,
         anio_mapa,
         tipo_mapa,
         mapa){
  
  tabla_plazas %>%
    filter(anio == anio_mapa,
           unidad_legal_tipo == tipo_mapa) %>% 
    right_join(mapa,
               c("codigo_provincia" = "DPA_PROVIN")) %>%
    ggplot() +
    geom_polygon(aes(x = long,
                     y = lat,
                     group = group,
                     fill = plazas_totales)) +
    theme_minimal()
 }
 
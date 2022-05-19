library(tidyverse)
library(ggmap)
library(janitor)
library(leaflet)
library(glue)
librar
ruc_completo <- read_rds("C:/Users/Alex/OneDrive/Documentos/RAR/RUC/ruc_completo_establecimientos.rds")

marker_icon <- makeIcon(
  iconUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-icon.png",
  shadowUrl = "https://cdnjs.cloudflare.com/ajax/libs/leaflet/1.8.0-beta.0/images/marker-shadow.png",
)

ruc_completo <- clean_names(ruc_completo)

ubicacion <- ruc_completo %>% 
  select(
    # Identificación de la empresa:
    
    numero_ruc,
    razon_social,
    nombre_comercial,
    nombre_fantasia_comercial,
    tipo_contribuyente,
    
    # Ubicación DPA:
    
    descripcion_provincia,
    descripcion_canton,
    descripcion_parroquia,
    
    # Establecimiento:
    
    numero_establecimiento,
    calle,
    numero,
    interseccion
  )

rm(ruc_completo)


# Generando nuevas variables ----------------------------------------------

ubicacion <- ubicacion %>% 
  mutate(
    nombre_final = case_when(
      !is.na(nombre_fantasia_comercial) ~ nombre_fantasia_comercial,
      !is.na(nombre_comercial) ~ nombre_comercial,
      !is.na(razon_social) ~ razon_social
    )
  ) %>% 
  select(-c(
    nombre_fantasia_comercial,
    nombre_comercial,
    razon_social
  ))

# Diagnóstico de un texto con error de encodign en la lectura:
ubicacion <- ubicacion %>% 
  map_at(.at = c( "descripcion_provincia",
                  "descripcion_canton",
                  "descripcion_parroquia",
                  "calle",
                  "interseccion",
                  "nombre_final"),
         ~{
    
    variable <- .x

    Encoding(variable) <- "latin1"
    
    return(variable)
    
  })

ubicacion <- as_tibble(ubicacion)

textclean::check_text(ubicacion$direccion[157])

# Es necesario cambiar el encoding del texto dado que: 
# Unicode characters that are not mapped to UTF8




ubicacion <- ubicacion %>% 
  mutate(
    across(.cols = c( "descripcion_provincia",
                      "descripcion_canton",
                      "descripcion_parroquia",
                      "calle",
                      "interseccion",
                      "nombre_final"),
           .fns = textclean::replace_non_ascii)
  )


ubicacion <- ubicacion %>% 
  mutate(
    across(.cols = c( "descripcion_provincia",
                      "descripcion_canton",
                      "descripcion_parroquia",
                      "calle",
                      "interseccion",
                      "nombre_final"),
           .fns = ~replace_na(.x,"")),
    direccion = str_c(calle,
                      interseccion,
                      numero,
                      descripcion_parroquia, 
                      "Ecuador",
                      sep = ", ")
  )

ubicacion <- ubicacion %>% 
  mutate(
    direccion = str_replace(direccion,", ,",", ")
  )

  
write_rds(ubicacion,"C:/Users/Alex/OneDrive/Documentos/RAR/RUC/ubicacion_corregida.rds")

ubi_quito <- ubicacion %>% 
  filter(str_detect(descripcion_canton,
                    pattern = "QUITO"))

ubi_quito <- ubi_quito %>% 
  rowid_to_column(var = "id_dir")

georeference_df <- ubi_quito %>% 
  select(id_dir,direccion) 

georeference_df_reduced <- georeference_df %>% 
  slice(1:100)  
  
test <- ubi_quito %>%
  slice(1:100) %>% 
  mutate(
    # inject = glue()
    
    georefence = map(direccion,safely(geocode),output = "latlona",source = "google", 
                          inject = c("components"="country:EC&administrative_area_level_1:PICHINCHA")))

test_2 <- test %>% 
  mutate(
    reduced = map(georefence,"result"),
    reduced = map(reduced,~if(is.null(.x)){tibble()}else{.x})) %>% 
    # reduced = map(reduced,select,lon,lat)) %>% 
  select(-georefence) %>%
  unnest(reduced)

write_rds(test_2,file = "C:/Users/Alex/OneDrive/Documentos/RAR/RUC/test_geo.rds")

leaflet(test_2) %>% 
  leaflet::addTiles() %>% 
  addMarkers(lng = ~lon,
             lat = ~lat,
             popup = ~htmltools::htmlEscape(direccion),
             label = ~direccion,
             icon = marker_icon)




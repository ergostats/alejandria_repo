library(tidyverse)
library(furrr)
library(ggmap)
library(janitor)
library(leaflet)
library(glue)
library(textclean)

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
                      "numero",
                      "calle",
                      "interseccion",
                      "nombre_final"),
           .fns = ~replace_na(.x,"")),
    direccion = str_c(calle,
                      interseccion,
                      numero,
                      descripcion_parroquia,
                      sep = ", ")
  )

ubicacion <- ubicacion %>% 
  mutate(
    direccion = str_replace(direccion,", ,",", ")
  )

  
write_rds(ubicacion,"C:/Users/Alex/OneDrive/Documentos/RAR/RUC/ubicacion_corregida.rds")

ubicacion <- read_rds("C:/Users/Alex/OneDrive/Documentos/RAR/RUC/ubicacion_corregida.rds")

register_google(key = "AIzaSyC2cHMss2yQRCPP4mSvC6yG7YUaqzN4fJ0", write = TRUE)

ubi_quito <- ubicacion %>% 
  filter(str_detect(descripcion_canton,
                    pattern = "QUITO"))

ubi_quito <- ubi_quito %>% 
  rowid_to_column(var = "id_dir")

georeference_df <- ubi_quito %>% 
  select(id_dir,direccion) 


argumentos <- list(
  archivo =  rep(str_c("parte_", 1:8),4)[1:30],
  begin = seq(from = 1,to = nrow(georeference_df),by = 50000),
  end = c(seq(from = 50000,to = nrow(georeference_df),by = 50000),nrow(georeference_df))
  
)

no_cores <- availableCores() - 2

oplan <- plan(multisession, workers = no_cores)

geo_df <- function(x){
  g <- safely(.f = ggmap::geocode)
  
  library(tidyverse)
  library(ggmap)
  library(janitor)
  
  g(
    location = x,
    output = "latlona",
    source = "google", 
    inject = c("components"="country:EC"))
}

externa <- function(archivo,begin,end,base){
  
  
  
  test <- base %>% 
    filter(id_dir %in% begin:end) %>% 
    mutate(
      georefence = map(direccion,function(zzz) geo_df(zzz)))
  
  
  test <- test %>% 
    mutate(
      reduced = map(georefence,function(yyy)yyy[["result"]]),
      reduced = map(reduced,function(xxx)if(is.null(xxx)){tibble()}else{xxx})) %>% 
    # reduced = map(reduced,select,lon,lat)) %>% 
    select(-georefence) %>%
    unnest(reduced)
  
  # time <- rnorm(n = 1,mean = 7.5,sd = 10)
  
  msm <- str_c("Listo tramo desde ",begin," hatsta ",end)
  
  # Sys.sleep(time)
  
  print(msm)
  
  return(test)
  
  write_tsv(x = test,file = str_c("ruc/ruc_scrapped/",archivo,".txt"),append = T)
  
}

geolocalization_uio <- future_pmap(
  .l = argumentos,
  .f = externa,
  base = georeference_df)


on.exit(plan(oplan),add = T)

georeference_df %>% 

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




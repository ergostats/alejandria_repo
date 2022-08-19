
# demanda de salud --------------------------------------------------------

# ------------------------------------------------------------------------- #
#            Limpieza y generación de ofertas de servicio de salud          #
# ------------------------------------------------------------------------- #


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(rlang)
library(haven)
library(readxl)
library(janitor)
library(scales)
library(patchwork)
library(pins)
library(labelled)
library(fuzzyjoin) 

# Carpeta PINS ------------------------------------------------------------

path_temporal <- "C:/Users/Alex/OneDrive/Documentos/RAR/"

carpeta <- board_folder(path = path_temporal)



dicc <- pin_read(carpeta,"diccionario_RAS") %>% 
  mutate(label = str_extract(str_to_lower(descripcion),".* [:digit:]{1}"),
         label = str_remove_all(label,"[:punct:]"),
         label = str_remove(label," o de llamada y menos de 4"),
         label = str_replace_all(label,"[:space:]","_"),
         label = str_replace_all(label,"_+","_"),
         bloque_var = str_remove_all(label,"_[46]"),
         bloque_var = str_remove_all(bloque_var,"_eventual"))

# Excluimos los totales, para evitar contar dos veces ---------------------

excluidos <- dicc %>% 
  filter(str_detect(str_to_lower(descripcion),"uso inec")) %>% 
  pull(codigo_de_la_variable) %>% 
  str_to_lower()


# Identificadores basicos de la base del RAS ------------------------------

identificaciones <- c("parr_ubi", "clase","sector","area")

# Diccionario de clase de establecimiento ---------------------------------

dicc_clase <- tribble(
  ~clase,~descripcion,
  1 , "Hospital Básico",
  2 , "Hospital General",
  3 , "Infectología",
  4 , "Gineco-Obstétrico",
  5 , "Pediátrico",
  6 , "Psiquiátrico y Sanatorio de Alcohólicos",
  7 , "Dermatológico (leprocomios)",
  8 , "Oncológico",
  9 , "Neumológico (Antituberculoso)",
  10 , "Geriátrico",
  11 , "Hospital de Especialidades",
  12 , "Clínica General (sin especialidad) (Privada)",
  13 , "Gineco-Obstetricia",
  14 , "Pediatría",
  15 , "Traumatología",
  16 , "Psiquiatría",
  17 , "Otras Clínicas especializadas",
  20 , "Puesto de Salud",
  21 , "Subcentro de Salud",
  22 , "Centro de Salud A",
  23 , "Centro de Salud B",
  24 , "Centro de Salud C",
  25 , "Dispensario Médico (Policlínico)",
  26 , "Consultorio General",
  27 , "Consultorio de Especialidad(es) Clínico -Quirúrgico",
  28 , "Centro de Especialidades",
  29 , "Centro Clínico-Quirúrgico Ambulatorio (Hospital del Día)",
  30 , "Centros especializados",
  31 , "Otros establecimientos sin internación"
)


# Doctores y tiempos ------------------------------------------------------

# Variables de doctores a tiempo completo

dicc <- dicc %>%
  mutate(bloque_var = str_replace(bloque_var,"_8","_completos"),
         codigo_de_la_variable = str_to_lower(codigo_de_la_variable)) 

ras_1 <- read_rds("codam_2022/bases_rds/ras_procesando.rds")

ras_1

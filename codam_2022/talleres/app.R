library(tidyverse)
library(gt)
library(patchwork)
library(scales)
library(pins)
library(shiny)
library(bslib)
library(DT)


source("funciones.R")

carpeta <- board_register(board = "github",
                          repo = "AlexB4891/taller_codam_2022",
                                 branch = "main",
                                 token = "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")

theme <- bs_theme(
  bg = "#03071e", fg = "white", primary = "#f48c06",
  base_font = font_google("Noto Sans"),
  code_font = font_google("Space Mono"),
  heading_font = font_google("Oswald")
)


# gitcreds::gitcreds_set(url = )

ui <- fluidPage(
  theme = theme,
  sidebarLayout(sidebarPanel = sidebarPanel(
    uiOutput("selector_provincia"),
    sliderInput(inputId = "anio",label = "Año de estudio",
                min = 2012,
                max = 2020,
                value = 2020),
    uiOutput("selector_tipo")),
    mainPanel = mainPanel(plotOutput("mapa_plot")))
  
)

server <- function(input, output, session) {
  
  mapa <- pin_reactive(board = carpeta,
                    name = "shape_provincia",
                    interval = 6000)
  
  
  datos_provincia <- pin_reactive(board = carpeta,
                       name = "die_plazas_totales",
                       interval = 6000)

  output$selector_provinicia <- renderUI({
    
    choices <- isolate(mapa()) %>% 
      pull(DPA_PROVIN) %>% 
      unique
    
    selectInput(inputId = "provin",
                label = "Selecciona una provincia",
                choices = choices,
                selected = "01")
    
  })
  
  output$selector_tipo <- renderUI({
    
    choices <- isolate(datos_provincia()) %>% 
      pull(unidad_legal_tipo) %>% 
      unique
    
    selectInput(inputId = "tipo",
                label = "Selecciona un tipo",
                choices = choices,
                selected = "Persona Jurídica")
    
  })
  
  
  output$metadato_mapa <- renderText({
    
    pin_info(name = "shape_provincia",
             board = carpeta)
    
  })
  
  output$metadato_plazas <- renderText({
    
    pin_info(name = "shape_provincia",
             board = carpeta)
    
  })
  
  
  output$mapa_plot <- renderPlot({
    
    generar_mapa(tabla_plazas = datos_provincia(),
                 anio_mapa = input$anio,
                 tipo_mapa = input$tipo,
                 mapa = mapa())
    
    # mapa() %>%   
    #   filter(DPA_PROVIN == input$provin) %>% 
    #   ggplot() +
    #   geom_polygon(aes(x = long,y = lat,group = group,fill = DPA_PROVIN))
    
  })
  
}

shinyApp(ui, server)
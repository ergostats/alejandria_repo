#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

carpeta <- board_folder("C:/Users/Esteban/OneDrive/RAR")

bdd_index_ids <- pin_read(carpeta, name = "bdd_index_ids")
shape_parroquia <- pin_read(carpeta, name = "shape_parroquia")
shape_centroides<- pin_read(carpeta, name = "shape_parroquia_centroides")

bdd_index_ids <- bdd_index_ids %>% mutate(prov=str_sub(dpa_parroq,1,2))
provincias <- bdd_index_ids %>% pull(prov) %>% unique()

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("year",
                  "Año:",
                  min = 2015,
                  max = 2019,
                  value = 2015),
      selectInput("prov", "Provincia", choices = provincias, selected="17")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    tabla <- bdd_index_ids %>% filter(anio==input$year,
                                      prov==input$prov)
    
    graph_mapa(tabla = tabla,
               fill_var = "ids_index",
               dpa = "dpa_parroq",
               centroides = shape_centroides,
               year = "anio",
               shape = shape_parroquia %>%   
                 filter(str_detect(DPA_PARROQ,str_c("^", input$prov)))) +
      scale_fill_viridis_c(option = "inferno")+
      # facet_grid(rows = vars(anio)) +
      theme(legend.position = "bottom",
            legend.title = element_blank()) +
      labs(title = "b) Egresos hospitalarios - Año: {frame_time}'",
           subtitle = "Logaritmo del número total de egresos")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

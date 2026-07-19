library(shiny)
library(tidyverse)
library(DT)

#Limpieza de datos

dataset <- dataset %>%
  distinct(track_id, .keep_all = TRUE) %>%
  drop_na()

#Seleccionamos las variables que queremos

audio_vars <- c("danceability", "energy", "loudness", "speechiness",
                "acousticness", "instrumentalness", "liveness",
                "valence", "tempo", "duration_ms")

# Lista de géneros ordenados por popularidad media
generos_ordenados <- dataset |>
  group_by(track_genre) |>
  summarise(pop_media = mean(popularity, na.rm = TRUE)) |>
  arrange(desc(pop_media)) |>
  pull(track_genre)

#10 generos mas populares

top10_generos <- head(generos_ordenados, 10)


#UI

ui <- fluidPage(
  titlePanel("Explorando las características musicales de los éxitos de Spotify"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("generos", "Géneros a incluir:",
                  choices = generos_ordenados,
                  selected = top10_generos,
                  multiple = TRUE),
      sliderInput("rango_pop", "Rango de popularidad:",
                  min = 0, max = 100, value = c(0, 100)),
      
      selectInput("xvar", "Variable eje X (gráfico de dispersión):",
                  choices = audio_vars, selected = "energy"),
      selectInput("yvar", "Variable eje Y (gráfico de dispersión):",
                  choices = audio_vars, selected = "danceability"),
 
      sliderInput("alpha", "Transparencia de los puntos:",
                   min = 0.1, max = 1, value = 0.5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Popularidad por género",
                 plotOutput("plot_generos")),
        
        tabPanel("Distribución de popularidad",
                 plotOutput("plot_hist")),
        
        tabPanel("Características de audio",
                 plotOutput("plot_scatter")),
        
        tabPanel("Tabla de datos",
                 DTOutput("tabla"))
      )
    )
  )
)


#SERVER

server <- function(input, output) {
  
  # Filtro central: se calcula una sola vez y lo usan todos los outputs
  datos_filtrados <- reactive({
    req(input$generos)
    dataset |>
      filter(track_genre %in% input$generos,
             popularity >= input$rango_pop[1],
             popularity <= input$rango_pop[2])
  })
  
  #Popularidad media por género
  output$plot_generos <- renderPlot({
    resumen <- datos_filtrados() |>
      group_by(track_genre) |>
      summarise(pop_media = mean(popularity, na.rm = TRUE),
                cantidad = n())
    
    ggplot(resumen, aes(x = reorder(track_genre, pop_media),
                        y = pop_media, fill = cantidad)) +
      geom_col() +
      coord_flip() +
      scale_fill_gradient(low = "lightblue", high = "darkred",
                          name = "Cantidad\nde canciones") +
      labs(title = "Popularidad media por género",
           x = "Género", y = "Popularidad media")
  })
  
  #Distribución de popularidad
  output$plot_hist <- renderPlot({
    ggplot(datos_filtrados(), aes(x = popularity)) +
      geom_histogram(bins = 30, fill = "#1DB954") +
      labs(title = "Distribución de popularidad",
           x = "Popularidad", y = "Cantidad de tracks")
  })
  
  #Dispersión entre dos características de audio
  output$plot_scatter <- renderPlot({
    ggplot(datos_filtrados(),
           aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
               color = popularity)) +
      geom_point(alpha = input$alpha) +
      scale_color_gradient(low = "lightblue", high = "darkred") +
      theme_minimal() +
      labs(title = paste(input$yvar, "vs.", input$xvar),
           x = input$xvar, y = input$yvar)
  })
  
  #Tabla interactiva ---
  output$tabla <- renderDT({
    datos_filtrados() %>%
      select(track_name, artists, track_genre, popularity,
             danceability, energy, valence, tempo)
  })
}


shinyApp(ui, server)
      

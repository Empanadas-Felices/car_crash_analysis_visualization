# Angie Solís y Emilia Víquez

library(dplyr)
library(tidyverse)
library(ggplot2)
library(rjson)
library(shiny)
library(shinyjs)
library(scales)
library(plotly)
library(viridis)
library(usdata)

# ______________________________________________________________________________
# Limpieza de datos
# ______________________________________________________________________________

# Dado que el dataset inicial es muy grande, se procedió a agilizar el proceso de 
# limpieza de datos, creando archivos con los datos limpios al final del proceso de limpieza. 

# Obtenido de: https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents 
data <- read.csv("US_Accidents_March23.csv")

data <- data %>% rename(Temperature = Temperature.F.)
data <- data %>% rename(Wind_Chill = Wind_Chill.F.)
data <- data %>% rename(Humidity = Humidity...)
data <- data %>% rename(Pressure = Pressure.in.)
data <- data %>% rename(Visibility = Visibility.mi.)
data <- data %>% rename(Wind_Speed = Wind_Speed.mph.)
data <- data %>% rename(Precipitation = Precipitation.in.)

# Selección de columnas deseadas
data <- data %>% select(Start_Time, Severity, State, Temperature, Wind_Chill, Humidity, 
                       Pressure, Visibility, Wind_Speed, Precipitation, 
                       Weather_Condition, Sunrise_Sunset)

# Omitir las filas con NA
data <- na.omit(data)

# Obtener los años
data$Start_Time <- as.Date(data$Start_Time, format = "%Y-%m-%d")
data$Start_Time <- format(data$Start_Time, "%Y")
data$Start_Time <- as.integer(data$Start_Time)
data <- data[data$Start_Time < 2023, ]  # 2023 is excluded since the year hasn't finished
data <- data %>% rename(Year = Start_Time) 

dataMapa <- data
data$State <- abbr2state(data$State)

state_counts <- dataMapa %>%
group_by(State, Year) %>%
summarise(Count = n())


# Para guardar los cambios iniciales
write.csv(data, file = "archivoLimpioEstados.csv", row.names = FALSE)
write.csv(state_counts, file = "archivoMapa.csv", row.names = FALSE)


dataUnidades <- data.frame(
  Variable = c("Temperature", "Wind_Chill", "Humidity", "Pressure", "Visibility", "Wind_Speed", "Precipitation"),
  Unit = c("F", "F", "%", "in", "mi", "mph", "in")
)

# Data sets para las visualizaciones
data <- read.csv("archivoLimpioEstados.csv")
state_counts <-read.csv("archivoMapa.csv")

# Significado de las severidades
severity_levels <- c("1" = "Low", "2" = "Moderate", "3" = "High", "4" = "Very High")
severity_mapping <- c("Low" = 1, "Moderate" = 2, "High" = 3, "Very High" = 4)

# ______________________________________________________________________________
# Visualización inicial de variables no numéricas
# ______________________________________________________________________________

ggplot(data, aes(x = State)) +
  geom_bar() +
  labs(title = "Estados",
       x = "Estados",
       y = "Amount")

# ______________________________________________________________________________
# Visualización de accidentes por estado
# ______________________________________________________________________________

plot_ly(
  data = state_counts,
  type = "choropleth",
  locations = ~State,
  z = ~Count,
  locationmode = "USA-states",
  colors = c("#f7db02", "#db7b2b", "#c92424"),  
  zmin = 0,
  zmax = max(state_counts$Count),
  frame = ~Year,  
  text = ~paste("State: ", State, "<br>Count: ", Count)
) %>%
  layout(
    title = "Accidents by State from 2016 to 2022",
    geo = list(scope = 'usa',
               bgcolor = "#c1e8f5"),
    paper_bgcolor = "#c1e8f5",
    plot_bgcolor = "#c1e8f5"
  )


# ______________________________________________________________________________
# Visualización de severidades y de variables ambientales
# ______________________________________________________________________________

ui <- navbarPage(
  "Accident Trends in the US from 2016 to 2022",
  tabPanel("Severity",
           fluidPage(
             useShinyjs(),
             
             titlePanel(div(span("Accident Trends by Severity", style={'padding-left: 55px'})
             )),  
             
             fluidRow(
               style = "padding-left: 55px;",
               column(width = 4,
                      selectInput("stateInput", "Select State:", choices = sort(unique(data$State)))
               ),
               column(width = 4,
                      selectInput("sunriseSunsetInput", "Select Day/Night:", choices = unique(data$Sunrise_Sunset))
               )
             ),
             fluidRow(
               column(width = 8,
                      plotlyOutput("linePlot", height = "100%", width = "150%")
               )
             )
           )
  ),
  
  tabPanel("Environment Variables",
           fluidPage(
             useShinyjs(),
             
             titlePanel(div(span("Accident Trends by Environment Variables", style={'padding-left: 35px'})
             )),
             
             fluidRow(
               fluidRow(
                 style = "padding-left: 50px;",
                 column(width = 2,
                        selectInput("state", "Choose a State:", choices = sort(unique(data$State)))
                 ),
                 column(width = 2,
                        selectInput("severity", label = "Choose a Severity:", 
                                    choices = c("Low", "Moderate", "High", "Very High"),
                                    selected = "Low")
                 ),
                 column(width = 2,
                        selectInput("year", "Choose a Year:", choices = sort(unique(data$Year), decreasing = TRUE))
                 ),
                 column(width = 2,
                        selectInput("variable", "Choose a Variable:", choices = sort(setdiff(names(data)[3:ncol(data)], c("Pressure", "Wind_Chill", "State","Sunrise_Sunset", "Weather_Condition"))))
                 )
               ),
               fluidRow(
                 column(width = 6,
                        plotlyOutput("barPlot", height = "100%", width = "200%")
                 )
               )
             )
           )
  )
)

# Servidor
server <- function(input, output) {
  background_color <- reactiveVal("#FFFFFF")  # Fondo es blanco por default
  
  # Dependiendo del modo, el fondo va a cambiar
  observe({
    if (input$sunriseSunsetInput == "Night") {
      background_color("#2d2d2e")  # Modo oscuro
    } else {
      background_color("#FFFFFF")  # Modo claro
    }
  })
  
  observe({
    shinyjs::runjs(sprintf("document.body.style.backgroundColor = '%s';", background_color()))
    shinyjs::runjs(sprintf("document.body.style.color = '%s';", ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")))
    shinyjs::runjs(sprintf("$('.shiny-title').css('color', '%s');", ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")))
  })
  
  filteredData <- reactive({
    data %>%
      filter(State == input$stateInput, Sunrise_Sunset == input$sunriseSunsetInput) %>%
      complete(Year = seq(min(Year), max(Year), 1)) %>%
      mutate(Year = as.integer(Year)) %>%
      arrange(State, Year)
  })
  
  output$linePlot <- renderPlotly({
    p <- ggplot(filteredData(), aes(x = Year, color = factor(Severity, levels = names(severity_levels), labels = severity_levels))) +
      geom_line(stat = "count") +
      geom_point(stat = "count", size = 2) +
      labs(
        title = paste("Count of Accidents by Severity in", input$stateInput, "over the Years during the", input$sunriseSunsetInput),
        x = 'Year',
        y = 'Count of accidents',
        color = 'Severity'
      ) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = background_color()),
        plot.background = element_rect(fill = background_color()),
        plot.title = element_text(color = ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")),
        axis.title = element_text(color = ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")),
        axis.text = element_text(color = ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")),
        legend.title = element_text(color = ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000")),
        legend.text = element_text(color = ifelse(background_color() == "#2d2d2e", "#FFFFFF", "#000000"))
      ) +
      scale_x_continuous(
        breaks = seq(min(filteredData()$Year), max(filteredData()$Year), 1),
        limits = c(min(filteredData()$Year), max(filteredData()$Year))
      ) +
      scale_color_manual(values = c("Low" = "#1bc210", "Moderate" = "#fae820", "High" = "orange", "Very High" = "red"))
    
    ggplotly(p, config = list(scrollZoom = TRUE, displayModeBar = FALSE))
  })

  
  output$barPlot <- renderPlotly({
    filtered_data <- subset(data, State == input$state)
    filtered_data <- subset(filtered_data, Severity == severity_mapping[input$severity])
    filtered_data <- subset(filtered_data, Year == input$year)
    filtered_data <- arrange(filtered_data, desc(Year))
    
    # Nested color mapping based on both variable and severity
    color_mapping <- list(
      "Low" = list(
        Temperature = "#e8875d",
        Humidity = "#99c9c3",
        Visibility = "#ff88b9",
        Wind_Speed = "#7bbb68",
        Precipitation = "#76aae5"
      ),
      "Moderate" = list(
        Temperature = "#cd693e",
        Humidity = "#7fa7a2",
        Visibility = "#e85d97",
        Wind_Speed = "#4a9c51",
        Precipitation = "#799ec7"
      ),
      "High" = list(
        Temperature = "#af5a35",
        Humidity = "#51827b",
        Visibility = "#c85082",
        Wind_Speed = "#336b38",
        Precipitation = "#6f87a2"
      ),
      "Very High" = list(
        Temperature = "#924a2b",
        Humidity = "#386963",
        Visibility = "#a14069",
        Wind_Speed = "#254e29",
        Precipitation = "#566c83"
      )
    )
    
    gg <- ggplot(filtered_data, aes(x = !!sym(input$variable))) +
      geom_bar(fill = color_mapping[[input$severity]][input$variable]) +
      labs(
        title = paste(input$variable, "during accidents of", input$severity, "Severity in", input$state, "for", input$year),
        x = paste(input$variable, "(", dataUnidades$Unit[dataUnidades$Variable == input$variable], ")"),
        y = "Count of accidents"
      ) +
      theme_minimal() +
      scale_y_continuous(labels = label_number_si())
    
    # Convert to plotly
    p <- ggplotly(gg)
    
    p <- p %>% layout(
      xaxis = list(rangemode = "tozero"),
      yaxis = list(rangemode = "tozero"),
      margin = list(l = 50, r = 20, b = 50, t = 40),
      paper_bgcolor = 'rgba(255,255,255,0.7)',
      plot_bgcolor = 'rgba(255,255,255,0.7)',
      showlegend = FALSE,
      grid = list(color = 'rgba(128,128,128,0.5)')
    )
    
    p
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
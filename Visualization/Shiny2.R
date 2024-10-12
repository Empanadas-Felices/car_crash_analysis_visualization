# Angie Solís y Emilia Víquez

library(dplyr)
library(tidyverse)
library(ggplot2)
library(shiny)
library(shinyjs)
library(rjson)
library(plotly)
library(viridis)
library(usdata)

# ______________________________________________________________________________
# Limpieza de datos
# ______________________________________________________________________________

# Cambio de nombre de columnas
data <- read.csv("filtered_data2016-2019.csv")

# dataOriginal <- dataOriginal %>% rename(Temperature = Temperature.F.)
# dataOriginal <- dataOriginal %>% rename(Wind_Chill = Wind_Chill.F.)
# dataOriginal <- dataOriginal %>% rename(Humidity = Humidity...)
# dataOriginal <- dataOriginal %>% rename(Pressure = Pressure.in.)
# dataOriginal <- dataOriginal %>% rename(Visibility = Visibility.mi.)
# dataOriginal <- dataOriginal %>% rename(Wind_Speed = Wind_Speed.mph.)
# dataOriginal <- dataOriginal %>% rename(Precipitation = Precipitation.in.)

# Selección de columnas deseadas
# data <- dataOriginal %>% select(Start_Time, Severity, State, Temperature, Wind_Chill, Humidity, 
#                                Pressure, Visibility, Wind_Speed, Precipitation, 
#                               Weather_Condition, Sunrise_Sunset)

# Omitir las filas con NA
# data <- na.omit(data)

# Obtener los años
# data$Start_Time <- as.Date(data$Start_Time, format = "%Y-%m-%d")
# data$Start_Time <- format(data$Start_Time, "%Y")
# data$Start_Time <- as.integer(data$Start_Time)
# data <- data[data$Start_Time < 2023, ]  # 2023 is excluded since the year hasn't finished
# data <- data %>% rename(Year = Start_Time) 

data$State <- abbr2state(data$State)


# ______________________________________________________________________________
# Visualización de cantidad de accidentes por variable
# ______________________________________________________________________________

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #FFFFFF; /* Background for the Shiny app */
      }
      .container {
        max-width: 600px; /* Adjust the max-width as needed */
        margin: auto;
        margin-top: 20px;  /* Adjust the top margin as needed */
        text-align: center; /* Center the text within the container */
      }
      .title-center h1, .title-center h3 {
        text-align: center; /* Center the title and header text */
      }
      .plot-container {
        background-color: #FFFFFF; /* White background for the plot container */
        padding: 20px; /* Add padding to the plot container */
        border-radius: 10px; /* Add border-radius for rounded corners */
      }
      #barPlot {
        background-color: #FFFFFF; /* Set the background color of the plot area */
      }
    "))
  ),
  titlePanel(""),
  
  fluidRow(
    column(
      width = 6,
      offset = 3,
      h1("Interactive Visualization"),
      h3("Choose options:"),
      fluidRow(
        column(width = 4,
               selectInput("year", "Choose a Year:", choices = sort(unique(data$Year), decreasing = TRUE))
        ),
        column(width = 4,
               selectInput("variable", "Choose a Variable:", choices = sort(setdiff(names(data)[3:ncol(data)], c("State","Sunrise_Sunset", "Weather_Condition"))))
        ),
        column(width = 4,
               selectInput("state", "Choose a State:", choices = sort(unique(data$State)))
        )
      ),
      div(id = "barPlot", class = "plot-container",
          plotlyOutput("barPlot", height = 400, width = "150%")
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$barPlot <- renderPlotly({
    # Filter data based on entered state and year
    filtered_data <- subset(data, State == input$state)
    
    # Arrange the data by Year in descending order
    filtered_data <- arrange(filtered_data, desc(Year))
    
    # Create a mapping of colors for each variable
    color_mapping <- c(
      Temperature = "#e8875d",
      Wind_Chill = "#b8c99f",
      Humidity = "#99c9c3",
      Pressure = "#e85d97",
      Visibility = "#e0d484",
      Wind_Speed = "#336b38",
      Precipitation = "#799ec7"
    )
    
    # Create a bar plot for the selected variable by count
    gg <- ggplot(filtered_data, aes(x = !!sym(input$variable))) +
      geom_bar(fill = color_mapping[input$variable]) +  # Set the fill color of the bars
      labs(title = paste("Count of", input$variable, "in", input$state, "for Year", input$year),
           x = input$variable,
           y = "Count") +
      theme_minimal()  # Use minimal theme to have gray gridlines
    
    # Convert ggplot to plotly for zooming capabilities
    p <- ggplotly(gg)
    
    # Configure the layout for adaptive axes and white background
    p <- p %>% layout(
      xaxis = list(rangemode = "tozero"),
      yaxis = list(rangemode = "tozero"),
      margin = list(l = 50, r = 20, b = 50, t = 40),  # Adjust margins as needed
      paper_bgcolor = 'rgba(255,255,255,0.7)',  # Set the background color of the plot area
      plot_bgcolor = 'rgba(255,255,255,0.7)',   # Set the background color of the entire plot
      showlegend = FALSE,  # Hide the legend
      grid = list(color = 'rgba(128,128,128,0.5)')  # Set the color of the gridlines
    )
    
    p
  })
}

# Run the application
shinyApp(ui = ui, server = server)


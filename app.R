library(tidyverse)
library(shiny)
library(bslib)

data <- read_csv("data.csv")

ui <- fluidPage(
  theme = bs_theme(primary = "#ADD8E6", 
                   secondary = "#FFEBCD", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "spacelab"),

  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "disease", # to use in code
                  label = "Disease:", # how it looks in UI
                  choices = c("asthma_er_avg", "cancer_incidence_rate")
      ),
      selectInput(inputId = "pollutant", # to use in code
                  label = "Pollutant:", # how it looks in UI
                  choices = c("annual_mean_co", "annual_mean_no2", "annual_mean_ozone", "annual_mean_pb", "annual_mean_pm25", "annual_mean_pm10", "annual_mean_so2")
      ),
      selectInput(inputId = "variable", # to use in code
                  label = "Variable:", # how it looks in UI
                  choices = c("asthma_er_avg", "cancer_incidence_rate", "annual_mean_co", "annual_mean_no2", "annual_mean_ozone", "annual_mean_pb", "annual_mean_pm25", "annual_mean_pm10", "annual_mean_so2")
      )
      
    ),

    mainPanel(
      plotOutput(outputId = "line_graph"),
      plotOutput(outputId = "density_plot")
    )
  )
)

server <- function(input, output) {
  
  output$line_graph <- renderPlot({
    disease <- input$disease
    pollutant <- input$pollutant
    data %>% 
      filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[pollutant]], y = .data[[disease]])) +
      geom_point() +
      geom_smooth(method = "lm") +
      labs(title = "disease vs pollutant")
  })
  
  output$density_plot <- renderPlot({
    variable <- input$variable
    data %>% 
      filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[variable]])) +
      geom_density()
  })
}

shinyApp(ui = ui, server = server)
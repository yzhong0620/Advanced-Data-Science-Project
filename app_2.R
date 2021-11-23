library(tidyverse)
library(shiny)
library(bslib)

data <- read_csv("data_standardized.csv")

quant_vars <- data %>% 
  select(-County) %>% 
  names() %>% 
  sort()

predictors <- data %>% 
  select(-County, -Year) %>% 
  select(annual_mean_co, annual_mean_no2, annual_mean_ozone, annual_mean_pb, 
         annual_mean_pm25, annual_mean_pm10, annual_mean_so2, hpi2score, economic, education, housing,
         healthcareaccess, neighborhood, pollution, transportation, social, insured, uncrowded, homeownership, 
         automobile, commute, inpreschool, inhighschool, bachelorsed, employed, abovepoverty, income, tree_canopy,
         supermarkets, park_access, h2o_contam) %>% 
  names() %>% 
  sort()

outcomes <- data %>% 
  select(-County, -Year, -annual_mean_co, -annual_mean_no2, -annual_mean_ozone, -annual_mean_pb,
         -annual_mean_pm25, -annual_mean_pm10, -annual_mean_so2, -hpi2score, -economic, -education, -housing,
         -healthcareaccess, -neighborhood, -pollution, -transportation, -social, -insured, -uncrowded, -homeownership,
          -automobile, -commute, -inpreschool, -inhighschool, -bachelorsed, -employed, -abovepoverty, -income, -tree_canopy,
         -supermarkets, -park_access, -h2o_contam) %>%
  names() %>% 
  sort()

ui <- fluidPage(
  theme = bs_theme(primary = "#ADD8E6", 
                   secondary = "#FFEBCD", 
                   base_font = list(font_google("Raleway"), "-apple-system", 
                                    "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    "Segoe UI Symbol"), 
                   bootswatch = "cosmo"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "disease", # to use in code
                  label = "Predictor:", # how it looks in UI
                  choices = predictors
      ),
      selectInput(inputId = "pollutant", # to use in code
                  label = "Outcome (for scatterplot):", # how it looks in UI
                  choices = outcomes
      ),
      selectInput(inputId = "variable", # to use in code
                  label = "Outcome (for density plot):", # how it looks in UI
                  choices = outcomes
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
     # filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[disease]], y = .data[[pollutant]])) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_minimal() +
      labs(title = "disease vs pollutant")
  })
  
  output$density_plot <- renderPlot({
    variable <- input$variable
    data %>% 
      #filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[variable]])) +
      geom_density() +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
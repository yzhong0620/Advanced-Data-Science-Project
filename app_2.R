library(tidyverse)
library(shiny)
library(bslib)
library(ggplot2)
library(usmap)

data <- read_csv("data_standardized.csv") %>% 
  mutate(fips = FIPS) %>% 
  select(-FIPS)

coordinates <- read_csv("county_coordinates.csv")

data_with_coordinates <- data %>% 
  left_join(coordinates, by = "County")

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

years <- c(2017, 2018, 2019)

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
      selectInput(inputId = "year", # to use in code
                  label = "Year (for map):", # how it looks in UI
                  choices = years
      )
      
    ),
    
    mainPanel(
      plotOutput(outputId = "line_graph"),
      plotOutput(outputId = "density_plot"),
      plotOutput(outputId = "map")
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
    pollutant <- input$pollutant
    data %>% 
      #filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[pollutant]])) +
      geom_density() +
      theme_minimal()
  })
  
  output$map <- renderPlot({
   # year <- input$year
    #disease <- input$disease
    #pollutant <- input$pollutant
   data_year <- data %>% 
      filter(Year == input$year)
    
     #mutate(outcome_variable = .data[[disease]]) %>% 
      plot_usmap(regions = "counties", include = "CA", data = data_2017, values = input$disease) + 
      labs(title = "US Counties",
           subtitle = "This map is supposed to have points but isn't working currently.") + 
      theme(legend.position = "right") +
      scale_fill_continuous(
        low = "lightblue", high = "navy", name = "Heart Disease Deaths \nper Person (2017)", label = scales::comma
      ) 
  })
}

shinyApp(ui = ui, server = server)
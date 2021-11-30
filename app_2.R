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


#reorder column order for coordinates
col_order <- c("Longitude", "Latitude")

coordinates3 <- coordinates %>% 
  mutate(Longitude = Longitude * -1) %>% 
  select(-County)

coordinates2 <- coordinates3[, col_order]

transformed_data <- usmap_transform(coordinates2) 

transformed_data_with_data <- data_with_coordinates %>% 
  left_join(transformed_data, by = "Latitude")


#trying to transform data for points like some people say you need to but still doesn't work
transformed_data <- usmap_transform(coordinates2) 

transformed_data_with_data <- data_with_coordinates %>% 
  left_join(transformed_data, by = "Latitude")

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
  
      selectInput(inputId = "predictor", # to use in code
                  label = "Predictor:", # how it looks in UI
                  choices = predictors
      ),
      selectInput(inputId = "outcome", # to use in code
                  label = "Outcome (for scatterplot):", # how it looks in UI
                  choices = outcomes
      ),
      selectInput(inputId = "year", # to use in code
                  label = "Year (for map):", # how it looks in UI
                  choices = years
      ),
      submitButton(text = "Create plots")
      ), 
    mainPanel(
      tabsetPanel(tabPanel("Line Graph", plotOutput("line_graph")),
                  tabPanel("Density Plot", plotOutput("density_plot")),
                  tabPanel("Map", plotOutput("map"))
      )
    )
    )
)

server <- function(input, output) {
  
  output$line_graph <- renderPlot({
    predictor <- input$predictor
    outcome <- input$outcome
    data %>% 
     # filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[predictor]], y = .data[[outcome]])) +
      geom_point() +
      geom_smooth(method = "lm") +
      theme_minimal() +
      labs(title = "predictor vs outcome")
  })
  
  output$density_plot <- renderPlot({
    outcome <- input$outcome
    data %>% 
      #filter(!(COUNTY == "California")) %>% 
      ggplot(aes(x = .data[[outcome]])) +
      geom_density() +
      theme_minimal()
  })
  
  output$map <- renderPlot({
   data_year <- data %>% 
      filter(Year == input$year)
    
     #mutate(outcome_variable = .data[[disease]]) %>% 
      plot_usmap(regions = "counties", include = "CA", data = data_year, values = input$outcome) + 
      labs(title = "US Counties",
           subtitle = "This map is supposed to have points but isn't working currently.") + 
      theme(legend.position = "right") +
      scale_fill_continuous(
        low = "lightblue", high = "navy", name = "Outcome in selected year", label = scales::comma
      ) + 
        geom_point(data = transformed_data_with_data, aes(x = Longitude.1, y = Latitude.1, size = .data[[input$predictor]]),
                   color = "red", alpha = 0.25) 
  })
}

shinyApp(ui = ui, server = server)
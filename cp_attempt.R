library(shiny)       # for app
library(tidyverse)   # for plotting and wrangling
library(tidymodels)  # for modeling
library(ranger)      # for random forest
library(bslib)       # for theming

data <- read_csv("data_standardized.csv") 

model <- read_rds("asthma_model_final.rds")

#create a table with min, max, and median of each variable
stats_num <-
  data  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))

ui <- fluidPage(
  
  # Application title
  titlePanel("Asthma ER Visits Ceteris Paribus"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      # scrollable side panel:
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 600px; overflow-y: auto; }'
      )),
      
      
      
      #number of accounts delinquent
      sliderInput(inputId = "annual_mean_co",
                  label = "Number of accounts delinquent:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_co") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_co") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_co") %>% 
                    pull(med_val), 
                  step = .01, 
                  round = TRUE),
      
      #balance to credit limit
      sliderInput(inputId = "annual_mean_no2",
                  label = "annual_mean_no2:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_no2") %>% 
                    pull(min_val) %>% 
                    #round(digits = -1),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_no2") %>% 
                    pull(max_val) %>% 
                    #round(digits = -1),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_no2") %>% 
                    pull(med_val) %>% 
                   # round(digits = -1), 
                  step = .01),
      
      #annual income
      sliderInput(inputId = "annual_mean_ozone",
                  label = "annual_mean_ozone:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_ozone") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_ozone") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_ozone") %>% 
                    pull(med_val), 
                  step = .01),
      
      #annual_mean_pb
      sliderInput(inputId = "annual_mean_pb",
                  label = "annual_mean_pb:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_pb") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_pb") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_pb") %>% 
                    pull(med_val), 
                  step = .01, 
                  round = TRUE),
      
      #Past due amount owed
      sliderInput(inputId = "annual_mean_pm25",
                  label = "annual_mean_pm25:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_pm25") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_pm25") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_pm25") %>% 
                    pull(med_val), 
                  step = .01, 
                  round = TRUE),
      
      #The total amount committed to that loan at that point in time.
      sliderInput(inputId = "annual_mean_pm10",
                  label = "annual_mean_pm10:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_pm10") %>% 
                    pull(min_val) %>% 
                   # round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_pm10") %>% 
                    pull(max_val) %>% 
                   # round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_pm10") %>% 
                    pull(med_val) %>% 
                   # round(digits = -3), 
                  step = 0.01),
      
      #Number of personal finance inquiries
      sliderInput(inputId = "annual_mean_so2",
                  label = "annual_mean_so2:",
                  min = stats_num %>% 
                    filter(variable =="annual_mean_so2") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_mean_so2") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_mean_so2") %>% 
                    pull(med_val), 
                  step = .01, 
                  round = TRUE),
      
      #Number of credit inquiries in past 12 months
      sliderInput(inputId = "hpi2score",
                  label = "hpi2score:",
                  min = stats_num %>% 
                    filter(variable =="hpi2score") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="hpi2score") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="hpi2score") %>% 
                    pull(med_val), 
                  step = .1, 
                  round = TRUE),
      
  
      
      #The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
      sliderInput(inputId = "economic",
                  label = "economic:",
                  min = stats_num %>% 
                    filter(variable =="economic") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="economic") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="economic") %>% 
                    pull(med_val), 
                  step = .01, 
                  round = TRUE),
      
      #Number of installment accounts
      sliderInput(inputId = "education",
                  label = "Neducations:",
                  min = stats_num %>% 
                    filter(variable =="education") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="education") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="education") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of installment accounts opened in past 12 months
      sliderInput(inputId = "housing",
                  label = "Number of installment accounts opened in past 12 months:",
                  min = stats_num %>% 
                    filter(variable =="housing") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="housing") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="housing") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of installment accounts opened in past 24 months
      sliderInput(inputId = "healthcareaccess",
                  label = "healthcareaccess:",
                  min = stats_num %>% 
                    filter(variable =="healthcareaccess") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="healthcareaccess") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="healthcareaccess") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #number of installment accounts opened in past 6 months
      sliderInput(inputId = "neighborhood",
                  label = "Number of installment accounts opened in past 6 months:",
                  min = stats_num %>% 
                    filter(variable =="neighborhood") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="neighborhood") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="neighborhood") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),

      
      #Revolving line utilization rate
      sliderInput(inputId = "pollution",
                  label = "Revolving line utilization rate:",
                  min = stats_num %>% 
                    filter(variable =="pollution") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="pollution") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="pollution") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #loan interest rate
      sliderInput(inputId = "transportation",
                  label = "Loan transportation rate:",
                  min = stats_num %>% 
                    filter(variable =="transportation") %>% 
                    pull(min_val)%>% 
                    #round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="transportation") %>% 
                    pull(max_val)%>% 
                    #round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="transportation") %>% 
                    pull(med_val)%>% 
                   # round(digits = 1), 
                  step = .1, 
                  round = TRUE),
      
      #total current balance of installment accounts
      sliderInput(inputId = "social",
                  label = "socials:",
                  min = stats_num %>% 
                    filter(variable =="social") %>% 
                    pull(min_val) %>% 
                   # round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="social") %>% 
                    pull(max_val) %>% 
                    #round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="social") %>% 
                    pull(med_val) %>% 
                    #round(digits = -4), 
                  step = 1),
      
      #Total installments high credit/credit limit
      sliderInput(inputId = "insured",
                  label = "insured:",
                  min = stats_num %>% 
                    filter(variable =="insured") %>% 
                    pull(min_val) %>% 
                   # round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="insured") %>% 
                    pull(max_val) %>% 
                   # round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="insured") %>% 
                    pull(med_val) %>% 
                  #  round(digits = -3), 
                  step = 1),
      
      sliderInput(inputId = "uncrowded",
                  label = "uncrowded:",
                  min = stats_num %>% 
                    filter(variable =="uncrowded") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="uncrowded") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="uncrowded") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "homeownership",
                  label = "homeownership:",
                  min = stats_num %>% 
                    filter(variable =="homeownership") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="homeownership") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="homeownership") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "automobile",
                  label = "automobile:",
                  min = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),   
      
      sliderInput(inputId = "automobile",
                  label = "automobile:",
                  min = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="automobile") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "commute",
                  label = "commute:",
                  min = stats_num %>% 
                    filter(variable =="commute") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="commute") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="commute") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "inpreschool",
                  label = "inpreschool:",
                  min = stats_num %>% 
                    filter(variable =="inpreschool") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="inpreschool") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="inpreschool") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "inhighschool",
                  label = "inhighschool:",
                  min = stats_num %>% 
                    filter(variable =="inhighschool") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="inhighschool") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="inhighschool") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "bachelorsed",
                  label = "bachelorsed:",
                  min = stats_num %>% 
                    filter(variable =="bachelorsed") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="bachelorsed") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="bachelorsed") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "employed",
                  label = "employed:",
                  min = stats_num %>% 
                    filter(variable =="employed") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="employed") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="employed") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "abovepoverty",
                  label = "abovepoverty:",
                  min = stats_num %>% 
                    filter(variable =="abovepoverty") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="abovepoverty") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="abovepoverty") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "income",
                  label = "income:",
                  min = stats_num %>% 
                    filter(variable =="income") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="income") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="income") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "tree_canopy",
                  label = "tree_canopy:",
                  min = stats_num %>% 
                    filter(variable =="tree_canopy") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="tree_canopy") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="tree_canopy") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "supermarkets",
                  label = "supermarkets:",
                  min = stats_num %>% 
                    filter(variable =="supermarkets") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="supermarkets") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="supermarkets") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = .1),
      
      sliderInput(inputId = "park_access",
                  label = "park_access:",
                  min = stats_num %>% 
                    filter(variable =="park_access") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="park_access") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="park_access") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      sliderInput(inputId = "h2o_contam",
                  label = "h2o_contam:",
                  min = stats_num %>% 
                    filter(variable =="h2o_contam") %>% 
                    pull(min_val) %>% 
                    # round(digits = -3),
                    max = stats_num %>% 
                    filter(variable =="h2o_contam") %>% 
                    pull(max_val) %>% 
                    # round(digits = -3),
                    value = stats_num %>% 
                    filter(variable =="h2o_contam") %>% 
                    pull(med_val) %>% 
                    #  round(digits = -3), 
                    step = 1),
      
      
      submitButton(text = "Create the CP profile"),
    ),
    
    # Show the plot
    mainPanel(     
      plotOutput("cp_plot")
    )
  )
)

# create the line graph
server <- function(input, output) {output$cp_plot <- renderPlot({
  
  
  select(annual_mean_co, annual_mean_no2, annual_mean_ozone, annual_mean_pb, 
         annual_mean_pm25, annual_mean_pm10, annual_mean_so2, hpi2score, economic, education, housing,
         healthcareaccess, neighborhood, pollution, transportation, social, insured, uncrowded, homeownership, 
         automobile, commute, inpreschool, inhighschool, bachelorsed, employed, abovepoverty, income, tree_canopy,
         supermarkets, park_access, h2o_contam)
  
  obs <- tibble(annual_mean_co = input$annual_mean_co,
                annual_mean_no2 = input$annual_mean_no2, 
                annual_mean_ozone = input$annual_mean_ozone, 
                annual_mean_pb = input$annual_mean_pb, 
                annual_mean_pm25 = input$annual_mean_pm25,
                annual_mean_pm10 = input$annual_mean_pm10, 
                annual_mean_so2 = input$annual_mean_so2, 
                hpi2score = input$hpi2score, 
                economic = input$economic, 
                education = input$education, 
                housing = input$housing,
                healthcareaccess = input$healthcareaccess,
                neighborhood = input$neighborhood, 
                pollution = input$pollution, 
                transportation = input$transportation, 
                social = input$social, 
                insured = input$insured, 
                uncrowded = input$uncrowded, 
                homeownership = input$homeownership, 
                automobile = input$automobile, 
                commute = input$commute, 
                inpreschool = input$inpreschool, 
                inhighschool = input$inhighschool, 
                bachelorsed = input$bachelorsed, 
                employed = input$employed, 
                abovepoverty = input$abovepoverty, 
                income = input$income, 
                tree_canopy = input$tree_canopy,
                supermarkets = input$supermarkets, 
                park_access = input$park_access, 
                h2o_contam = input$h2o_contam)
  
  #observation
  obs_many <- obs %>% 
    sample_n(size = 50, replace = TRUE) %>% 
    select(-.data[[input$var]]) %>% #remove variable determined for the x axis
    mutate(var_of_interest = seq(stats_num %>% 
                                   filter(variable == input$var) %>% #rename x axis var as variable
                                   pull(min_val), 
                                 stats_num %>% 
                                   filter(variable == input$var) %>% 
                                   pull(max_val), 
                                 length.out = 50)) %>% 
    set_names(ifelse(names(.) == "var_of_interest", 
                     input$var, 
                     names(.)))
  
  #make funded amount an integer
  # if (input$var == "funded_amnt") {
  #   obs_many %>% 
  #     select(.data[[input$var]]) %>% 
  #     bind_cols(
  #       predict(lending_mod,
  #               new_data = obs_many %>% 
  #                 mutate(across(.data[[input$var]], 
  #                               ~as.integer(round(.x)))), 
  #               type = "prob")
  #     ) %>% 
  #     ggplot(aes(x = .data[[input$var]],
  #                y = .pred_good)) +
  #     geom_line() +
  #     labs(title = "Predicted probability of loan fully paid back \nor currently on-time",
  #          y = NULL) +
  #     theme_minimal()
  # } else {
  #   obs_many %>% 
  #     select(.data[[input$var]]) %>% 
  #     bind_cols(
  #       predict(lending_mod,
  #               new_data = obs_many, 
  #               type = "prob")
  #     ) %>% 
  #     ggplot(aes(x = .data[[input$var]],
  #                y = .pred_good)) +
  #     geom_line() +
  #     labs(title = "Predicted probability of loan fully paid back or currently on-time",
  #          y = NULL) +
  #     theme_minimal()
  # }
})
}
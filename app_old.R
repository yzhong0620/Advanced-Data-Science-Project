
library(shiny)       # for app
library(tidyverse)   # for plotting and wrangling
library(tidymodels)  # for modeling
library(ranger)      # for random forest
library(bslib)       # for theming

data("lending_club")


# Load in model created in Assignment_05 (separate github repo)
lending_mod <- read_rds("lending_rf_final.rds")

# create an alphabetized list of states
states <- 
  lending_club  %>% 
  select(addr_state) %>% 
  distinct(addr_state) %>% 
  arrange(addr_state) %>% 
  pull(addr_state)

# modify employment length
emp_len <- 
  lending_club %>% 
  select(emp_length) %>% 
  distinct() %>% 
  mutate(emp_length = fct_relevel(emp_length, "emp_ge_10", after = 10)) %>% 
  arrange(emp_length) %>% 
  pull(emp_length)


#create a table with min, max, and median of each variable
stats_num <-
  lending_club  %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(cols = everything(),
               names_to = "variable", 
               values_to = "value") %>% 
  group_by(variable) %>% 
  summarize(min_val = min(value),
            max_val = max(value),
            med_val = median(value))



ui <- fluidPage(
  theme = bs_theme(#primary = "#123B60", 
                  # secondary = "#D44420", 
                  # base_font = list(font_google("Raleway"), "-apple-system", 
                                    # "BlinkMacSystemFont", "Segoe UI", "Helvetica Neue", "Arial", 
                                    # "sans-serif", "Apple Color Emoji", "Segoe UI Emoji", 
                                    # "Segoe UI Symbol"), 
                   bootswatch = "cerulean"),
  
  # Application title
  titlePanel("Lending Data Ceteris Paribus"),
  
  # Sidebar with inputs
  sidebarLayout(
    sidebarPanel(
      # scrollable side panel:
      tags$head(tags$style(
        type = 'text/css',
        'form.well { max-height: 600px; overflow-y: auto; }'
      )),
      
      #number of accounts delinquent
      sliderInput(inputId = "acc_now_delinq",
                  label = "Number of accounts delinquent:",
                  min = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="acc_now_delinq") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #balance to credit limit
      sliderInput(inputId = "all_util",
                  label = "Balance to credit limit on all trades:",
                  min = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(min_val) %>% 
                    round(digits = -1),
                  max = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(max_val) %>% 
                    round(digits = -1),
                  value = stats_num %>% 
                    filter(variable =="all_util") %>% 
                    pull(med_val) %>% 
                    round(digits = -1), 
                  step = 10),
      
      #annual income
      sliderInput(inputId = "annual_inc",
                  label = "Annual income:",
                  min = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="annual_inc") %>% 
                    pull(med_val), 
                  step = 1000),
      
      #delinq_2yrs
      sliderInput(inputId = "delinq_2yrs",
                  label = "Number of 30+ days past-due incideces for past 2 yrs:",
                  min = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="delinq_2yrs") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Past due amount owed
      sliderInput(inputId = "delinq_amnt",
                  label = "Past due amount owed:",
                  min = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="delinq_amnt") %>% 
                    pull(med_val), 
                  step = 1000, 
                  round = TRUE),
      
      #The total amount committed to that loan at that point in time.
      sliderInput(inputId = "funded_amnt",
                  label = "Total amount committed to that loan at that point in time:",
                  min = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(min_val) %>% 
                    round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(max_val) %>% 
                    round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="funded_amnt") %>% 
                    pull(med_val) %>% 
                    round(digits = -3), 
                  step = 1000),
      
      #Number of personal finance inquiries
      sliderInput(inputId = "inq_fi",
                  label = "Number of personal finance inquiries:",
                  min = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_fi") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of credit inquiries in past 12 months
      sliderInput(inputId = "inq_last_12m",
                  label = "Number of credit inquiries in past 12 months:",
                  min = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #The number of inquiries in past 6 months (excluding auto and mortgage inquiries)
      sliderInput(inputId = "inq_last_6mths",
                  label = "The number of inquiries in past 6 months (excluding auto and mortgage inquiries):",
                  min = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="inq_last_6mths") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of installment accounts
      sliderInput(inputId = "num_il_tl",
                  label = "Number of installment accounts:",
                  min = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="num_il_tl") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of installment accounts opened in past 12 months
      sliderInput(inputId = "open_il_12m",
                  label = "Number of installment accounts opened in past 12 months:",
                  min = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_12m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Number of installment accounts opened in past 24 months
      sliderInput(inputId = "open_il_24m",
                  label = "Number of installment accounts opened in past 24 months:",
                  min = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_24m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #number of installment accounts opened in past 6 months
      sliderInput(inputId = "open_il_6m",
                  label = "Number of installment accounts opened in past 6 months:",
                  min = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="open_il_6m") %>% 
                    pull(med_val), 
                  step = 1, 
                  round = TRUE),
      
      #Revolving line utilization rate
      sliderInput(inputId = "revol_util",
                  label = "Revolving line utilization rate:",
                  min = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(min_val),
                  max = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(max_val),
                  value = stats_num %>% 
                    filter(variable =="revol_util") %>% 
                    pull(med_val), 
                  step = 2, 
                  round = TRUE),
      
      #loan interest rate
      sliderInput(inputId = "int_rate",
                  label = "Loan interest rate:",
                  min = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(min_val)%>% 
                    round(digits = 1),
                  max = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(max_val)%>% 
                    round(digits = 1),
                  value = stats_num %>% 
                    filter(variable =="int_rate") %>% 
                    pull(med_val)%>% 
                    round(digits = 1), 
                  step = .1, 
                  round = TRUE),
      
      #total current balance of installment accounts
      sliderInput(inputId = "total_bal_il",
                  label = "Total current balance of installment accts:",
                  min = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(min_val) %>% 
                    round(digits = -4),
                  max = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(max_val) %>% 
                    round(digits = -4),
                  value = stats_num %>% 
                    filter(variable =="total_bal_il") %>% 
                    pull(med_val) %>% 
                    round(digits = -4), 
                  step = 10000),
      
      #Total installments high credit/credit limit
      sliderInput(inputId = "total_il_high_credit_limit",
                  label = "Total installments high credit/credit limit:",
                  min = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(min_val) %>% 
                    round(digits = -3),
                  max = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(max_val) %>% 
                    round(digits = -3),
                  value = stats_num %>% 
                    filter(variable =="total_il_high_credit_limit") %>% 
                    pull(med_val) %>% 
                    round(digits = -3), 
                  step = 1000),
      
      #state
      selectInput(inputId = "addr_state", 
                  label = "State:", 
                  choices = states),
      
      #employment length
      selectInput(inputId = "emp_length", 
                  label = "Employment length:", 
                  choices = emp_len),
      
      #LC assigned loan subgrade
      selectInput(inputId = "sub_grade", 
                  label = "LC assigned loan subgrade:", 
                  choices = lending_club %>% 
                    select(sub_grade) %>% 
                    distinct() %>% 
                    arrange(sub_grade) %>% 
                    pull(sub_grade)),
      selectInput(inputId = "term", 
                  label = "Term:", 
                  choices = lending_club %>% 
                    select(term) %>% 
                    distinct() %>% 
                    arrange(term) %>% 
                    pull(term)),
      selectInput(inputId = "verification_status", 
                  label = "Verification Status:", 
                  choices = lending_club %>% 
                    select(verification_status) %>% 
                    distinct() %>% 
                    arrange(verification_status) %>% 
                    pull(verification_status)),
      selectInput(inputId = "var",
                  label = "Variable to vary in the plot: (x-axis)",
                  choices = list(Income = "annual_inc",
                                 `Interest rate` =  "int_rate",
                                 `Funded amount` = "funded_amnt")),
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
  
  
  #variable options
  obs <- tibble(funded_amnt = input$funded_amnt, 
                term = input$term, 
                int_rate = input$int_rate, 
                sub_grade = input$sub_grade, 
                addr_state = input$addr_state, 
                verification_status = input$verification_status, 
                annual_inc = input$annual_inc, 
                emp_length = input$emp_length, 
                delinq_2yrs = input$delinq_2yrs, 
                inq_last_6mths = input$inq_last_6mths, 
                revol_util = input$revol_util, 
                acc_now_delinq = input$acc_now_delinq, 
                open_il_6m = input$open_il_6m, 
                open_il_12m = input$open_il_12m, 
                open_il_24m = input$open_il_24m, 
                total_bal_il = input$total_bal_il, 
                all_util = input$all_util, 
                inq_fi = input$inq_fi, 
                inq_last_12m = input$inq_last_12m, 
                delinq_amnt = input$delinq_amnt,
                num_il_tl = input$num_il_tl, 
                total_il_high_credit_limit = input$total_il_high_credit_limit)
  
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
  if (input$var == "funded_amnt") {
    obs_many %>% 
      select(.data[[input$var]]) %>% 
      bind_cols(
        predict(lending_mod,
                new_data = obs_many %>% 
                  mutate(across(.data[[input$var]], 
                                ~as.integer(round(.x)))), 
                type = "prob")
      ) %>% 
      ggplot(aes(x = .data[[input$var]],
                 y = .pred_good)) +
      geom_line() +
      labs(title = "Predicted probability of loan fully paid back \nor currently on-time",
           y = NULL) +
      theme_minimal()
  } else {
    obs_many %>% 
      select(.data[[input$var]]) %>% 
      bind_cols(
        predict(lending_mod,
                new_data = obs_many, 
                type = "prob")
      ) %>% 
      ggplot(aes(x = .data[[input$var]],
                 y = .pred_good)) +
      geom_line() +
      labs(title = "Predicted probability of loan fully paid back or currently on-time",
           y = NULL) +
      theme_minimal()
  }
})
}


# Run the application 
shinyApp(ui = ui, server = server)
# Load libraries
library(shiny)
library(tidyverse)

# Application Layout
shinyUI(
  fluidPage(
    br(),
    # TASK 1: Application title
    titlePanel(title = "Trends in Demographics and Income"),
    p("Explore the difference between people who earn less than 50K and more than 50K. You can filter the data by country, then explore various demogrphic information."),
    
    # TASK 2: Add first fluidRow to select input for country
    fluidRow(
      column(12, 
             wellPanel(selectInput(inputId = "country",
                                   label = "Select the country of interest",
                                   choices = c("United-States", "Canada", "Mexico", "Germany", "Philippines"),
                                   selected = "United-states"
                                   ))  # add select input 
             )
    ),
    
    # TASK 3: Add second fluidRow to control how to plot the continuous variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a continuous variable and graph type (histogram or boxplot) to view on the right"),
               radioButtons(inputId = "continuous_variable",
                            label = "Continuous:",
                            choices = c("age", "hours_per_week"),
                            selected = "age"),
               radioButtons(inputId = "graph_type",
                            label = "Graph: ",
                            choices = c("histogram", "boxplot"),
                            selected = "histogram"),
               sliderInput(inputId = "bins",
                           label = "Select number of bins",
                           min = 10,
                           max = 50,
                           value = 30))),
      column(9,
             plotOutput("p1"))  # add plot output
    ),
    
    # TASK 4: Add third fluidRow to control how to plot the categorical variables
    fluidRow(
      column(3, 
             wellPanel(
               p("Select a categorical variable to view bar chat on the right. Use the checkbox to view a stacked bar chart to combine the income levels into one graph"),
               radioButtons(inputId = "categorical_variable",
                            label = "Categorical:",
                            choices = c("education", "workclass", "sex"),
                            selected = "workclass"),
               checkboxInput(inputId = "is_stacked",
                             label = "Stack bars",
                             value = T))),
      column(9,
             plotOutput("p2")), # add plot output
      h6("This plot was created by Lawrence", br(),
        "Data source: IBM Data visualization team")
    )))

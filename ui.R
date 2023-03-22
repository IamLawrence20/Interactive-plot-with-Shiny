gapminder::gapminder
library(shiny)
library(gapminder)
library(tidyverse)

gapminder <- gapminder::gapminder
countries <- unique(gapminder$country)
con <- countries[1:5]
con
shinyUI(

  fluidPage(
    titlePanel(title = "Visualization of the Gapminder dataset"),
    h6("This dataset is available on Gapminder.com"),
    
    fluidRow(
      column(10,
             wellPanel(
               selectInput(inputId = "country",
                           label = "Select your country of interest",
                           choices = countries,
                           selected = "Nigeria")
             )),
      column(3,
             wellPanel(
               varSelectInput(inputId = "x_axis",
                              label = "Select the variable on x-axis",
                              data = gapminder[3:6],
                              selected = "year"),
               varSelectInput(inputId = "y_axis",
                              label = "Select the variable on y-axis",
                              data = gapminder[3:6],
                              selected = "pop"),
               radioButtons(inputId = "graph",
                            label = "Select preferred plot type",
                            choices = c("Scatter plot", "Line graph")),
               selectInput(inputId = "colour",
                           label = "Select preffered plot colour",
                           choices = c("red", "blue", "black", "green", "yellow", "grey")),
               h5("year = ranges from 1952 to 2007 in increments of 5 years", br(),
                  "lifeExp = life expectancy at birth, in years", br(),
                  "pop = Population", br(),
                  "gdpPercap = GDP per capita (US$. inflation-adjusted)")
               )),
      column(7,
             plotOutput("p1"))
    )
  )
)

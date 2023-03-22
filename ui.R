
# Loading required packages -----------------------------------------------

library(tidyverse)
library(shiny)
library(leaflet)


# Reading data ------------------------------------------------------------
Election_2023 <- read.csv("Election_2023_2.csv")
elect <- Election_2023
elect_2 <- Election_2023


# For map -----------------------------------------------------------------
party <- function(df, x, y, z){
  df %>% 
    mutate(
      winner = case_when(
        {{x}} > {{y}} & {{x}} > {{z}} ~ "red",
        {{y}} > {{x}} & {{y}} > {{z}} ~ "blue",
        {{z}} > {{x}} & {{z}} > {{y}} ~ "green")
    )
}

elect_m <- Election_2023
elect_m2 <- elect_m %>%
  party(elect$APC, elect$PDP, elect$LP)

elect_m3 <- elect_m %>%
  party(elect$APC, elect$PDP, elect$LP) %>% 
  mutate(
    ran = sample(c("a", "b", "c"), 38, replace = T)
  )


elect_m3 <- elect_m3 %>% 
  pivot_longer(
    cols = c(winner, Region, ran),
    names_to = "name",
    values_to = "value",
    values_drop_na = T
  ) %>% 
  arrange(State)
elect_col <- elect_m3[1:4]


elect_clean <- elect_m2 %>% 
  pivot_longer(
    cols = !c(State, Region, lat, lon, winner),
    names_to = "party",
    values_to = "votes",
    values_drop_na = T
  ) %>% 
  arrange(State)
# end of script---------------------------------------------------------------------

all_states <- elect_2[1:37,]
all_states <- all_states %>% 
  pivot_longer(
    cols = !c(State, Region),
    names_to = "party",
    values_to = "votes",
    values_drop_na = T
  ) %>% 
  arrange(State)


elect <- elect %>% 
  pivot_longer(
    cols = !c(State, Region),
    names_to = "party",
    values_to = "votes",
    values_drop_na = T
  ) %>% 
  arrange(State)
party <- elect$party
votes <- elect$votes
shinyUI(
  fluidPage(
    titlePanel("Results of the 2023 Presidential Election in Nigeria"),
    p("This is a graphical representation of the 2023 election in Nigeria"),
    h6("The results cover the top three candidates from the three leading political parties"),
    sidebarLayout(
      sidebarPanel(
        wellPanel(
          selectInput(inputId = "state",
                      label = "Select your state of interest",
                      choices = unique(elect$State))
        ),
        wellPanel(
          radioButtons(inputId = "graph",
                         label = "Select graph type:",
                         choices = c("Barchart", "Piechart", "Map"),
                         selected = "Barchart"),
            radioButtons(inputId = "position",
                         label = "Select bin position",
                         choices = c("Stack", "Dodge")),
            h6("Check the 'Region group' box to display votes according to regions"),
            checkboxInput(inputId = "facet",
                          label = "Region group",
                          value = F),
            h6("Key", br(),
               "APC: All Progressives Congress", br(),
               "LP: Labour Party", br(),
               "PDP: People's Democratic Party")
          )
        ),
      mainPanel(tabsetPanel(
        tabPanel(
          title = "Plot",
          plotOutput("p1", height = 500)
        ),
        tabPanel(
          title = "Overall plot",
          plotOutput("p2", height = 550)
        ),
        tabPanel(
          title = "Map",
          leafletOutput("map", width = "100%", height = 550),
          h5("Tip: Click points to show votes received by each party")
        )
      ),
      h4("Produced by: Lawrence")
      )
    )))
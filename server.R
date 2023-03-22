
# Loading required packages -----------------------------------------------
library(tidyverse)
library(shiny)
library(leaflet)


# Reading data ------------------------------------------------------------
Election_2023 <- read.csv("Election_2023_2.csv")
elect <- Election_2023[1:5]
elect_2 <- Election_2023[1:5]

# for map -----------------------------------------------------------------
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
# end ---------------------------------------------------------------------


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
shinyServer(
  function(input, output){
    
    df_state <- reactive({
      elect %>% 
        filter(State == input$state)
    })
    output$p1 <- renderPlot({
      if (input$graph == "Piechart"){
        ggplot(df_state(), aes(x = " ", y = votes, fill = as.factor(party)))+
          geom_bar(stat = "identity")+
          scale_fill_discrete(name = "Party")+
          coord_polar(theta = "y")+
          ggthemes::theme_clean()+
          labs(x = " ",
               y = " ")
       
      }else
      {
        ggplot(df_state(), aes(x = party, y = votes, fill = party))+
          geom_bar(stat = "identity", show.legend = F)+
          geom_text(aes(label = votes), size = 7, vjust = 0, face = "bold")+
          scale_x_discrete(name = "Party")+
          scale_y_continuous(name = "Votes")+
          theme_minimal()+
          labs(
            title = paste0("Election results from ", input$state, " state")
          )+
          theme(plot.title.position = "plot",
                plot.title = element_text(face = "bold", hjust = 0, size = 15),
                axis.title = element_text(face = "bold", 
                                          colour = "black",
                                          size = 15),
                axis.text = element_text(colour = "black",
                                         size = 12),
                axis.text.x = element_text(face = "bold"),
                plot.caption = element_text(size = 12, hjust = 0),
                plot.caption.position = "plot")}

        })
    
    output$map <- renderLeaflet({
      if (input$graph == "Map"){
        elect_clean %>% 
          leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron") %>% 
          addCircleMarkers(weight = 1, color = elect_clean$winner, radius = 10,
                           popup = c(paste0("<b>APC<b> = ", elect_col$APC, "\nPDP = ", elect_col$PDP, "\nLP = ", elect_col$LP))) %>% 
          addLegend(colors = c("red", "green", "blue"), labels = c("APC", "LP", "PDP"))
      }
    })
    
    output$p2 <- renderPlot({
      if (input$facet){
        ggplot(all_states, aes(x = State, y = votes, fill = as.factor(party)))+
          geom_bar(stat = "identity", show.legend = F, position = str_to_lower(input$position))+
          scale_x_discrete(name = "States")+
          scale_y_continuous(name = "Votes")+
          ggthemes::theme_clean()+
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 0, face = "bold", colour = "black"),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 15),
            strip.text = element_text(size = 15, face = "bold"))+
          facet_wrap(~as.factor(Region), scales = "free")}
      else
      {ggplot(all_states, aes(x = State, y = votes, fill = party))+
          geom_bar(stat = "identity", show.legend = F, position = str_to_lower(input$position))+
          scale_x_discrete(name = "States")+
          scale_y_continuous(name = "Votes")+
          theme_minimal()+
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, face = "bold", colour = "black"),
            axis.title = element_text(face = "bold", size = 15),
            axis.text = element_text(size = 10),
            plot.title = element_text(face = "bold", size = 15)) 
      }

    })
  })

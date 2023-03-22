# Load libraries
library(shiny)
library(tidyverse)

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

adult <- drop_na(adult)

prediction <- adult$prediction

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$country)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      ggplot(df_country(), aes_string(x = input$continuous_variable)) +
        geom_histogram(col = "white", fill = "blue", bins = input$bins) +  # histogram geom
        labs(x = input$continuous_variable,
             y = "Number of People",
             title = paste("Trend of", input$continuous_variable)) +  # labels
        facet_wrap(~factor(prediction)) +    # facet by prediction
        ggthemes::theme_clean()+
        theme(
          axis.title = element_text(face = "bold", size = 15))
    }
    else {
      # Boxplot
      ggplot(df_country(), aes_string(y = input$continuous_variable)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(
          title = paste("Boxplot of", input$continuous_variable)) +  # labels
        facet_wrap(~factor(prediction)) +  # facet by prediction
        ggthemes::theme_clean()+
        theme(
          axis.title = element_text(face = "bold", size = 15))
    }
    
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes_string(x = input$categorical_variable)) +
      labs(y = "No of People",
           title = paste("Bar chart of", input$categorical_variable)) +  # labels
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45),
        axis.title = element_text(face = "bold", size = 15),
        axis.text = element_text(size = 10),
        plot.title = element_text(face = "bold", size = 15)
      )    # modify theme to change text angle and legend position
    
    if (input$is_stacked) {
      p + geom_bar(aes(fill = prediction))  # add bar geom and use prediction as fill
    }
    else{
      p + 
        geom_bar(aes_string(fill = input$categorical_variable)) + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(~factor(prediction))   # facet by prediction
    }
  })
  
})

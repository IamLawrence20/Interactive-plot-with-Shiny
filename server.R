
library(shiny)
library(gapminder)
library(tidyverse)

shinyServer(
  function(input, output){
    
    country_df <- reactive({
      gapminder %>% 
        filter(country == input$country)
    })
    
    output$p1 <- renderPlot({
      if (input$graph == "Scatter plot") {
        ggplot(country_df(), aes (x = !!input$x_axis,
                                  y = !!input$y_axis))+
          scale_x_continuous(breaks = seq(1952, 2008, 5),
                             limits = c(1952, 2008))+
          geom_point(col = input$colour, size = 2.5)+
          labs(
            title = paste0("Scatter plot of ", input$x_axis, " vs ", input$y_axis),
            caption = "Data source: Gapminder.com\nProduced by Lawrence")+
          theme_bw()+
          theme(
            plot.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold", 
                                      colour = "black",
                                      size = 15),
            axis.text = element_text(colour = "black",
                                     size = 12),
            plot.caption = element_text(size = 12, hjust = 0),
            plot.caption.position = "plot")
      }else
        {
          ggplot(country_df(), aes (x = !!input$x_axis,
                                 y = !!input$y_axis))+
          geom_line(lwd = 1.2, col = input$colour)+
            scale_x_continuous(breaks = seq(1952, 2008, 5),
                               limits = c(1952, 2008))+
            labs(
              title = paste0("Line graph of ", input$x_axis, " vs ", input$y_axis),
              caption = "Data source: Gapminder.com\nProduced by Lawrence")+
            theme_grey()+
            theme(
              plot.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold", 
                                        colour = "black",
                                        size = 15),
            axis.text = element_text(colour = "black",
                                       size = 12),
            plot.caption = element_text(size = 12, hjust = 0),
            plot.caption.position = "plot")}

    })
  }
)
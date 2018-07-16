library("readr")
library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")
library("stringr")
library("rdrop2")
library("readxl")
library(googleVis)

function(input, output) {
  Models <- readRDS("/home/rstudio/Data.Rdata")
  
  ModelsShort <- reactive({Models[which(Models$ModelName %in% input$ModList),]})
  
  dataset <- reactive({
        data.frame(ModelName = ModelsShort()$ModelName,
               Game = ModelsShort()$Game,
               Factor = ModelsShort()[,input$factor])
  })
  
  output$tmplot <- renderPlotly(plot_ly(dataset(), 
                                          x = ~Game, y = dataset()[,3], 
                                            type = 'scatter', mode = 'lines', linetype = ~ModelName)%>%
                                  layout(autosize = F, width = 1100, height = 630, title = 'Predictions after each game')%>%
                                  layout(legend = list(orientation = 'h'))
                                
                                )

}





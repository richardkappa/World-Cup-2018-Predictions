library("readr")
library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")
library("stringr")
library("rdrop2")
library("readxl")
library(googleVis)

Models <- readRDS("/home/rstudio/Data.Rdata")

pageWithSidebar(
  titlePanel('Model Predictions Over Over Time')
  , sidebarPanel(
     selectInput('factor', 'Metric', c("Rank", "Score", "GD_Error", "p_Exact", "p_Draw", "p_Result"), selected ="Rank"),
     checkboxGroupInput("ModList", "Which Predictions", unique(Models$ModelName)[order(unique(Models$ModelName))], 
                        selected = c("Oliver Garner", "Edward Li Predictions", "Fifa Ranking, Simple", "PB2", "FinalFarewell"))
     , width = 3)
  , mainPanel(
    plotlyOutput("tmplot")
  )
)


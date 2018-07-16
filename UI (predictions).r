library("readr")
library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")

shinyUI(fluidPage(
  titlePanel("Prediction League"),
  dataTableOutput(outputId = "LeagueTable"),
  plotlyOutput("plot")
)
)

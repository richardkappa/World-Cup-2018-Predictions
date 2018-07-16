# World Cup 2018 Predictions League

The R code needed to run a predictions league for the 2018 world cup and to make two shiny apps in R to display the results to the competitiors. The data needed by the model is in predictions.zip

# Predictions
Contains two parts of the shiny app.
Server.r imports all of the competition .csvs and creates the league.
UI.r creates the UI for the shiny app. This bit is very basic, just a title, table and chart.

# Tracker
The key bit of code here is CreateTracker.r. This creates a dataframe with snapshots of the league after each game so that the shiny app can display the league position in an interactive chart

server.r imports the dataframe and creates a chart to display the data

ui.r displays the chart and selects which competitiors to display on the chart initially.

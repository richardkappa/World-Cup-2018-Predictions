library("readr")
library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")
library("stringr")
library("rdrop2")
library("readxl")

shinyServer(function(input, output) {

  #Model List
  ModelList<- read_csv("http://richardkappa.co.uk/wp-content/uploads/2018/06/ModelList-ol.csv")
  
  #Import actual Scores
  A_data  <- "http://richardkappa.co.uk/wp-content/uploads/2018/06/Actuals.csv"
  names <- c("TeamA",	"TeamB", "ActualA", "ActualB")
  
  Actual<- read_csv("http://richardkappa.co.uk/wp-content/uploads/2018/06/Actuals.csv")
  #Actual<- read_excel(paste("~/Dropbox/Predictions/Actuals", ".xlsx", sep=""), 1)
  Actual <- Actual[complete.cases(Actual), ]
  names(Actual) <- names
  
  #Calculate Results For Actuals
  Actual$A_Result <- ifelse(Actual$ActualA>Actual$ActualB,"A",  ifelse(Actual$ActualA==Actual$ActualB,"0","B"))
  Actual$A_GD <- Actual$ActualA - Actual$ActualB
  Actual$A_GD2 <- ifelse(abs(Actual$A_GD)>=2,1,0)
  
  #Function for scoring the models
  CalcScore <- function(Model){
    
    location <- ModelList[which(ModelList$ModelName == Model),]$ModelFileName
    location2 <- paste("http://richardkappa.co.uk/wp-content/uploads/2018/06",location, sep="/")
    
    #Import the model
    Model<- read_csv(location2)
    Model <- Model[,1:4]
    names <- c("TeamA",	"TeamB", "AScore", "BScore")
    names(Model) <- names
    
    #Calculate Results For Models
    Model$M_Result <- ifelse(Model$AScore>Model$BScore,"A",  ifelse(Model$AScore==Model$BScore,"0","B"))
    Model$M_GD <- Model$AScore - Model$BScore
    Model$M_GD2 <- ifelse(abs(Model$M_GD)>=2,1,0)
    
    #Merge model onto actual
    Merged <- inner_join(Model, Actual, by=c("TeamA",	"TeamB"))
    
    #Score the model
    Merged$Score <- 
      #0 if result wrong
      ifelse(Merged$M_Result!=Merged$A_Result,0,
             #5 for exact guess
             ifelse(Merged$AScore==Merged$ActualA & Merged$BScore==Merged$ActualB,5,
                    #3 for correctly guessing a tie           
                    ifelse(Merged$M_Result==0,3,
                           #4 for correct result and goal difference
                           ifelse(Merged$M_GD==Merged$A_GD,4,
                                  #3 for predicting gd>=2 and 2 for correct result only
                                  ifelse(Merged$M_GD2==Merged$A_GD2,3,2
                                  )))))
    #Calculate total score
    Score <- sum(Merged$Score)
    GD_Error <- sum(abs(Merged$M_GD-Merged$A_GD))
    
    Exact <- sum(ifelse(Merged$AScore==Merged$ActualA & Merged$BScore==Merged$ActualB,1,0))
    Draw <- sum(ifelse((Merged$M_Result==0 & Merged$M_Result==Merged$A_Result),1,0))
    Result <- sum(ifelse((Merged$M_Result==Merged$A_Result),1,0))
    
    return(toString(list(Score,GD_Error,Exact,Draw,Result)))
  }
  
  #Score all of the models
  ModelList$ModelFileName <- gsub(" ", "-", ModelList$ModelFileName)
  ModelList$ModelFileName <- gsub("'", "", ModelList$ModelFileName)
  ModelList$Score <- lapply(ModelList$ModelName,CalcScore)
  
  Results <- as.data.frame(str_split_fixed(ModelList$Score, ",", 5))
  
  new_ModelList <- cbind(ModelList, Results)
  
  new_ModelList$V1 <- as.numeric(levels(new_ModelList$V1))[new_ModelList$V1]
  new_ModelList$V2 <- as.numeric(levels(new_ModelList$V2))[new_ModelList$V2]
  new_ModelList$V3 <- as.numeric(levels(new_ModelList$V3))[new_ModelList$V3]
  new_ModelList$V4 <- as.numeric(levels(new_ModelList$V4))[new_ModelList$V4]
  new_ModelList$V5 <- as.numeric(levels(new_ModelList$V5))[new_ModelList$V5]
  
  names <- c("Model Name",	"Prediction Name", "Employee Name", "AllScores", "Score", "GD_Error", "p_Exact", "p_Draw", "p_Result")
  names(new_ModelList) <- names
  
  
  #Order by the score
  order.scores<-order(-new_ModelList$Score, new_ModelList$GD_Error)
  
  new_ModelList <- new_ModelList[order.scores,] 
  order.scores<-order(-new_ModelList$Score)
  new_ModelList$Rank <- NA
  new_ModelList$Rank[order.scores] <- 1:nrow(new_ModelList)
  
  new_ModelList <- new_ModelList[,c("Rank",	"Model Name", "Employee Name", "Score", "p_Result", "p_Exact", "p_Draw", "GD_Error")]
  
  output$LeagueTable <- renderDataTable(new_ModelList)
  
  output$plot <- renderPlotly({plot_ly(data=new_ModelList, x = ~as.factor(Rank)) %>%
      
      
      add_trace(y = new_ModelList$Score, type = 'scatter', mode = 'lines', name = 'Score',
                line = list(color = 'rgb(0, 0, 0)', width = 2),
                hoverinfo = "text",
                text = paste(new_ModelList$"Model Name")) %>%
      
      
      layout(title = 'Prediction League',
             xaxis = list(title = "Rank"),
             yaxis = list(side = 'left', title = 'Score', showgrid = TRUE, zeroline = FALSE)) })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
  
}  
)

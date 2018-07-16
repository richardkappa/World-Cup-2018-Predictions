library("readr")
library("dplyr")
library("shiny")
library("ggplot2")
library("plotly")
library("stringr")
library("rdrop2")
library("readxl")
library(googleVis)
#loop league

#Model List
ModelList<- read_csv("/home/rstudio/Predictions/ModelList.csv")

#Import actual Scores
A_data  <- "/home/rstudio/Predictions/Actuals.csv"
names <- c("TeamA",	"TeamB", "ActualA", "ActualB")

fullactual<- read_csv(A_data)
fullactual <- fullactual[complete.cases(fullactual), ]
names(fullactual) <- names

CalcScore <- function(Model){
  
  location <- ModelList[which(ModelList$ModelName == Model),]$ModelFileName
  location2 <- paste("/home/rstudio/Predictions",location, sep="/")
  print(Model)
  
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

prediction_Count <- nrow(fullactual)
Model_Count <- nrow(ModelList)

Models <- ModelList[,1]
M <- ModelList[,1]

Models$Score <- NA
Models$Rank <- NA
Models$Game <- NA
Models$GD_Error <- NA
Models$p_Exact <- NA
Models$p_Draw <- NA
Models$p_Result <- NA

for (i in 1:prediction_Count) {
  Actual <- fullactual[1:i,]
  #Calculate Results For Actuals
  Actual$A_Result <- ifelse(Actual$ActualA>Actual$ActualB,"A",  ifelse(Actual$ActualA==Actual$ActualB,"0","B"))
  Actual$A_GD <- Actual$ActualA - Actual$ActualB
  Actual$A_GD2 <- ifelse(abs(Actual$A_GD)>=2,1,0)
  
  #Score all of the models
#  ModelList$ModelFileName <- gsub(" ", "-", ModelList$ModelFileName)
#  ModelList$ModelFileName <- gsub("'", "", ModelList$ModelFileName)
  ModelList$Score <- lapply(ModelList$ModelName,CalcScore)
  
  
  Results <- as.data.frame(str_split_fixed(ModelList$Score, ",", 5))
  Results$V1 <- as.numeric(levels(Results$V1))[Results$V1]
  Results$V2 <- as.numeric(levels(Results$V2))[Results$V2]
  Results$V3 <- as.numeric(levels(Results$V3))[Results$V3]
  Results$V4 <- as.numeric(levels(Results$V4))[Results$V4]
  Results$V5 <- as.numeric(levels(Results$V5))[Results$V5]

  names <- c("Score", "GD_Error", "p_Exact", "p_Draw", "p_Result")
  names(Results) <- names
  Results <- Results[,names]
  M2 <- cbind(M, Results)
  
  order.scores<-order(-M2$Score, M2$GD_Error)

  M2 <- M2[order.scores,] 
  order.scores<-order(-M2$Score)
  M2$Rank <- NA
  M2$Rank[order.scores] <- 1:nrow(M2)
  
  M2$Game <- i
  
  Models <- rbind(Models,M2)
}

Models <- Models[complete.cases(Models), ]

ModList <- c("Oliver Garner", "Edward Li Predictions", "Fifa Ranking, Simple", "PB2", "FinalFarewell")

jpeg('LeagueOverTime.jpg', 
     units="cm", 
     width=30, 
     height=21, 
     pointsize=5, 
     res=150)

ggplot(data=Models[which(Models$ModelName %in% ModList),], aes(x=Game, y=Rank, group=ModelName)) +
  geom_line(aes(color=ModelName))  +
  geom_point(aes(color=ModelName)) +
  theme(legend.position="bottom")

dev.off()

plot_ly(Models[which(Models$ModelName %in% ModList),]
             , x = ~Game, y = ~Score, type = 'scatter', mode = 'lines', linetype = ~ModelName)

saveRDS(Models, "/home/rstudio/Data.Rdata")

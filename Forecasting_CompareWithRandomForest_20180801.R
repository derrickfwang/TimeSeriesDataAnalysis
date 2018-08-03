library(readxl)
setwd('C:\\...')

Df1 <- read_excel('.....xlsx',sheet = 1)

inputTrain <- Df1[1:147,]
inputTest <- Df1[148:182,]

varNames <- names(Df1)
varNames <- varNames[!varNames %in% c("Date", "Amt")]
varNames1 <- paste(varNames, collapse="+")

rf.form <- as.formula(paste("Amt", varNames1, sep=" ~ "))

library(randomForest)
Model1.rf <- randomForest(
  rf.form,
  inputTrain,
  ntree=150,
  importance=T
)

varImpt <- data.frame(importance(Model1.rf, type=2))
varImpt['MeanDecreaseAccuracy'] <- importance(Model1.rf, type=1)

inputTrain$predictedAmt <- predict(Model1.rf, inputTrain)
inputTest$predictedAmt <- predict(Model1.rf, inputTest)

library(e1071)
library(caret)

plot(Model1.rf)
varImpPlot(Model1.rf,
           sort=T,
           main="Variable Importance")

library(dplyr)

inputTest %>% ggplot(aes(Date)) +
  geom_line(aes(y=Amt, colour='b')) +
  geom_line(aes(y=predictedAmt, colour='r')) 


inputTrain %>% ggplot(aes(Date)) +
  geom_line(aes(y=Amt, colour='b')) +
  geom_line(aes(y=predictedAmt, colour='r')) 

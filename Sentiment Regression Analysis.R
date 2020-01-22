library(readr)
library(ggplot2)
library(doParallel)
library(plotly)
library(caret)
library(corrplot)

factorsFIN <- read_csv("concat_factorsFIN.csv")
websitesFIN <- read_csv("concat_websitesFIN.csv")
iphone <- read_csv("iphone_smallmatrix_labeled_8d.csv")
galaxy <- read_csv("galaxy_smallmatrix_labeled_9d (1).csv")


# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)

# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)


#----------- Data Exploration -----------#
sum(is.na(galaxy))
sum(is.na(iphone))

#checking Variance for iphone

plot_ly(iphone, x= ~iphonesentiment, type='histogram')
nzvMetrics <- nearZeroVar(iphone, saveMetrics = TRUE)
nzvMetrics

nzv <- nearZeroVar(iphone, saveMetrics = FALSE) 
nzv

iphoneNZV <- iphone[,-nzv]
str(iphoneNZV)


#checking Variance galaxy

plot_ly(galaxy, x= ~galaxysentiment, type='histogram')

nzvMetricsG <- nearZeroVar(galaxy, saveMetrics = TRUE)
nzvMetricsG
nzvG <- nearZeroVar(galaxy, saveMetrics = FALSE) 
nzvG


galaxyNZV <- galaxy[,-nzv]
str(galaxyNZV)
correlG <- cor(galaxyNZV, )



# Let's sample the data before using RFE
set.seed(123)
iphoneSample <- iphone[sample(1:nrow(iphone), 1000, replace=FALSE),]

# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 3,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResults <- rfe(iphoneSample[,1:58], 
                  iphoneSample$iphonesentiment, 
                  sizes=(1:58), 
                  rfeControl=ctrl)

# Get results
rfeResults

# Plot results
plot(rfeResults, type=c("g", "o"))


iphoneRFE <- iphone[,predictors(rfeResults)]
iphoneRFE

# add the dependent variable to iphoneRFE
iphoneRFE$iphonesentiment <- iphone$iphonesentiment

# review outcome
str(iphoneRFE)

library(dplyr)

sentimentIphone_5 <- dplyr::filter(iphone, iphonesentiment == 5 )
sentimentIphone_4 <- dplyr::filter(iphone, iphonesentiment == 4 )
sentimentIphone_3 <- dplyr::filter(iphone, iphonesentiment == 3 )
sentimentIphone_2 <- dplyr::filter(iphone, iphonesentiment == 2 )
sentimentIphone_1 <- dplyr::filter(iphone, iphonesentiment == 1 )

sentimentgalaxy_5 <- dplyr::filter(galaxy, galaxysentiment == 5)
sentimentgalaxy_4 <- dplyr::filter(galaxy, galaxysentiment == 4)
sentimentgalaxy_3 <- dplyr::filter(galaxy, galaxysentiment == 3)
sentimentgalaxy_2 <- dplyr::filter(galaxy, galaxysentiment == 2)
sentimentgalaxy_1 <- dplyr::filter(galaxy, galaxysentiment == 1)

corrplot(cor(factorsFIN), method = "pie")
corrplot(cor(iphoneRFE), method = "pie")

#data partition and modelling#
    
set.seed(123)
part.index <- createDataPartition(iphoneRFE$iphonesentiment, 
                                      p = 0.75,                         
                                      list = FALSE)
iTrain <- iphoneRFE[ part.index,]
iTest  <- iphoneRFE[-part.index,]
    

#this library for testing different ML algo at the same time#
library(caretEnsemble)

set.seed(123)
my_control <- trainControl(method = "cv", 
                           number = 3, 
                           savePredictions = "final")

set.seed(222)
model_list <- caretList(iTrain, iTrain$iphonesentiment,
                        trControl = my_control,
                        methodList = c("lm", "svmRadial", 
                                        "xgbTree", "xgbLinear"),
                        tuneList = NULL,
                        continue_on_fail = FALSE, 
                        preProcess = c("center","scale"))

options(digits = 3)
model_results <- data.frame(
  LM = min(model_list$lm$results$RMSE),
  SVM = min(model_list$svmRadial$results$RMSE),
  XGBT = min(model_list$xgbTree$results$RMSE),
  XGBL = min(model_list$xgbLinear$results$RMSE)
)
print(model_results)


resamples <- resamples(model_list)
dotplot(resamples, metric = "RMSE")

# PREDICTIONS
pred_lm <- predict.train(model_list$lm, newdata = iTest)
pred_svm <- predict.train(model_list$svmRadial, newdata = iTest)
pred_xgbT <- predict.train(model_list$xgbTree, newdata = iTest)
pred_xgbL <- predict.train(model_list$xgbLinear, newdata = iTest)


# Create a confusion matrix from random forest predictions 
cmRF <- confusionMatrix(pred_lm, irisTest$iphonesentiment) 
cmRF


# RMSE
pred_RMSE <- data.frame(
                        LM = RMSE(pred_lm, iTest$iphonesentiment),
                        SVM = RMSE(pred_svm, iTest$iphonesentiment),
                        XGBT = RMSE(pred_xgbT, iTest$iphonesentiment),
                        XGBL = RMSE(pred_xgbL, iTest$iphonesentiment))
print(pred_RMSE)



 
library(readr)
library(ggplot2)
library(doParallel)
library(plotly)
library(caret)
library(corrplot)


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
str(iPhoneRFE)



corrplot(cor(factorsFIN), method = "pie")
corrplot(cor(iphoneRFE), method = "pie")

#data partition and modelling#

set.seed(123)
part.index <- createDataPartition(iphoneRFE$iphonesentiment, 
                                  p = 0.75,                         
                                  list = FALSE)
iTrain <- iphoneRFE[ part.index,]
irisTest  <- iphoneRFE[-part.index,]


#this library for testing different ML algo at the same time#
library(caretEnsemble)

set.seed(123)
my_control <- trainControl(method = "cv", 
                           number = 3, 
                           savePredictions = "final")


set.seed(222)
model_list <- caretList(iTrain, iTrain$iphonesentiment,
                        trControl = my_control,
                        methodList = c("kknn", "knn"),
                        tuneList = NULL,
                        continue_on_fail = FALSE, 
                        preProcess = c("center","scale"))

options(digits = 3)
model_results <-data.frame(KNN = min(model_list$knn$results$kappa), KKNN = min(model_list$kknn$results$kappa))
print(model_results)


resamples <- resamples(model_list)
dotplot(resamples, metric = "RMSE")

# PREDICTIONS
pred_lm <- predict.train(model_list$kknn, newdata = irisTest)
pred_svm <- predict.train(model_list$knn, newdata = irisTest)



# Create a confusion matrix from random forest predictions 
cmRF <- confusionMatrix(pred_lm, irisTest$iphonesentiment) 
cmRF


# RMSE
pred_RMSE <- data.frame(ensemble_1 = RMSE(predict_ens1, y_test),
                        ensemble_2 = RMSE(predict_ens2, y_test),
                        LM = RMSE(pred_lm, y_test),
                        SVM = RMSE(pred_svm, y_test),
                        RF = RMSE(pred_rf, y_test),
                        XGBT = RMSE(pred_xgbT, y_test),
                        XGBL = RMSE(pred_xgbL, y_test))
print(pred_RMSE)



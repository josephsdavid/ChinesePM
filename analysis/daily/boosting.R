source("../../R/helpers.R")
load("bags.Rda")
library(caret)

fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:50)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 0.2)
nrow(gbmGrid)
library(doParallel)
workers <- makeCluster(11L)
registerDoParallel(workers)
cbagVal <- as.data.frame(lapply(bagval, as.numeric))
gbmFit <- train(ppm ~ ., data = cbagVal, 
                method = "gbm", 
                trControl = fitControl, 
                verbose = FALSE, 
                ## Now specify the exact models 
                ## to evaluate:
                tuneGrid = gbmGrid)
plot(gbmFit)
cbagTest <- as.data.frame(lapply(bagtest, as.numeric))
gpmPred <- predict(gbmFit, cbagTest)
options(scipen = 999)
scores(as.bag(gpmPred))
#           ASE          MAPE 
# 1540375.75973      57.42912 

# extrem gradient boosting time
beepr::beep(0)
library(doParallel)
workers <- makeCluster(11L)
registerDoParallel(workers)
#xgbGrid <- expand.grid(nrounds = (1:1,  
 #                      max_depth = (1:10)*5,
  #                     colsample_bytree = seq(0.1, 0.9, length.out = 11),
   #                    eta = .1,
                   #    gamma=0,
                  #     min_child_weight = 1,
                    #   subsample = 1
#)
xgb_trcontrol = trainControl(
                             method = "repeatedcv",
                             number = 10,  
                             repeats = 10,
                             allowParallel = TRUE,
                             verboseIter = F,
                             returnData = F
)

xgbFit = train(ppm ~.,
                  data = cbagVal,
                  trControl = xgb_trcontrol,
                  tuneLength = 10 ,
                  method = "xgbDART"
)
beepr::beep(0)

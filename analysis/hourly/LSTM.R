source("mvarsetup.R")
library(keras)
library(kerasR)
library(tswgewrapped) # for my brilliant mlag_dfr function
lag_transform <- function(x, k= 1){
    
      lagged =  c(rep(NA, k), x[1:(length(x)-k)])
      DF = as.data.frame(cbind(lagged, x))
      colnames(DF) <- c( paste0('x-', k), 'x')
      DF[is.na(DF)] <- 0
      return(DF)
}
dat <- trainM%>% dplyr::select(PM_US.Post)  %>% lag_transform 
N <- nrow(dat)
n <- round(N * ( 0.7 ), digits = 0)
trainK <- dat[1:n,]
testK <- dat[(n+1):N,]

scaleData = function(train, test, featureRange = c(0, 1)) {
  x = train
  frMin = featureRange[1]
  frMax = featureRange[2]
  stdTrain = ((x - min(x) ) / (max(x) - min(x)  ))
  stdTest  = ((test - min(x) ) / (max(x) - min(x)  ))
  
  scaledTrain = stdTrain *(frMax -frMin) + frMin
  scaledTest = stdTest *(frMax -frMin) + frMin
  
  return( list(scaledTrain = (scaledTrain), scaledTest = (scaledTest) ,scaler= c(min =min(x), max = max(x))) )
  
}
scaled <- scaleData(trainK, testK, c(-1,1))
xTrain <- scaled$scaledTrain[,1]
yTrain <- scaled$scaledTrain[,2]
xTest <- scaled$scaledTest[,1]
yTest <- scaled$scaledTest[,2]
str(xTest)

descale <- function(scaled, scaler, featureRange = c(0,1)){
  min <- scaler[1]
  max <- scaler[2]
  t <- length(scaled)
  mins <- featureRange[1]
  maxs <- featureRange[2]
  inverted <- double(t)
  for (i in 1:t){
    X <- (scaled[i] - mins)/(maxs - mins)
    raw <- X*(max-min)+min
    inverted[i] <- raw
  }
  return(inverted)
}
dim(xTrain) <- c(length(xTrain),1,1)
str(yTrain)
dim(xTest) <- c(length(xTest),1,1)
dim(xTrain)


xShape2 <- dim(xTrain)[2]
dim(xTrain)
xShape3 <- dim(xTrain)[3]
batchSize <- 1
units  <- 1
model <- keras_model_sequential() %>%
  layer_lstm(units, batch_input_shape = c(batchSize, xShape2, xShape3), stateful= TRUE, dropout = 0.2, recurrent_dropout = 0.2)%>%
  layer_dense(units = 1)

model %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(lr = 0.0001),
                  metrics =c("accuracy"))
summary(model)
for (i in 1:Epochs){
  model %>% fit(xTrain, yTrain, epochs = 1, batch_size = batchSize, verbose = 1, shuffle = F, validation_data = list(xTest,yTest))
  model %>% reset_states()
}
plot(h)

testM
testset <- as.numeric(testM$PM_US) %>% lag_transform(k = 1) 
testset


scaltest <- scaleData(testset, testset, c(-1,1))
xTest2 <- scaltest$scaledTest[,1]
str(xTest2)
yTest2 <- scaltest$scaledTest[,2]

dim(xTest2) <- c(length(xTest2),1,1)

L = 72
scaler = scaltest$scaler
xTest2[2,,]
xTest2[1,,]
predictions = numeric(L)
for(i in 1:L){
  X = xTest2[i , , ]
  dim(X) = c(1,1,1)
  # forecast
  yhat = model %>% predict(X, batch_size=batchSize)

  # invert scaling
  yhat = descale(yhat, scaler,  c(-1, 1))

  # save prediction
  predictions[i] <- yhat
}
toArray <- . %>% array(dim = c(1,1,1))
xTest2[1,,] %>%  array(dim = c(1,1,1)) -> onebeh
predictions <- numeric(72)
onestep <- model %>% predict(onebeh, batch_size = 1) 
predictions[1]  <- onestep
predictions
onepred <- function(pt){
  model %>% predict(pt, batch_size = 1)
}
predictions[2]  <-  predictions[1] %>% toArray %>% onepred
predictions[3]  <- predictions[2] %>% toArray %>% onepred
predictions
for(i in 2:predictions){
  predictions[i]  <- predictions[i-1] %>% toArray %>% onepred
}


onestep_pred <- model %>% predict(array(xTest2[1,,], dim = c(1,1,1))) %>% descale(scaler, c(-1,1))

mean((predictions - yTest2)^2)
plot(predictions)
points(as.numeric(testM$PM_US))


fibrary(forecast)
library(ggplot2)
library(ggthemes)
library(nnfor)
library(ggplot2)
library(ggthemes)
library(doParallel)
library(dplyr)
library(foreach)
source("../../R/preprocessing.R")
source("../../R/helpers.R")


china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)


train[1:3]  <- NULL
train[9:14] <- NULL
test[1:3]  <- NULL
test[9:14] <- NULL
traints <- (purrr::keep(train,is.ts))
trainexogen <- purrr::discard(train, is.ts)
trainexogen <- lapply(trainexogen, as.factor) %>>% as.data.frame
test %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> test
traints %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> train

nnet <- nnetar(train[[1]], xreg = train[-1],repeats = 1000)
load("nnetar.Rda")
save(nnet, file = "nnetar.Rda")
summary(nnet)
#           Length Class        Mode     
# x         1461   ts           numeric  
# m            1   -none-       numeric  
# p            1   -none-       numeric  
# P            1   -none-       numeric  
# scalex       2   -none-       list     
# scalexreg    2   -none-       list     
# size         1   -none-       numeric  
# xreg      5844   -none-       numeric  
# subset    1461   -none-       numeric  
# model     1000   nnetarmodels list     
# nnetargs     0   -none-       list     
# fitted    1461   ts           numeric  
# residuals 1461   ts           numeric  
# lags        28   -none-       numeric  
# series       1   -none-       character
# method       1   -none-       character
# call         4   -none-       call     
nnetfor <- forecast(nnet, h = 366, xreg = test[-1])
autoplot(nnetfor)
autoplot(as.nfor(nnetfor))
options(scipen = 999)
scores(as.nfor(nnetfor))
#          MAPE           ASE 
#      60.74935 3729064.24571 
#          MAPE           ASE 
#      66.49151 3781826.27512 


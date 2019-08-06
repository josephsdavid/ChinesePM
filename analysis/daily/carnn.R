library(doParallel)
library(foreach)
library(dplyr)
library(latticeExtra)
library(ggthemes)
library(ggplot2)

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
traints %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> training
# now we can load caret without masking the train function

rm(train)
library(caret)
library(RSNNS)
d.nn <- mlp(training[-1],training[[1]])
predict(d.nn,test[-1])

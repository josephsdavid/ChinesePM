library(nnfor)
library(ggplot2)
library(ggthemes)
library(doParallel)
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

# this is too slow, there must be a better way, we will try out caret
# I will use nnetar and caret
xample <- beep( mlp(traints$PM_US.Post,m = c(365) ) )
train <- as.data.frame(lapply(train, function(x) ts(x, frequency = 365)))
test <- as.data.frame(lapply(test, function(x) ts(x, frequency = 365)))
attmpt <- mlp(y = train$PM_US.Post)

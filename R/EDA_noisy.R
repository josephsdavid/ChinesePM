library(tswgewrapped)
library(ggplot2)
source("preprocessing.R")
china <- preprocess_noisy('../data/')
ls(china)
# [1] "BeijingPM_"   "ChengduPM_"   "GuangzhouPM_" "ShanghaiPM_"  "ShenyangPM_" 
str(china)
# <environment: 0x625fa48> 

myts <- china$BeijingPM_$PM_US

split <- function(xs){
	train_ind <- 1:(floor(length(xs)*4/5))
	training <- xs[train_ind]
	testing <- xs[-train_ind]
	list(train = training, test = testing)
}


tslist <- split(myts)
train  <- tslist$train
test <- tslist$test

plotts.sample.wge(train)

ma <- function(xs, n) {
    stats::filter(xs, rep(1, n))/n
}

plotts.sample.wge(na.omit(ma(train, 24*7*4)))

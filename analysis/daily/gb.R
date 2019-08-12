library(gbm)
library(dplyr)
source("../../R/helpers.R")
load("bags.Rda")
head(bagval)
head(bagtest)
options(scipen = 999)

boost <- gbm(ppm ~ ., data = bagval, n.trees = 10000, interaction.depth = 4, shrinkage = 0.01)
summary(boost)
predict(boost, bagtest, n.trees = 10000) %>% as.bag %>% scores

library(ggplot2)
library(forecast)
library(ggthemes)


predict(boost, bagtest, n.trees = 10000) %>% ts -> boostts

autoplot(boostts) + autolayer(ts(bagtest$ppm)) + theme_hc() + scale_color_hc()

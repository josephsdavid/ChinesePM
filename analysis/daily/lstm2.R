library(keras)
library(ggplot2)
library(cowplot)
library(glue)
library(ggthemes)
library(ggplot2)
library(tidyverse)
library(tswgewrapped)
library(tibbletime)
library(rsample)
library(timetk)
source("../../R/preprocessing.R")
source("../../R/helpers.R")

china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )

bj2 <- dlist(bj)
length(window( cleandays( bj$PM_US ), end = 6 ))
dfsplit(bj2)



train[1:3]  <- NULL
train[9:14] <- NULL
test[1:3]  <- NULL
test[9:14] <- NULL
traints <- (purrr::keep(train,is.ts))
trainexogen <- purrr::discard(train, is.ts)
trainexogen <- lapply(trainexogen, as.factor) %>>% as.data.frame
test %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> test
traints %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> train
bj2 %>% dplyr::select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> dat


nrow(dat)
# [1] 1826
# Designing a resampling strategy

trainPeriods <- 365*2
testPeriods <- 365
skipSpan  <- 60


rollingOriginResamples <- rolling_origin(
                                         dat,
                                         initial = trainPeriods,
                                         assess = testPeriods,
                                         cumulative = FALSE,
                                         skip = skipSpan
)


PlotSplit <- function(n){
  autoplot(ts(
              training(rollingOriginResamples$splits[[n]])$PM_US, frequency = 365, end = 2)
  ) +
  autolayer(ts(
               testing(rollingOriginResamples$splits[[n]])$PM_US, frequency = 365, start = 2)
  ) + theme_hc() + scale_color_hc()  +
  theme(axis.title.x = element_blank() )  + theme(axis.title.y = element_blank()) + 
  guides(color = F)
}

plot_grid(plotlist = lapply(1:12, PlotSplit))


source("../../R/preprocessing.R")
source("../../R/helpers.R")
library(ggplot2)
library(ggthemes)
load(file = "forecasts.Rda")
save(trendCast, seaCast, armaCast, bat, mseas, test, file = "forecasts.Rda")


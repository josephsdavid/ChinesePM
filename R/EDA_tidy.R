source("preprocessing.R")
fine_china <- preprocess_tidy("../data/")
fine_china$BeijingPM_$PM_US -> bjts
forecast::gglagplot(bjts)
bjts
library(tswgewrapped)
library(ggplot2)
autoplot(decompose(bjts))
plotts.sample.wge(bjts)
difference(seasonal, bjts)
plotts.sample.wge(bjts)

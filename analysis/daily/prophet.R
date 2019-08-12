library(ggplot2)
library(ggthemes)
library(prophet)
library(ggplot2)
library(ggthemes)
library(doParallel)
library(dplyr)
library(foreach)
source("../../R/preprocessing.R")
source("../../R/helpers.R")


china <- preprocess("../../data/")
head(china$BeijingPM_$month)
head(china$BeijingPM_$day)
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
head(train[c("month","day","year")])
#   month day year
# 1     1   1 2010
# 2     1   2 2010
# 3     1   3 2010
# 4     1   4 2010
# 5     1   5 2010
# 6     1   6 2010

tail(train[c("month","day","year")])
#      month day year
# 1456    12  26 2013
# 1457    12  27 2013
# 1458    12  28 2013
# 1459    12  29 2013
# 1460    12  30 2013
# 1461    12  31 2013

head(test[c("month","day","year")])
#   month day year
# 1    12  31 2013
# 2     1   1 2014
# 3     1   2 2014
# 4     1   3 2014
# 5     1   4 2014
# 6     1   5 2014
traindates <- seq(as.Date("2010/1/1",as.Date("2013/12/31"), "days"))
st <- as.Date("2010-1-1")
en <- as.Date("2013-12-31")
trainDates <- seq(st,en, by = "day")
trainDates %>% length
prophet(train)
traindf <- data.frame(ds = trainDates, y = train$PM_US.Post )
model <- prophet(traindf)
future <- make_future_dataframe(model, periods = 366)
fore <- predict(model,future)
plot(model,fore)

fore %>% as.proph %>% autoplot
fore %>% as.proph %>% scores



source("../../R/preprocessing.R")
source("../../R/helpers.R")
library(tswgewrapped)
library(ggplot2)
library(ggthemes)
bj <- preprocess("../../data/")
bj <- bj$BeijingPM_ %>>% as.data.frame
uvar <- bj$PM_US.Post
uvar <- uvar %>>% forecast::tsclean() %>>% abs


splitMvar <- function(start=3, end =c(6, 8760-48)){
  startInd <- length(window(bj$PM_US, end = start))
  endInd <- length(window(bj$PM_US, end = end))
  bjnots <- purrr::discard(bj, is.ts)
  bjnots <- data.frame(lapply(bjnots, as.factor))
  bjts <- purrr::keep(bj, is.ts)
  bjts$PM_US.Post <- uvar # just because it is already cleaned
  notstrain <- bjnots[startInd:endInd,]
  tsTrain <- data.frame(lapply(bjts, function(x) window(x, start = start, end = end)))
  trainM <<- cbind(tsTrain, notstrain)
  notstest <- bjnots[(endInd+1):nrow(bjnots), ]
  tsTest  <- data.frame(lapply(bjts, function(x) window(x, start = c(6,8760-47))))
  testM <<-cbind(tsTest, notstest)
}
splitMvar()

library(forcats)
hoursToDayNight <- function(df){
  df[["hour"]] %>% 
    fct_collapse(
                 night =  c(as.character(18:23), as.character(0:5)), 
                 day = as.character(6:17)) %>% as.numeric %>% `-`(1)
}
trainM$dayNight <- hoursToDayNight(trainM) 
testM$dayNight <- hoursToDayNight(testM)
names(trainM)
#  [1] "PM_Dongsi"       "PM_Dongsihuan"   "PM_Nongzhanguan" "PM_US.Post"     
#  [5] "DEWP"            "HUMI"            "PRES"            "TEMP"           
#  [9] "Iws"             "precipitation"   "Iprec"           "No"             
# [13] "year"            "month"           "day"             "hour"           
# [17] "season"          "cbwd"            "dayNight"       
trainM[c(1:3,10:18)] <- NULL
testM[c(1:3,10:18)] <- NULL
trainM$DEWP <- NULL
testM$DEWP <- NULL

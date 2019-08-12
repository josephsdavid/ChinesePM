library(dplyr)

source("../../R/preprocessing.R")
source("../../R/helpers.R")


china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
ds <- bj2 %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws)
st <- as.Date("2010-1-1")
en <- as.Date("2014-12-31")
dts <- seq(st, en, by = "day")
ds$date <- dts

write.csv(ds,"gluonSet.csv", row.names = FALSE)


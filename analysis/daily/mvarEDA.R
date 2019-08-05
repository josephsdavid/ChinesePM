library(tidyverse)
library(ggthemes)


source("../../R/helpers.R")
source("../../R/preprocessing.R")

china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
traints <- as.list(keep(train,is.ts))

plotAllTs <- function(df){
    df%>%keep(is.ts) %>% 
    gather() %>% mutate(.data = .,t = rep( 1:nrow(df),(nrow(.)/nrow(df)))) %>%
    ggplot(aes(x = t, y = value)) +
    facet_wrap(~ key, scales = "free") +
    geom_line() 
}
plotAllTs(train)
colnames(train)
#  [1] "PM_Dongsi"       "PM_Dongsihuan"   "PM_Nongzhanguan" "PM_US.Post"     
#  [5] "DEWP"            "HUMI"            "PRES"            "TEMP"           
#  [9] "Iws"             "precipitation"   "Iprec"           "No"             
# [13] "year"            "month"           "day"             "hour"           
# [17] "season"          "cbwd"           
ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = cbwd)) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few() + 
  scale_color_few(palette = "Dark")

train %>% arrange(cbwd) %>% 
  group_by(cbwd)%>%
  summarise(mean = mean(PM_US.Post))  -> meanwind
meanwind
# # A tibble: 4 x 2
#   cbwd   mean
#   <fct> <dbl>
# 1 cv    3841.
# 2 NE    1630.
# 3 NW    1641.
# 4 SE    2743.
ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = as.factor(month))) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few()

train %>% arrange(month) %>% 
  group_by(month)%>%
  summarise(mean = mean(PM_US.Post))  -> meanmonth
meanmonth
# # A tibble: 12 x 2
#    month  mean
#    <int> <dbl>
#  1     1 2642.
#  2     2 2697.
#  3     3 2313.
#  4     4 1900.
#  5     5 1987.
#  6     6 2624.
#  7     7 2280.
#  8     8 2280.
#  9     9 2217.
# 10    10 2775.
# 11    11 2518.
# 12    12 2410.


ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = as.factor(year))) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few()

train %>% arrange(year) %>% 
  group_by(year)%>%
  summarise(mean = mean(PM_US.Post))  -> meanyr
meanyr
# # A tibble: 4 x 2
#    year  mean
#   <int> <dbl>
# 1  2010 2561.
# 2  2011 2410.
# 3  2012 2158.
# 4  2013 2414.

ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = as.factor(season))) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few()

train %>% arrange(season) %>% 
  group_by(season)%>%
  summarise(mean = mean(PM_US.Post))  -> meansea
meansea

plot_vs_response <- function(x){
  plot(train$PM_US.Post ~ train[[x]], xlab = x)
  lw1 <- loess(train$PM_US.Post ~ train[[x]])
  j <- order(train[[x]])
  lines(train[[x]][j],lw1$fitted[j],col="red",lwd=3)
}

train %>% keep(is.numeric) %>% names -> numNames
numNames <- numNames[-c(1:4, 11:17)] 
length(numNames)
par(mfrow = c(3,2))
lapply(numNames, plot_vs_response)
ppm <- train$PM_US.Post
ccfplot <- function(x){
  ccf(ppm,train[[x]],main = x)
}
ccfs <- lapply(numNames,ccfplot)

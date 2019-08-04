library(tidyverse)
library(ggthemes)


source("../../R/helpers.R")
source("../../R/preprocessing.R")

china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
traints <- as.list(keep(train,is.ts))

plotfun <- function(xs){
  p  <- ggplot()
}


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
ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = as.factor(month))) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few()

train %>% arrange(month) %>% 
  group_by(month)%>%
  summarise(mean = mean(PM_US.Post))  -> meanmonth
meanmonth


ggplot(train ) + 
  geom_point(aes(x = No, y = PM_US.Post, color = as.factor(year))) + 
  geom_line(aes( x = No, y = PM_US.Post, alpha = 0.1 )) + 
  geom_smooth(aes(x = No, y = PM_US.Post), method = "auto") +
  theme_few()

train %>% arrange(year) %>% 
  group_by(year)%>%
  summarise(mean = mean(PM_US.Post))  -> meanyr
meanyr

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
numNames
#  [1] "PM_Dongsi"       "PM_Dongsihuan"   "PM_Nongzhanguan" "PM_US.Post"     
#  [5] "DEWP"            "HUMI"            "PRES"            "TEMP"           
#  [9] "Iws"             "precipitation"   "Iprec"           "No"             
# [13] "year"            "month"           "day"             "hour"           
# [17] "season"         
numNames <- numNames[-c(4, 11:17)] 
length(numNames)
par(mfrow = c(3,3))
lapply(numNames, plot_vs_response)

train %>% keep(is.numeric) %>% names -> numNames
numNames <- numNames[-c(1:4, 11:17)] 
length(numNames)
par(mfrow = c(3,2))
lapply(numNames, plot_vs_response)

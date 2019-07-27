library(tswgewrapped)
library(ggplot2)
source("preprocessing.R")
dta <- preprocess('../data/')
ls(dta)
# [1] "BeijingPM_"   "ChengduPM_"   "GuangzhouPM_" "ShanghaiPM_"  "ShenyangPM_" 
beijing <- dta$BeijingPM_
names(beijing)
#  [1] "No"              "year"            "month"           "day"            
#  [5] "hour"            "season"          "PM_Dongsi"       "PM_Dongsihuan"  
#  [9] "PM_Nongzhanguan" "PM_US Post"      "DEWP"            "HUMI"           
# [13] "PRES"            "TEMP"            "cbwd"            "Iws"            
# [17] "precipitation"   "Iprec"          
bjts <- beijing$PM_Nongzhanguan
bjts %>>% forecast::tsclean() %>>% abs -> bjts
plot(decompose(bjts))
plotts.sample.wge(bjts)
# this shows the same thing
forecast::gglagplot(bjts)
forecast::ggAcf(bjts)
par(mfrow=c(1,1))
acf(bjts)
difference(arima, bjts, 1) # this is because I imputed so many NAs, the last 30,000 values should be good
difference(arima, abs( forecast::tsclean( beijing$PM_US ) ),1)

aic5(bjts, silent = F)

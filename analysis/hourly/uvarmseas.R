source("../R/helpers.R")
source("../R/preprocessing.R")
library(forecast)
library(ggplot2)
library(cowplot)
china <- preprocess("../data/")
names(china)
# [1] "ChengduPM_"   "ShenyangPM_"  "ShanghaiPM_"  "BeijingPM_"   "GuangzhouPM_"

beij <- china$BeijingPM_

bj <- beij$PM_US %>>% tsclean %>>% abs %>>% 
  msts(seasonal.periods = c(24, (24*7), (24*365.25)))

train <-  window(bj, end = 6)



p <- autoplot(bj,ylab = "ppm" , xlab = "hour") 
bj %>>% mstl %>>% autoplot
# bjbats <- tbats(bj)
# save our model
# save(bjbats, file = "bjbats.Rda")
load("bjbats.Rda")
plot(bjbats)
summary(bjbats)
#                   Length  Class  Mode     
# lambda                  1 -none- numeric  
# alpha                   1 -none- numeric  
# beta                    1 -none- numeric  
# damping.parameter       1 -none- numeric  
# gamma.one.values        3 -none- numeric  
# gamma.two.values        3 -none- numeric  
# ar.coefficients         0 -none- NULL     
# ma.coefficients         0 -none- NULL     
# likelihood              1 -none- numeric  
# optim.return.code       1 -none- numeric  
# variance                1 -none- numeric  
# AIC                     1 -none- numeric  
# parameters              2 -none- list     
# seed.states            24 -none- numeric  
# fitted.values       52584 msts   numeric  
# errors              52584 msts   numeric  
# x                 1262016 -none- numeric  
# seasonal.periods        3 -none- numeric  
# k.vector                3 -none- numeric  
# y                   52584 msts   numeric  
# p                       1 -none- numeric  
# q                       1 -none- numeric  
# call                    2 -none- call     
# series                  1 -none- character
# method                  1 -none- character

bjpred <- predict(bjbats, h = 150)
plot(bjpred, include = 365)

# Now lets try out harmonic regression

# bjtest <- auto.arima(train, seasonal = F, lambda = 0, 
#                      xreg = fourier(train, K = rep(10,3)))
# save(bjtest, file = "bjtest.Rda")
load("bjharm.Rda")
bjfore <- forecast(
                   bjharm,
                   xreg = fourier(bj, K = rep(10,3)),
                   h = 365*24*3
)
bjfore %>>% fitted %>>% str
results <- fitted(bjfore)
fit <- results[(length(results)-(365*24*3+1)):length(results)]

fits <- data.frame(ppm = fit, type = "predicted")
acts <- data.frame(ppm = as.numeric(bj), type = "observed")
df  <- rbind(acts, fits)
df$t <- seq_along(df$ppm)
library(ggthemes)
ggplot(df) + geom_line(aes(x = t, y = ppm, color = type)) + scale_color_few(palette = "Dark") + theme_tufte()

# todo: ASE, combo forecasts
test <- window(bj, start = 6)
length(test)
load("bjtest.Rda")
bjfor.test <- Arima(test, model = bjtest, 
                    xreg = fourier(
                                   test,
                                   K = rep(10,3)
                                   ))

getASE(bjfor.test)
# [1] 416.4127
bjpredt <- bjfore %>>% forecast(h = length(test))
str(fitted( bjpredt ))
fitted(bjpredt) %>>%
  autoplot + autolayer(test)

trainf <- data.frame(ppm = train, type = "actual")
predf <- data.frame(ppm = as.numeric( fitted(bjfor.test) ), 
                    type = "predicted",
                    t = seq_along(fitted(bjfor.test)))

testf <- data.frame(ppm = as.numeric(test), 
                    type = "actual",
                    t = seq_along(test))

predVact <- list(testf, predf)
p <- ggplot() + theme_economist() + scale_color_few(palette = "Dark" )
doplot <- function(df){
  p <<- p + geom_line(data = df,
                      aes(
                          x = t,
                          y = ppm,
                          color = type
                          ))
}
out <- lapply(predVact, doplot)


out
# testPredPlot <- function(xs){
# 	p <- ggplot() + theme_economist() + scale_color_few(palette = "Dark" )
# 
# 	doplot <- function(df){
# 		p <<- p + geom_line(data = df,
# 				    aes(
# 					x = t,
# 					y = ppm,
# 					color = type
# 					))
# 	}
# 	lapply(xs, doplot)
# 
# }


testPredPlot(predVact)

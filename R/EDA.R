library(tswgewrapped)
library(ggthemes)
library(ggplot2)
source("preprocessing.R")

# function definition

# fix up outliers
clean <- forecast::tsclean

# Generator function to fix up sampling rate to something reasonable

change_samples <- function(n){
	function(xs){
		out <- unname(tapply(
				     xs,
				     (seq_along(xs)-1) %/% n,
				     sum
				     ))
		out <- ts(out, frequency = (8760/n))
		out
	}
}

# daily and weekly sampling, monthly is 4 weeks
to_daily <- change_samples(24)
to_weekly <- change_samples(24*7)
to_monthly <- change_samples(24*7*4)
to_season <- change_samples(24*(365/4))

# pipelining final cleaning and conversion, removing crappy NA
cleandays <- function(xs) {
	xs %>>% clean %>>% abs %>>% to_daily
}

cleanweeks <- function(xs) {
	xs %>>% clean %>>% abs %>>% to_weekly
}
cleanmonths <- function(xs) {
	xs %>>% clean %>>% abs %>>% to_monthly
}
cleanseas <- function(xs) {
	xs %>>% clean %>>% abs %>>% to_season
}

# some plts
seasonplot <- forecast::ggseasonplot
subseriesplot <- forecast::ggsubseriesplot
lagplot <- forecast::gglagplot

# data import
# imports the data as a hash table
fine_china <- preprocess("../data/")
names(fine_china)
# [1] "ChengduPM_"   "ShenyangPM_"  "ShanghaiPM_"  "BeijingPM_"   "GuangzhouPM_"
# <environment: 0x7bf3c10>

# Beijing US Post
fine_china$BeijingPM_$PM_US %>>% cleandays %>>% window(end = 6) -> bjUS_day
fine_china$BeijingPM_$PM_US %>>% cleanweeks %>>% window(end = 6) -> bjUS_week
fine_china$BeijingPM_$PM_US %>>% cleanmonths %>>% window(end = 6) -> bjUS_month
fine_china$BeijingPM_$PM_US %>>% cleanseas %>>% window(end = 6) -> bjUS_seas

plotts.sample.wge(bjUS_day)
plotts.sample.wge(bjUS_week)
plotts.sample.wge(bjUS_month)
plotts.sample.wge(bjUS_seas)
decompose(bjUS_day, "multiplicative") %>>% autoplot+ theme_economist()
decompose(bjUS_day,   "additive") %>>% autoplot+ theme_economist()
decompose(bjUS_week, "multiplicative") %>>% autoplot+ theme_economist()
decompose(bjUS_week,  "additive") %>>% autoplot+ theme_economist()
decompose(bjUS_month, "multiplicative") %>>% autoplot+ theme_economist()
decompose(bjUS_month, "additive") %>>% autoplot+ theme_economist()
bjUS_week %>>%  lagplot+ theme_economist()
bjUS_day %>>% seasonplot + theme_economist()
bjUS_day %>>% seasonplot(polar = TRUE) + theme_economist()
bjUS_week %>>% seasonplot+ theme_economist()
bjUS_month %>>% seasonplot+ theme_economist()
bjUS_month %>>% seasonplot(polar = T)+ theme_economist()
bjUS_seas %>>% seasonplot(polar = T) + theme_economist()
bjUS_seas %>>% seasonplot + theme_economist()

# Years 1 and 2 are a bit odd, so we mmight try windowing by 3

#  next lets look at shanghai

shang_US <- fine_china$ShanghaiPM_$PM_US
resample <- function(xs){
	xs %>>% cleandays %>>% window(start = 3,end = 6) -> day
	xs %>>% cleanweeks %>>% window(start = 3,end = 6) -> week
	xs %>>% cleanmonths %>>% window(start = 3,end = 6) -> month
	xs %>>% cleanseas %>>% window(start = 3,end = 6) -> seas
	list(day = day, week = week, month = month, season = seas)
}
usShang <- resample(shang_US)
plotts.sample.wge(usShang$day)
plotts.sample.wge(usShang$week)
plotts.sample.wge(usShang$month)
plotts.sample.wge(usShang$sea)
decompose(usShang$day, "multiplicative") %>>% autoplot+ theme_economist()
decompose(usShang$day,   "additive") %>>% autoplot+ theme_economist()
decompose(usShang$week, "multiplicative") %>>% autoplot+ theme_economist()
decompose(usShang$week,  "additive") %>>% autoplot+ theme_economist()
decompose(usShang$month, "multiplicative") %>>% autoplot+ theme_economist()
decompose(usShang$month, "multiplicative") %>>% autoplot+ theme_economist()
decompose(usShang$sea, "additive") %>>% autoplot+ theme_economist()
decompose(usShang$sea, "additive") %>>% autoplot+ theme_economist()
usShang$week %>>%  lagplot+ theme_economist()
usShang$day %>>% seasonplot + theme_economist()
usShang$day %>>% seasonplot(polar = TRUE) + theme_economist()
usShang$week %>>% seasonplot+ theme_economist()
usShang$week %>>% seasonplot(polar = T)+ theme_economist()
usShang$month %>>% seasonplot+ theme_economist()
usShang$month %>>% seasonplot(polar = T)+ theme_economist()
usShang$seas %>>% seasonplot(polar = T) + theme_economist()
usShang$seas %>>% seasonplot + theme_economist()

# We see with the weekly plot we have a lot of seasonality


# lets just for fun do some predictions with the daily data

shang <- usShang$day
plotts.sample.wge(shang)
# we have clear seasonality, and maybe a wandering behavior. I believe we have a biannual seasonality, based off of the monthly graph
shang %>>% ( difference(seasonal,., (365)) ) -> shang2
difference(arima, shang2, 1) -> shang3
shang3 %>>% aicbic(p=0:10)-> aics
pander(aics)
# 
# 
#   *
# 
#     ------------------------
#      &nbsp;   p   q    aic
#     -------- --- --- -------
#      **20**   3   1   13.51
# 
#      **6**    0   5   13.52
# 
#      **4**    0   3   13.52
# 
#      **10**   1   3   13.52
# 
#      **3**    0   2   13.55
#     ------------------------
# 
#   *
# 
#     ------------------------
#      &nbsp;   p   q    bic
#     -------- --- --- -------
#      **20**   3   1   13.54
# 
#      **4**    0   3   13.55
# 
#      **6**    0   5   13.55
# 
#      **10**   1   3   13.56
# 
#      **3**    0   2   13.57
#     ------------------------
# 
# 
# <!-- end of list -->
# 
# 
# NULL
shang %>>% ( difference(seasonal,., 365) ) %>>% aicbic(p=0:10) -> aics 
pander(aics)
# 
# 
#   *
# 
#     -----------------------
#      &nbsp;   p   q   aic
#     -------- --- --- ------
#      **20**   3   1   13.5
# 
#      **11**   1   4   13.5
# 
#      **26**   4   1   13.5
# 
#      **16**   2   3   13.5
# 
#      **24**   3   5   13.5
#     -----------------------
# 
#   *
# 
#     ------------------------
#      &nbsp;   p   q    bic
#     -------- --- --- -------
#      **13**   2   0   13.53
# 
#      **3**    0   2   13.53
# 
#      **8**    1   1   13.53
# 
#      **7**    1   0   13.53
# 
#      **20**   3   1   13.53
#     ------------------------
# 
# 
# <!-- end of list -->
# 
# 
# NULL
par(mfrow = c(1,1))
est_shang <- estimate(shang2, p=2, q = 0)
acf(est_shang$res)
ljung_box(est_shang$res, p =2, q =0)
fore_and_assess <- function(...){
	f <- forecast(...)
	out <- assess(..., plot = FALSE)
	f$ASE <- out
	f
}
shang_seasonal <- fore_and_assess(type = aruma,
				  x = shang,
				  s = 365,
				  phi = est_shang$phi,
				  n.ahead = 24,
				  limits = F
)

est_shang2 <- estimate(shang3, p = 3, q = 1)
acf(est_shang2$res)
ljung_box(est_shang2$res, 3, 1)
#            [,1]             [,2]            
# test       "Ljung-Box test" "Ljung-Box test"
# K          24               48              
# chi.square 14.14806         35.92178        
# df         20               44              
# pval       0.8229101        0.8017901       

shang_aruma <- fore_and_assess(type = aruma,
				  x = shang,
				  s = 365,
				  d = 1,
				  phi = est_shang2$phi,
				  theta = est_shang2$theta,
				  n.ahead = 24,
				  limits = F
)

shang_seasonal$ASE
# [1] 1154198
shang_aruma$ASE
# [1] 1154911
test <- window(shang_US, start = 7)[1:24]
shang_aruma$f-> pred
test
ase(test, shang_aruma)
# [1] 1977888
ase(test, shang_seasonal)
# [1] 3278672

# ok looking damn good with the shang aruma

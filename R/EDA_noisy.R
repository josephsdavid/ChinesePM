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

# some plts
seasonplot <- forecast::ggseasonplot
subseriesplot <- forecast::ggsubseriesplot
lagplot <- forecast::gglagplot

# data import
# imports the data as a hash table
fine_china <- preprocess("../data/")
# <environment: 0x7bf3c10>

# Beijing US Post
fine_china$BeijingPM_$PM_US %>>% cleandays  -> bjUS_day
fine_china$BeijingPM_$PM_US %>>% cleanweeks -> bjUS_week
fine_china$BeijingPM_$PM_US %>>% cleanmonths -> bjUS_month
plotts.sample.wge(bjUS_day)
plotts.sample.wge(bjUS_week)
plotts.sample.wge(bjUS_month)
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
bjUS_week %>>% seasonplot(polar = TRUE)+ theme_economist()
bjUS_month %>>% seasonplot+ theme_economist()
bjUS_month %>>% seasonplot(polar = T)+ theme_economist()

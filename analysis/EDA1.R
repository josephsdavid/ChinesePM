library(tswgewrapped)
library(ggthemes)
library(ggplot2)
library(cowplot)
source("../R/preprocessing.R", echo = TRUE)
source("../R/helpers.R", echo = TRUE)


# data import
# imports the data as a hash table
fine_china <- preprocess("../data/")
names(fine_china)
# [1] "ChengduPM_"   "ShenyangPM_"  "ShanghaiPM_"  "BeijingPM_"   "GuangzhouPM_"
# <environment: 0x7bf3c10>


# Years 1 and 2 are a bit odd, so we mmight try windowing by 3

#  next lets look at shanghai

shang_US <- fine_china$ShanghaiPM_$PM_US

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
sda <- usShang$day %>>% seasonplot + theme_economist()
usShang$day %>>% seasonplot(polar = TRUE) + theme_economist()
sdw <- usShang$week %>>% seasonplot+ theme_economist()
usShang$week %>>% seasonplot(polar = T)+ theme_economist()
sdm <- usShang$month %>>% seasonplot+ theme_economist()
usShang$month %>>% seasonplot(polar = T)+ theme_economist()
usShang$seas %>>% seasonplot(polar = T) + theme_economist()
sds <- usShang$seas %>>% seasonplot + theme_economist()
titles <- c("Seasonal plot: Daily", "Seasonal plot: weekly", "Seasonal plot: monthly", "Seasonal plot: quarterly")
plot_grid(sda, sdw, sdm, sds, ncol=2,titles=titles )


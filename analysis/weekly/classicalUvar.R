source("setup.R")
library(tswgewrapped)
library(ggplot2)
library(ggthemes)

# Sample wge plots

plotts.sample.wge(train)
autoplot(train)

# seasonal differencing

train52 <- difference(seasonal, train, 52)

# normal

aics <- aicbic(train)
pander(aics)

# this looks a bit nicer, lets check out aics

aic52 <- aicbic(train52, p = 0:55, q = 0:8)
pander(aic52)

save(aic52, aics, file = "aics.Rda")


# estimating parameters


estn <- estimate(train, p = 3, q = 2)

est52 <- estimate(train52, p = 0, q = 1)

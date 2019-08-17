source("mvarsetup.R")
library(forecast)
library(ggplot2)
library(ggthemes)

# lets make the model

nnet <- nnetar(y = trainM[[1]], xreg = trainM[-1], scale.inputs = TRUE, repeats = 100, lambda = 0)
nnetfor <- forecast(nnet, xreg = trainM[(nrow(trainM)-71):nrow(trainM),-1], PI = TRUE)
save(nnet, nnetfor,file = "nnetar.Rda")
nnet$lags
nnet$p
# [1] 25
nnet$P
nnet$model
autoplot(nnetfor)
length(nnetfor$mean)
autoplot(as.nfor(nnetfor))
scores(as.nfor(nnetfor))
#        ASE       MAPE Conf.Score 
# 19847.9460   214.3944     6.0000 


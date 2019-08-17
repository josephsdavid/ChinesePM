source("mvarsetup.R")
library(vars)
library(forecast)
summary(lm(trainM))
# 
# Call:
# lm(formula = trainM)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -175.56  -50.10  -13.17   32.25  378.96 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.313e+03  7.936e+01  16.538  < 2e-16 ***
# DEWP         5.740e-01  1.715e-01   3.348 0.000816 ***
# HUMI         9.716e-01  5.454e-02  17.812  < 2e-16 ***
# PRES        -1.212e+00  7.722e-02 -15.697  < 2e-16 ***
# TEMP        -3.172e+00  1.693e-01 -18.734  < 2e-16 ***
# Iws         -2.815e-01  9.344e-03 -30.125  < 2e-16 ***
# dayNight     8.396e+00  8.681e-01   9.671  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 76.63 on 34985 degrees of freedom
# Multiple R-squared:  0.2303,	Adjusted R-squared:  0.2302 
# F-statistic:  1745 on 6 and 34985 DF,  p-value: < 2.2e-16
# 
vartrain <- trainM %>% dplyr::select(-c(dayNight,DEWP))
varexo <- matrix(trainM$dayNight, dimnames = list(NULL, "dayNight"))
varf <- fourier(
                msts(vartrain$PM_US, seasonal.periods = c(24,24*7,8760)),
                K = c(10,20,100)
)
varexof <- cbind(varexo,varf)
str(varexof)
makeExo <- function(n){
  day <- c(rep(0,5), rep(1,12), rep(0,7))
  return(matrix(rep(day,n), dimnames = list(NULL, "dayNight")))
}

makeExof <- function(n) {
  days <- makeExo(n)
  fours <- varf[1:(24*n),]
  cbind(days,fours)
}
str(makeExof(3))
ord <- VARselect(vartrain, lag.max = 100, exogen = varexo)
ord$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#     78     30     26     78  # AIC is ridiculous, SC or HQ

oVar <- VAR(vartrain, p = 30, exogen = varexo, type = "both")
opred <- predict(oVar, n.ahead = 72, dumvar = makeExo(3))
scores(as.var(opred))
#        ASE       MAPE Conf.Score 
# 19661.5791   350.2365    16.0000 
#        ASE       MAPE Conf.Score 
# 19796.3799   367.8633    15.0000 

ordf <- VARselect(vartrain, lag.max = 100, exogen = varexof)

ordf$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#     54     26      5     54 

fVar <- VAR(vartrain, p = 26, exogen = unname(varexof))
fpred <- predict(fVar, n.ahead = 72, dumvar = unname(makeExof(3)))
autoplot(as.var(fpred))
#        ASE       MAPE Conf.Score 
# 21969.5817   206.4883    21.0000 

# the forecasts look entirely the same, but one is fishy and untested and the other isnt. We will go with the one we know is correct, but it was an interesting experiment

save(ord, opred, file = "VAR.Rda")

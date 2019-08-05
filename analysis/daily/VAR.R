library(doParallel)
library(foreach)
library(dplyr)
library(latticeExtra)
library(ggthemes)
library(ggplot2)
library(vars)
library(forecast)


source("../../R/helpers.R")
source("../../R/preprocessing.R")

china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
names(train)
#  [1] "PM_Dongsi"       "PM_Dongsihuan"   "PM_Nongzhanguan" "PM_US.Post"     
#  [5] "DEWP"            "HUMI"            "PRES"            "TEMP"           
#  [9] "Iws"             "precipitation"   "Iprec"           "No"             
# [13] "year"            "month"           "day"             "hour"           
# [17] "season"          "cbwd"           
train[1:3]  <- NULL
train[9:14] <- NULL
test[1:3]  <- NULL
test[9:14] <- NULL
names(test)
traints <- (purrr::keep(train,is.ts))
trainexogen <- purrr::discard(train, is.ts)
trainexogen <- lapply(trainexogen, as.factor) %>>% as.data.frame

names(traints)
# [1] "PM_US.Post"    "DEWP"          "HUMI"          "PRES"          "TEMP"         
# [6] "Iws"           "precipitation" "Iprec"        
names(trainexogen)

# simple order
ord <- VARselect(traints, lag.max = 20, type = c("both"))
ords <- VARselect(traints, lag.max = 20, type = "both", season = c(7,365))
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#      8      3      1      8 
ord$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#      8      3      1      8 
ordn <- VARselect(traints, lag.max = 20)
ordn$selection
# AIC(n)  HQ(n)  SC(n) FPE(n) 
#      8      3      1      8 

# making the models
types <- c("const","trend","both","none")
ics <- c("AIC", "HQ", "SC", "FPE")
ps <- c(8,3,1)
szn <- c(NULL, 365)
init <- list(ps = ps, types = types, ics = ics, szn = szn)
grid <- expand.grid(init, stringsAsFactors = FALSE)
# grid search of VAR
workers <- makeCluster(11L)
invisible(clusterEvalQ(workers, library(vars)))
registerDoParallel(workers)

gridSearch <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  model <- VAR(traints,p = grid[i,1], type = grid[i,2], ic = grid[i,3], season = grid[i,4])
  preds <- predict(model, n.ahead = 366)
  pred <- preds$fcst$PM_US.Post[,1]
  ASE <- mean( (test$PM_US.Post-pred)^2 )
  c(ASE = ASE, p = grid[i,1], type = grid[i,2], ic = grid[i,3], season = grid[i,4])
}
load("varsear.Rda")
gridSearch %>% as_tibble -> gridSearch
gridSearch$ASE <- as.numeric(gridSearch$ASE)
gridSearch[2:4] <- lapply(gridSearch[2:4], factor)
gridSearch
stopCluster(workers)
registerDoSEQ()
gridSearch %>% arrange(ASE) 
#                ASE p type  ic season
# 1 3748040.22781357 3 none AIC    365
# 2 3748040.22781357 3 none  HQ    365
# 3 3748040.22781357 3 none  SC    365
# 4 3748040.22781357 3 none FPE    365
# 5 3752913.51274325 1 none AIC    365
# 6 3752913.51274325 1 none  HQ    365
gridV <-  cloud(ASE ~ p + type, 
       gridSearch,
       panel.3d.cloud = panel.3dbars, 
       scales = list(arrows = F, col = 1), 
       xbase= 0.2, ybase = 0.2,  pretty = T,
       col.facet = level.colors(gridSearch$ASE, 
          at = do.breaks(range(gridSearch$ASE), 20),
          col.regions = terrain.colors),
       colorkey = list(
                       col = terrain.colors, 
                       at = do.breaks(range(gridSearch$ASE), 
                                      20)),
       screen = list( z = 40, x = -30 )
 )

ggplot(data = gridSearch) + geom_bar(stat = "identity",aes(x = p, y = ASE, fill = type)) + facet_wrap(. ~ season)
var1 <- VAR(traints, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var1, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )
# [1] 3748146

plot(preds)
lm(data = traints, PM_US.Post ~.) -> thelm
summary(thelm)
# 
# Call:
# lm(formula = PM_US.Post ~ ., data = traints)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3605.8  -904.0  -208.8   606.5  9556.9 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    4.461e+04  7.021e+03   6.354 2.80e-10 ***
# DEWP          -9.597e-01  2.643e-01  -3.632 0.000291 ***
# HUMI           1.908e+00  9.242e-02  20.645  < 2e-16 ***
# PRES          -1.757e+00  2.839e-01  -6.189 7.88e-10 ***
# TEMP          -3.425e+00  3.395e-01 -10.088  < 2e-16 ***
# Iws           -1.151e+00  1.656e-01  -6.948 5.59e-12 ***
# precipitation -1.192e+20  5.488e+19  -2.173 0.029974 *  
# Iprec         -5.304e+19  3.843e+19  -1.380 0.167758    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1454 on 1453 degrees of freedom
# Multiple R-squared:  0.3653,	Adjusted R-squared:  0.3622 
# F-statistic: 119.4 on 7 and 1453 DF,  p-value: < 2.2e-16
# 

traints %>% select(PM_US.Post, HUMI, DEWP, TEMP, precipitation, PRES, Iws) -> trainsimp
var2 <- VAR(trainsimp, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var2, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )
# [1] 3747685

traints %>% select(PM_US.Post, HUMI,  TEMP, precipitation,  PRES, Iws) -> trainsimp2

var3 <- VAR(trainsimp2, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var3, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )
# [1] 3746216

traints %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> trainsimp3

var4 <- VAR(trainsimp3, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var4, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )
# [1] 3745787
plot(preds)
# NULL

autoplot.fore
# function(obj){
#   testdf <- data.frame(type = "actual", 
#                        t = seq_along(test), 
#                        ppm = as.numeric(test))
#   preddf <- data.frame(type = "predicted", 
#                        t = seq_along(test), 
#                        ppm = as.numeric( obj$fitted[1:length(test)] ))
#   dfl <- list(testdf,preddf)
#   testPredPlot(dfl)
# }

vpred <- as.var(preds)
autoplot(vpred)
scores(vpred)


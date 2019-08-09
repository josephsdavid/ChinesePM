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
plot(preds)
summary(lm(data = traints, PM_US.Post ~.)) -> thelm

traints %>% select(PM_US.Post, HUMI, DEWP, TEMP, precipitation, PRES, Iws) -> trainsimp
var2 <- VAR(trainsimp, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var2, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )

traints %>% select(PM_US.Post, HUMI,  TEMP, precipitation,  PRES, Iws) -> trainsimp2

var3 <- VAR(trainsimp2, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var3, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )

traints %>% select(PM_US.Post, HUMI,  TEMP,   PRES, Iws) -> trainsimp3

var4 <- VAR(trainsimp3, p = 3, type = "none", ic = "AIC", season = 365)
preds <- predict(var4, n.ahead = 366)
pred <- preds$fcst$PM_US.Post[,1]
mean( (test$PM_US.Post-pred)^2 )
plot(preds)
vpred <- as.var(preds)
autoplot(vpred)
scores(vpred)


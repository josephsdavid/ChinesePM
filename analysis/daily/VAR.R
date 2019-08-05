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
library(doParallel)
library(foreach)
# grid search of VAR
workers <- makeCluster(11L)
invisible(clusterEvalQ(workers, library(vars)))
registerDoParallel(workers)

gridSearch <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  model <- VAR(traints,p = grid[i,1], type = grid[i,2], ic = grid[i,3], season = grid[i,4])
  preds <- predict(model, n.ahead = 365)
  pred <- preds$fcst$PM_US.Post[,1]
  ASE <- mean( (test$PM_US.Post-pred)^2 )
  c(ASE = ASE, p = grid[i,1], type = grid[i,2], ic = grid[i,3])
  }
stopCluster(workers)
registerDoSEQ()
library(dplyr)
gridSearch %>% as.data.frame %>% arrange(ASE) %>% head



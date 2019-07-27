library(tswgewrapped)
library(pipeR)
library(magrittr)
load("../data/clean.Rda")
test <- test_clean
training <- train_clean
rm(list = c("test_clean", "train_clean"))
app <- training$Appliances
plotts.sample.wge(app)
difference(arima, app,1)-> appt
overfit(appt, p = 30)
factor.wge(phi = c(rep(0,11),1))
# 
# Coefficients of Original polynomial:  
# 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000 
# 
# Factor                 Roots                Abs Recip    System Freq 
# 1-1.0000B+1.0000B^2    0.5000+-0.8660i      1.0000       0.1667
# 1-1.0000B              1.0000               1.0000       0.0000
# 1-1.7321B+1.0000B^2    0.8660+-0.5000i      1.0000       0.0833
# 1+1.0000B+1.0000B^2   -0.5000+-0.8660i      1.0000       0.3333
# 1-0.0000B+1.0000B^2    0.0000+-1.0000i      1.0000       0.2500
# 1+1.7321B+1.0000B^2   -0.8660+-0.5000i      1.0000       0.4167
# 1+1.0000B             -1.0000               1.0000       0.5000
#   
#   
# NULL

factor.wge(phi = c(rep(0,23),1))
# clear sign of daily (2 hours each so 12 cycles) seaosnality
 
difference(arima, app, 1) %>>% (difference(seasonal, ., 12)) -> appts

clust <- makeCluster(2L, type = "FORK")
aicbic(appts, p = 0:20, q = 0:10 )
aic5.wge(appts, p = 0:30, type = "aicc")
aic5
# function (x, p = 0:8, q = 0:5, type = "aic", silent = TRUE) 
# {
#     ip <- expand(p, q)
#     iq <- rewrite(q, p)
#     out <- mapply(function(v1, v2) getpq(x, v1, v2, type, silent), 
#         ip, iq)
#     out <- (as.data.frame(t(out)))
#     colnames(out) <- c("p", "q", type)
#     head(out[order(out[, 3], decreasing = F), ], 5)
# }
# <bytecode: 0x50bd528>
# <environment: namespace:tswgewrapped>

expand <- function(v1,v2){
	c(sapply(v1, function (x) rep(x, length(v2)) ))
}

rewrite <- function(v1, v2){
	rep(v1, length(vx))
}
getpq <- function(x, p=8,q=5, type = "aic", silent = TRUE){
	if (silent == FALSE){
		cat("Calculating ",type," for ARMA(",p,", ", q," )\n", sep = "")
	}
	res <- try(aic.wge(x, p,q,type))
	if (is.list(res)) {
		out <- c(res$p, res$q, res$value)
	} else {
		out <- c(p,q,9999)
	}
	out
}
library(doParallel)
aic5par <- function(x, p, q, type = "aic"){
	out <- foreach(k = 0:p, .combine = rbind) %:%
		foreach(j = 0:q, .combine = rbind) %dopar% {
			 getpq(x, k, j, type, silent = TRUE)
		}
     out <- (as.data.frame((out)))
     colnames(out) <- c("p", "q", type)
     rownames(out) <- NULL
     head(out[order(out[, 3], decreasing = F), ], 5)
}
xs <- aic5par(appts, 30,10)

xs
str(xs)

difference(arima, app, 1) %>>% (difference(seasonal, ., 84)) -> appts
aicbic(appts, p = 0:18)

est <- estimate(appts, p = 0, q = 2)
est

forecast(type = aruma,x = app, s = 84, d = 1, phi = 0, theta = est$theta, n.ahead = 200)
t <- ljung_box(est$res, p = 0, q = 2)
plot_res(est$res)

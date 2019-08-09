library(functional) # to compose the preprocessing pipeline
library(data.table)# to read csvs
library(rlist) # for list manipulations
library(pipeR) # fast, dumb pipes
library(imputeTS) # to impute NAs
library(pander) # so i can read the output
library(foreach) # go fast
library(doParallel) # go fast

# Data import
#  datadir <- "../data/"
# function which imports the data as a list, then fixes up the names to be nice
import <- function(path){
  files <- list.files(path)
  files <- files[grepl(files, pattern = ".csv")]
  filepaths <- sapply(files, function(x) paste0(path,x))
  out <- lapply(filepaths, fread)
  fnames <- gsub(".csv","",files)
  fnames <- gsub("[[:digit:]]+","", fnames)
  names(out) <- fnames
  out
}
#  datas <- import(datadir)

naCount <- function(xs){
  rapply(xs, function(x) sum(is.na(x)/length(x)), how = "list")
}
# pander(naCount(datas))
# 
# 
#   * **BeijingPM_**:
# 
#       * **No**: _0_
#       * **year**: _0_
#       * **month**: _0_
#       * **day**: _0_
#       * **hour**: _0_
#       * **season**: _0_
#       * **PM_Dongsi**: _0.5236_
#       * **PM_Dongsihuan**: _0.61_
#       * **PM_Nongzhanguan**: _0.5259_
#       * **PM_US Post**: _0.04178_
#       * **DEWP**: _9.509e-05_
#       * **HUMI**: _0.006447_
#       * **PRES**: _0.006447_
#       * **TEMP**: _9.509e-05_
#       * **cbwd**: _9.509e-05_
#       * **Iws**: _9.509e-05_
#       * **precipitation**: _0.009204_
#       * **Iprec**: _0.009204_
# 
#   * **ChengduPM_**:
# 
#       * **No**: _0_
#       * **year**: _0_
#       * **month**: _0_
#       * **day**: _0_
#       * **hour**: _0_
#       * **season**: _0_
#       * **PM_Caotangsi**: _0.5356_
#       * **PM_Shahepu**: _0.5323_
#       * **PM_US Post**: _0.4504_
#       * **DEWP**: _0.01006_
#       * **HUMI**: _0.01017_
#       * **PRES**: _0.009908_
#       * **TEMP**: _0.01002_
#       * **cbwd**: _0.009908_
#       * **Iws**: _0.01014_
#       * **precipitation**: _0.0562_
#       * **Iprec**: _0.0562_
# 
#   * **GuangzhouPM_**:
# 
#       * **No**: _0_
#       * **year**: _0_
#       * **month**: _0_
#       * **day**: _0_
#       * **hour**: _0_
#       * **season**: _1.902e-05_
#       * **PM_City Station**: _0.3848_
#       * **PM_5th Middle School**: _0.5988_
#       * **PM_US Post**: _0.3848_
#       * **DEWP**: _1.902e-05_
#       * **HUMI**: _1.902e-05_
#       * **PRES**: _1.902e-05_
#       * **TEMP**: _1.902e-05_
#       * **cbwd**: _1.902e-05_
#       * **Iws**: _1.902e-05_
#       * **precipitation**: _1.902e-05_
#       * **Iprec**: _1.902e-05_
# 
#   * **ShanghaiPM_**:
# 
#       * **No**: _0_
#       * **year**: _0_
#       * **month**: _0_
#       * **day**: _0_
#       * **hour**: _0_
#       * **season**: _0_
#       * **PM_Jingan**: _0.5303_
#       * **PM_US Post**: _0.3527_
#       * **PM_Xuhui**: _0.521_
#       * **DEWP**: _0.0002472_
#       * **HUMI**: _0.0002472_
#       * **PRES**: _0.0005325_
#       * **TEMP**: _0.0002472_
#       * **cbwd**: _0.0002282_
#       * **Iws**: _0.0002282_
#       * **precipitation**: _0.07624_
#       * **Iprec**: _0.07624_
# 
#   * **ShenyangPM_**:
# 
#       * **No**: _0_
#       * **year**: _0_
#       * **month**: _0_
#       * **day**: _0_
#       * **hour**: _0_
#       * **season**: _0_
#       * **PM_Taiyuanjie**: _0.5362_
#       * **PM_US Post**: _0.5877_
#       * **PM_Xiaoheyan**: _0.5317_
#       * **DEWP**: _0.01316_
#       * **HUMI**: _0.01293_
#       * **PRES**: _0.01316_
#       * **TEMP**: _0.01316_
#       * **cbwd**: _0.01316_
#       * **Iws**: _0.01316_
#       * **precipitation**: _0.2427_
#       * **Iprec**: _0.2427_
# 
# 
# <!-- end of list -->
# 
# 
# NULL

# convert a vector to a time series with the proper frequency
tots <- function(v){
  ts(v, frequency = 365*24)
}
# tots(datas[[1]]$PM_US) %>>% tail

# convert a data frame into a list of time series objects, given column names
totslist <- function(df){
  badlist <- c(
               "No",
               "year",
               "month",
               "day",
               "hour",
               "season",
               "cbwd"
  )
  nms <- colnames(df)
  df <- as.list(df)
  for (name in nms){
    if (name %in% badlist){
      df[[name]] <- df[[name]]
    } else {
      df[[name]]  <- tots(df[[name]])
    }
  }
  df


}
# datas[[1]] %>>% totsdf %>>%str
# datas[[1]] %>>% totslist%>>%str
totsall <- function(xs){
  lapply(xs, totslist)
}
# str(datas[[1]]$PM_US)
# datas %>>% totsall -> datas

# impute NAs of a single list with spline interpolation
# try na.ma but dont fail on error, instead just do standard type checking
# if the output is a time series, impute the NAs, otherwise do nothing
imp_test <- function(v){
  out <- try(na.interpolation(v, "spline"))
  ifelse(
         is.ts(out),
         return(out),
         return(v)
  )
}
# impute the NAs of a single list
impute <- function(xs){
  foreach(i = 1:length(xs),
          .final = function(x){
            setNames(x, names(xs))
          }) %dopar% 
  imp_test(xs[[i]])
}
# cl <- makeCluster(11, type = "FORK")
# registerDoParallel(cl)
# na.ma(datas[[1]][["PM_"]], k=200)
# na.interpolation(datas[[1]][["PM_Dongsi"]], "spline") %>>% head
# impute(datas[[1]]) %>>% names

# impute NAs of the parent list
impute_list <- function(xs){
  lapply(xs, impute)
}

# make a fast hash table
to_hash <- function(xs){
  list2env(xs, envir = NULL, hash = TRUE)
}
# final preprocessing function:

preprocess <- Compose(import, totsall, impute_list, to_hash)

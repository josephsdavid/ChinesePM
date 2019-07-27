library(readxl)
library(pipeR)
read_excel("../data/Weather-DataSet/Manaus.xlsx")

readxlsx <- function(path){
	files <- list.files(path)
	files <- files[!grepl(files, pattern =".md")]
	files <- sapply(files,  function(x) paste0(path,x))
	out <- lapply(files, 
		      read_excel, 
		      skip = 4)
	names(out) <- gsub(".xlsx","",names(out))
	out
}
xs <- readxlsx("../data/Weather-DataSet/")
rename <- function(df){
	colnames(df) <- gsub(" ","",colnames(df))
	colnames(df)
	df
}
rename_all <- function(xs){
	lapply(xs,rename)
}
list.files("../data/Weather-DataSet")
wthr <- "../data/Weather-DataSet/"
library(functoinal)
wthr %>>% readxlsx %>>% rename_all
# $Manaus
# # A tibble: 312 x 7
#    Date                Cloudiness Rainfall MaxTemperature MinTemperature Humidity
#    <dttm>                   <dbl>    <dbl>          <dbl>          <dbl>    <dbl>
#  1 1990-01-31 00:00:00       6.59    234.            30.5           23.1     83.8
#  2 1990-02-28 00:00:00       7.57    190             30.3           23.1     83.2
#  3 1990-03-31 00:00:00       7.04    299.            30.5           23.1     84.8
#  4 1990-04-30 00:00:00       6.64    236.            30.3           22.9     84.8
#  5 1990-05-31 00:00:00       6.52    245.            31.2           23.3     82.2
#  6 1990-06-30 00:00:00       5.54     89.3           31.3           23.2     79.4
#  7 1990-07-31 00:00:00       5.69    174.            30.9           23.0     75.9
#  8 1990-08-31 00:00:00       3.97     55.4           31.7           22.9     71.5
#  9 1990-09-30 00:00:00       4.41     39             32.5           23.3     70.4
# 10 1990-10-31 00:00:00       5.14     79.4           32.8           23.6     70.1
# # … with 302 more rows, and 1 more variable: MeanTemperature <dbl>
# 
# $Natal
# # A tibble: 312 x 7
#    Date                Cloudiness Rainfall MaxTemperature MinTemperature Humidity
#    <dttm>                   <dbl>    <dbl>          <dbl>          <dbl>    <dbl>
#  1 1990-01-31 00:00:00       5.63     43.2           30.0           24.5     73.8
#  2 1990-02-28 00:00:00       5.70     17.9           30.2           24.0     75.8
#  3 1990-03-31 00:00:00       5.78     47.1           30.5           24.7     75.7
#  4 1990-04-30 00:00:00       5.98    141.            30.4           24.5     78.4
#  5 1990-05-31 00:00:00       5.63    192.            29.4           23.1     82.3
#  6 1990-06-30 00:00:00       5.91    281.            28.4           22.2     82.9
#  7 1990-07-31 00:00:00       6.03    167.            28.2           21.7     80.9
#  8 1990-08-31 00:00:00       6.58    194.            27.6           21.6     82.0
#  9 1990-09-30 00:00:00       5.74     62.1           28.6           22.4     74.0
# 10 1990-10-31 00:00:00       6.05     22.7           29.2           24.0     74.1
# # … with 302 more rows, and 1 more variable: MeanTemperature <dbl>
# 
# $PortoAlegre
# # A tibble: 312 x 7
#    Date                Cloudiness Rainfall MaxTemperature MinTemperature Humidity
#    <dttm>                   <dbl>    <dbl>          <dbl>          <dbl>    <dbl>
#  1 1990-01-31 00:00:00       1.58     64             31            20.3      70.1
#  2 1990-02-28 00:00:00       2       164.            30.0          20.5      74.7
#  3 1990-03-31 00:00:00       2.09    108             28.5          19.4      76.5
#  4 1990-04-30 00:00:00       2.36    196.            25.5          17.6      82.5
#  5 1990-05-31 00:00:00       2.03     53.6           21.4          11.0      79.8
#  6 1990-06-30 00:00:00       1.99    113.            18.1           8.12     80.4
#  7 1990-07-31 00:00:00       2.11     47.9           17.7           8.75     77.6
#  8 1990-08-31 00:00:00       1.25     20.8           22.3          10.3      71.5
#  9 1990-09-30 00:00:00       2.51    233.            20.2          11.1      77.7
# 10 1990-10-31 00:00:00       2.35    236.            26.3          16.7      76.5
# # … with 302 more rows, and 1 more variable: MeanTemperature <dbl>
# 
# $SaoPaulo
# # A tibble: 312 x 7
#    Date  Cloudineess PrecipitacaoTot… MaxTemperature MeanTemperature MinTemperature
#    <chr>       <dbl>            <dbl>          <dbl>           <dbl>          <dbl>
#  1 32904        4.80            175.            29.7            23.0           17.6
#  2 29/0…        3.67             68.6           30.4            23.8           18.9
#  3 32963        5.01            101.            28.5            22.5           18.1
#  4 32993        5.54             70.5           25.7            20.4           16.6
#  5 33024        3.19             46.6           26.4            20.0           15.2
#  6 33054        2.94              0             25.0            18.3           13.1
#  7 33085        3.38              0             24.9            17.7           12.2
#  8 33116        5.17             32.4           24.0            17.6           12.7
#  9 33146        4.77            114.            25.0            18.5           13.3
# 10 33177        4.63             78.7           28.1            21.7           16.5
# # … with 302 more rows, and 1 more variable: Humidity <dbl>
# 
?read_excel
data(flu)

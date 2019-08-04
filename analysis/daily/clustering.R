library(dtwclust)
library(ggplot2)
library(ggthemes)
library(magrittr)
library(tidyverse)
source("../../R/helpers.R")
source("../../R/preprocessing.R")
china <- preprocess("../../data/")
bj <- (china$BeijingPM_ )
bj2 <- dlist(bj)
dfsplit(bj2)
traints <- as.list(keep(train,is.ts))
load("grids.Rda")
# set up parallel computation
workers <- makeCluster(10L)
invisible(clusterEvalQ(workers, library(dtwclust)))
registerDoParallel(workers)

# partitional clustering grid search in parallel
cfg <- compare_clusterings_configs(
  types = "partitional",
  k = 2L:6L,
  controls = list(
    partitional = partitional_control(iter.max = 40L)
  ),
  distances = pdc_configs("distance",
    partitional = list(
      dtw_basic = list(
        window.size = seq(from = 5L, to = 50L, by = 5L),
        norm = c("L1", "L2")
        )
      )
    ),
  centroids = pdc_configs("centroid",
    share.config = c("p"),
    dba = list(
      window.size = seq(from = 5L, to = 50L, by = 5L),
      norm = c("L1", "L2")
      )
    ),
  no.expand = c("window.size","norm")
)

evaluators <- cvi_evaluators("Sil")


comparison <- compare_clusterings(
  traints, 
  types = "partitional",
  configs = cfg, 
  seed = 666L,
  score.clus = evaluators$score,
  pick.clus = evaluators$pick
)

head(comparison$results$partitional %>% arrange(desc(Sil)))
#    config_id rep k pam.precompute iter.max symmetric version preproc  distance
# 1 config19_1   1 2           TRUE       40     FALSE       2    none dtw_basic
# 2 config15_1   1 2           TRUE       40     FALSE       2    none dtw_basic
# 3 config13_1   1 2           TRUE       40     FALSE       2    none dtw_basic
# 4 config11_1   1 2           TRUE       40     FALSE       2    none dtw_basic
# 5  config9_1   1 2           TRUE       40     FALSE       2    none dtw_basic
# 6  config7_1   1 2           TRUE       40     FALSE       2    none dtw_basic
#   centroid window.size_distance norm_distance window.size_centroid norm_centroid
# 1      dba                   50            L1                   50            L1
# 2      dba                   45            L1                   45            L1
# 3      dba                   40            L1                   40            L1
# 4      dba                   35            L1                   35            L1
# 5      dba                   30            L1                   30            L1
# 6      dba                   25            L1                   25            L1
#         Sil
# 1 0.8807161
# 2 0.8806012
# 3 0.8804968
# 4 0.8803642
# 5 0.8802118
# 6 0.8800181
clusters <- repeat_clustering(traints, comparison, comparison$pick$config_id)
plot(clusters)
clusts <- clusters@cluster
clusters@family
names(clusts) <- names(traints)
clusts
#       PM_Dongsi   PM_Dongsihuan PM_Nongzhanguan      PM_US.Post            DEWP 
#               2               2               2               2               2 
#            HUMI            PRES            TEMP             Iws   precipitation 
#               2               1               2               2               2 
#           Iprec 
#               2 

# Next we will look at a hierarchical clustering
cfg2 <- compare_clusterings_configs(
  types = "h",
  k = 2L:6L,
  controls = list(
    hierarchical = hierarchical_control(method = "all")
  ),
  preprocs = pdc_configs("preproc", 
                         none = list(),
                         zscore = list()),
  centroid = pdc_configs("centroid",
  shape_extraction = list(),
  default = list(),
  dba=list()
  ),
  distances = pdc_configs("distance",
    hierarchical  = list(
      dtw_basic = list(
        window.size = seq(from = 5L, to = 50L, by = 5L),
        norm = c("L1", "L2")
        ),
      gak = list(),
      sbd = list()
      )
    ),
  no.expand = c("window.size","norm")
)

evaluators <- cvi_evaluators("Sil")

comparison2 <- compare_clusterings(
  traints, 
  types = "hierarchical",
  configs = cfg2, 
  seed = 666L,
  score.clus = evaluators$score,
  pick.clus = evaluators$pick
)
beepr::beep(0)
head(comparison2$results$hiera %>% arrange(desc(Sil)))
#     config_id k  method symmetric preproc distance centroid window.size_distance
# 1 config129_1 2  ward.D     FALSE    none      gak      dba                   NA
# 2 config129_2 3  ward.D     FALSE    none      gak      dba                   NA
# 3 config129_3 4  ward.D     FALSE    none      gak      dba                   NA
# 4 config129_4 5  ward.D     FALSE    none      gak      dba                   NA
# 5 config129_5 6  ward.D     FALSE    none      gak      dba                   NA
# 6 config129_6 2 ward.D2     FALSE    none      gak      dba                   NA
#   norm_distance       Sil
# 1          <NA> 0.9049773
# 2          <NA> 0.9049773
# 3          <NA> 0.9049773
# 4          <NA> 0.9049773
# 5          <NA> 0.9049773
# 6          <NA> 0.9049773

comparison2$results$hierarchical %>% arrange(desc(Sil),distance) %>% 
  ggplot(aes( fill = distance, y = Sil, x = distance)) + 
  geom_boxplot()+ 
  facet_wrap(centroid~method ) +
  scale_fill_few(palette = "Dark")

comparison2$results$hierarchical %>% arrange(desc(Sil),distance) %>% 
  ggplot(aes( fill = distance, y = Sil, x = distance)) + 
  geom_boxplot()+ 
  facet_wrap(centroid~. ) +
  scale_fill_few(palette = "Dark")

comparison2$results$hierarchical %>% arrange(desc(Sil),distance) %>% 
  ggplot(aes( fill = distance, x = Sil)) + 
  geom_density()+ 
  facet_wrap(distance~centroid ) +
  scale_fill_few(palette = "Dark")

comparison2$proc_time
#     user   system  elapsed 
#    0.224    0.156 4639.003 

save(comparison, comparison2, file = "grids.Rda")

cluster2 <- repeat_clustering(traints, comparison2, comparison2$pick$config_id)

plot(cluster2)
plot(cluster2, type = "sc")
cluster2@cluster
#       PM_Dongsi   PM_Dongsihuan PM_Nongzhanguan      PM_US.Post            DEWP 
#               1               1               1               1               1 
#            HUMI            PRES            TEMP             Iws   precipitation 
#               1               2               1               1               1 
#           Iprec 
#               1 
cluster2
cluster2@cluster

# at this point, it is pretty clear the results of the clustering, air pressure will not be included as it is unrelated in any method



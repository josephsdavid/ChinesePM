library(remotes)

# packages that dont build on nix shell
options(repos='http://cran.rstudio.org')
myPackages = installed.packages()
cranPackages <- c('nnfor', 'dtwclust', 'h2o')
to.install <- setdiff(cranPackages, myPackages[,1])
if(length(to.install)>0) install.packages(to.install)

if(!('tswgewrapped' %in% installed.packages())){
  install_github('josephsdavid/tswgewrapped')
}

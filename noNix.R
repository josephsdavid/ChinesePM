library(remotes)
options(repos='http://cran.rstudio.org')

myPackages = installed.packages()
shell <- readLines("shell.nix")
shell <- shell[grepl(shell,pattern = "rPackages")]
shell <-  gsub("rPackages.","",shell)
qhell <- gsub(" ", "", shell)
otherPackages <- c('nnfor', 'dtwclust')
cranPackages <- c(otherPackages,shell)

to.install <- setdiff(cranPackages, myPackages[,1])
if(length(to.install)>0) install.packages(to.install)

if(!('tswgewrapped' %in% installed.packages())){
  install_github('josephsdavid/tswgewrapped')
}

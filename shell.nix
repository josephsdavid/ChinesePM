
let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    name = "timeseRies";
    buildInputs = with pkgs; [
      R
      python37
      rPackages.qrnn
      rPackages.boot
      rPackages.nnet
      rPackages.extraTrees
      rPackages.RCurl
      rPackages.gghighlight
      rPackages.ggfocus
      rPackages.xgboost
      rPackages.devtools
      rPackages.gbm
      rPackages.codetools
      rPackages.tidyverse
      rPackages.remotes
      rPackages.skimr
      rPackages.httpuv
      rstudio
      vscode
      python37Packages.pydot
      rPackages.kerasR
      rPackages.rmarkdown
      rPackages.markdown
      rPackages.plotrix
      rPackages.tree
      rPackages.party
      rPackages.randomForest
      rPackages.opera
      rPackages.RSNNS
      rPackages.plyr
      rPackages.neuralnet
      rPackages.caret
      rPackages.lomb
      rPackages.pander
      rPackages.Mcomp
      rPackages.ggplot2
      rPackages.data_table
      glibc
      rPackages.knitr
      rPackages.latticeExtra
      rPackages.reticulate
      rPackages.orcutt
      rPackages.tswge
      rPackages.doParallel
      rPackages.foreach
      rPackages.memoise
      rPackages.dplyr
      rPackages.purrr
      rPackages.cowplot
      rPackages.magrittr
      rPackages.functional
      rPackages.prophet
      rPackages.drake
      rPackages.future
      rPackages.vars
      rPackages.microbenchmark
      rPackages.tseries
      rPackages.rlist
      rPackages.foreign
      rPackages.fpp2
      rPackages.lubridate
      openblas
      evince
      xsv
      texlive.combined.scheme-full
      python37Packages.pandas
      rPackages.imputeTS
      #rPackages.tensorflow
      rPackages.keras
      rPackages.FNN
      rPackages.fastNaiveBayes
      python37Packages.tensorflowWithCuda
      #python37Packages.tensorflow
      python37Packages.tensorflow-tensorboard
      python37Packages.Keras
      python37Packages.pip
      python37Packages.numpy
      python37Packages.pandas
      python37Packages.matplotlib
      python37Packages.mxnet
    ];
    shellHook = ''
#     echo "#!/usr/bin/env Rscript" > libs.R
#     echo "devtools::install_github('csgillespie/efficient', build_vignettes=TRUE)" >> libs.R
#     Rscript libs.R
     R CMD INSTALL /home/david/scratch/tswgewrapped
    '';

  }

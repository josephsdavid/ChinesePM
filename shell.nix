
let
  pkgs = import <nixpkgs> {};
in
  pkgs.mkShell {
    name = "timeseRies";
    buildInputs = with pkgs; [
       R
       python37
       rPackages.tidyverse
       rstudio
       rPackages.rmarkdown
       rPackages.lomb
       rPackages.pander
       rPackages.Mcomp
       rPackages.ggplot2
       rPackages.data_table
      rPackages.knitr
      rPackages.httpuv
      rPackages.reticulate
      rPackages.orcutt
      rPackages.tswge
      rPackages.caret
      rPackages.doParallel
      rPackages.foreach
      rPackages.memoise
#      rPackages.dtwclust
      rPackages.dplyr
      rPackages.purrr
      rPackages.cowplot
      rPackages.quantmod
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
      rPackages.vroom
      rPackages.lubridate
      openblas
      evince
      xsv
      texlive.combined.scheme-full
      python37Packages.pandas
      python37Packages.IMAPClient
      unar # support free software duh
      rPackages.imputeTS
      rPackages.tensorflow
      rPackages.keras
      rPackages.FNN
      rPackages.fastNaiveBayes
      python37Packages.tensorflow
      python37Packages.Keras
      rPackages.forcats
      rPackages.timetk
      rPackages.tidyquant
      rPackages.tibbletime
      rPackages.recipes
      rPackages.rsample
      rPackages.yardstick
      rPackages.tfruns

    ];
   shellHook = ''
#     echo "#!/usr/bin/env Rscript" > libs.R
#     echo "devtools::install_github('csgillespie/efficient', build_vignettes=TRUE)" >> libs.R
#     Rscript libs.R
     R CMD INSTALL /home/david/scratch/tswgewrapped
      '';

  }

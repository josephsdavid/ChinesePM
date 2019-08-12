library(keras)
library(kerasR)

plot_model(load_model_hdf5("../analysis/daily/winner.h5"))
mod <- load_model_hdf5("../analysis/daily/winner.h5")
plot_model(mod, to_file = "model.png")

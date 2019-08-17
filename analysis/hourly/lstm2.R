source("mvarsetup.R")
library(keras)
library(kerasR)
library(tswgewrapped) # for my brilliant mlag_dfr function
dat <- trainM %>% mlag_dfr("PM_US.Post", 1)  %>% data.matrix
maxt <- floor(0.8*nrow(dat))
# [1] 27993
trainDat <- dat[1:(floor(0.8*(nrow(dat)))),]
mean <- apply(trainDat,2,mean)
std <- apply(trainDat,2,sd)
dat <- scale(dat, center = mean, scale = std)
str(dat)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }

    samples <- array(0, dim = c(length(rows),
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
                      
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }           
    list(samples, targets)
  }
}

lookback <- 72
step <- 6
delay <- 14
batch_size <- 72

maxt
# [1] 27993
train_gen <- generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 27993,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  dat,
  lookback = lookback,
  delay = delay,
  min_index = 27994,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)
val_steps <- (nrow(dat) - maxt -1 - lookback) / batch_size
# [1] 96.19444

# baseline common sense: predict the mean

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()
# [1] 0.8895216


model_lstm <- keras_model_sequential() %>%
  layer_lstm(units = 60, dropout = 0.4, recurrent_dropout = 0.1, 
             activation = "sigmoid",
             input_shape = list(NULL, dim(dat)[[-1]])
                             ,return_sequences = TRUE
              ) %>%
bidirectional(layer_lstm(units = 128,  dropout = 0.4, recurrent_dropout = 0.4,
            activation = "sigmoid", return_sequences = TRUE)) %>%
bidirectional(layer_lstm(units =128,  dropout = 0.4, recurrent_dropout = 0.4,
            activation = "sigmoid")) %>%
#  layer_lstm(units = 32) %>%
  layer_dense(units = 1)


model_lstm %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.001),
  loss = "mse"
)
history <- model_lstm %>% fit_generator(
  train_gen,
  steps_per_epoch = 40,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)
beepr::beep(0)
plot(history)
testK <- testM %>% mlag_dfr("PM_US.Post", 1)   %>% data.matrix
testK <- scale(testK, center  = mean, scale = std)

test_gen = generator(
  testK,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

teststeps <- (nrow(testK) - lookback) / batch_size
preds <- predict_generator(object = model_lstm, generator = test_gen, steps = teststeps)
preds*attr(dat, "scaled:scale")[1] + attr(dat, "scaled:center")[1]
preds*attr(dat, "scaled:scale")[1] 
ASE(keraspreds, testM$PM_US.Post)
plot(keraspreds, type = "line")
lines(as.numeric(testM$PM_US.Post))


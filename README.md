# Time Series Analysis of PM 2.5 Content in Beijing

Soon to be expanded to the rest of china. Analysis methods included

* Classical ARIMA (with seasonality) analysis
* Multiseasonal Dynamic Harmonic Regression 
	* This is just regression with fourier expansions of the complex seasonality of the time series as regressors, and autocorrelated errors.
* TBATS
	* Basically smoothing but with fourier seasons to account for complex seasonality
* Vector Autoregression (VAR)
* Feed Forward Neural Network with AR lags and errors (NNETAR)
* Long Short Term Memory (LSTM-RNN)
	* Experimentally, a hybrid CNN-LSTM model was also attempted
* Ensembling all the forecasts together:
	* Stochastic Gradient Boosted Ensemble
	* Random Forest
* Experimental dynamic time warping and GAK time series clusterng
	* Done for fun, no impact on the final model


All hyperparamters for each individual model were tuned with a combination of experience, trial and error, and hours of grid searches.

## The Final Model

The final model consists of 6 parts, in two layers, and is set to make three-day-ahead forecasts of hourly data from Beijing:

### Layer One:

* Multiseasonal Dynamic Harmonic regression
	* fourier expansion of yearly, weekly, and daily seasonalities
	* AR(5) errors
* TBATS
	* Yearly, weekly, and daily seasonalities
* VAR
	* 5 endogenous variables, one exogenous variable
* NNETAR
	* Scaled inputs, Box-cox transformed
* LSTM
	* 4 Layers
		* Simple LSTM layer, sigmoid activation, dropout = recurrent dropout = 0.1 
		* 2 Bidirectional LSTM layers, each with sigmoid activation and a dropout of 0.1
		* Dense output layer
	* Trained using a rolling window generator function

	   


Each of these models were trained on a training set, and made forecasts on validation and test sets, to be used for ensembling

## Layer 2:

Stochastic gradient boosting machine was trained on the validation forecasts, and then cross validated using the test forecasts.

# Predicting the future

![](analysis/hourly/unnamed-chunk-105-2.png)

72 hour ahead forecasts were made using this model. To make each prediction, in the case of the multivariate models, each of the covariates were also forecasted. The prediction limits were calculated by bootstrapping the residuals of the final model and accounting for the fact that it contains several models, each with their own prediction limits.

## Reproducing this project

### With Nix

`nix-shell` will fully reproduce almost everything used in this project (you will have to remove the shellHook), including a CUDA powered deep learning stack. After that finishes running, run [libs.R](libs.R). Contents of shell.nix seen [here](shell.nix)

### Without Nix

You are on your own as far as setting up a deep learning stack and connecting it to R. To grab all R dependencies (some of which may require more setup), run [noNix.R](noNix.R)


# Contents
## To do list

- [x] preprocessing
- [x] EDA
- [x] Classical Univariate analysis
- [x] Multiseasonal Univariate analysis 
- [x] Multivariate analysis
- [x] clustering
- [x] LSTM
- [x] ANN
- [x] prophet
- [x] Ensemble forecast
- [ ] write paper

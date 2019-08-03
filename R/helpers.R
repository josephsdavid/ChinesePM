# fix up outliers
clean <- forecast::tsclean

# Generator function to fix up sampling rate to something reasonable

change_samples <- function(n){
  function(xs){
    out <- unname(tapply(
                         xs,
                         (seq_along(xs)-1) %/% n,
                         sum
                         ))
    out <- ts(out, frequency = (8760/n))
    out
  }
}

# daily and weekly sampling, monthly is 4 weeks
to_daily <- change_samples(24)
to_weekly <- change_samples(24*7)
to_monthly <- change_samples(24*7*4)
to_season <- change_samples(24*(365/4))

# pipelining final cleaning and conversion, removing crappy NA
cleandays <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_daily
}

cleanweeks <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_weekly
}
cleanmonths <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_monthly
}
cleanseas <- function(xs) {
  xs %>>% clean %>>% abs %>>% to_season
}


resample <- function(xs){
  xs %>>% cleandays %>>% window(end = 6) -> day
  xs %>>% cleanweeks %>>% window(end = 6) -> week
  xs %>>% cleanmonths %>>% window(end = 6) -> month
  xs %>>% cleanseas %>>% window(end = 6) -> seas
  list(day = day, week = week, month = month, season = seas)
}


# some plts
seasonplot <- forecast::ggseasonplot
subseriesplot <- forecast::ggsubseriesplot
lagplot <- forecast::gglagplot

# forecast and assess

fore_and_assess <- function(...){
  f <- forecast(...)
  out <- assess(..., plot = FALSE)
  f$ASE <- out
  f
}

getASE <- function(model){
  accuracy(model)[2]^2
}


# plot test set vs predictions

testPredPlot <- function(xs){
  p <- ggplot() + theme_economist() + scale_color_few(palette = "Dark" )

  doplot <- function(df){
    p <<- p + geom_line(data = df,
                        aes(
                            x = t,
                            y = ppm,
                            color = type
                            ))
  }
  lapply(xs, doplot)

}


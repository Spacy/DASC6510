source('load.R')
library(fpp3)
library(fable.prophet)

filepath = "tourism_monthly_dataset.tsf"
loaded_data <- convert_tsf_to_tsibble(filepath, "vistors", 'series_name', 'start_timestamp')

tsibble_data <- loaded_data[[1]]
frequency <- loaded_data[[2]]
forecast_horizon <- loaded_data[[3]]
contain_missing_values <- loaded_data[[4]]
contain_equal_length <- loaded_data[[5]]

#T1 data
t1_data <- tsibble_data |> filter(series_name=="T1")
t1_data |> autoplot() + labs(y="visitors")

t1_data |> gg_season() +labs(y="visitors")
t1_data |> gg_subseries() +labs(y="visitors")

head(tsibble_data)
series.names = tsibble_data |> as_tibble() |> select(series_name) |> distinct()
series.names = series.names$series_name
tsibble_data = tsibble_data |> 
  mutate(month = yearmonth(start_timestamp)) |> 
  as_tsibble(index = month)


forecast.single = function (series) {
  print(series)
  tourism.ts = tsibble_data |> filter(series_name == series)
  nb.all = nrow(tourism.ts)
  nb.test = 24
  nb.train = nb.all - nb.test
  
  tourism.train = tourism.ts |> filter(row_number() <= nb.train)
  tourism.test = tourism.ts |> filter(row_number() > nb.train)
  #tourism.train |> autoplot(vistors)
  
  #boxcox.lambda = tourism.train |> features(vistors, features = guerrero)
  #boxcox.lambda = boxcox.lambda$lambda_guerrero
  #tourism.train |> autoplot(box_cox(vistors, boxcox.lambda))
  #tourism.train |> model(STL(vistors)) |> components() |> autoplot()
  
  fit.all = tourism.train |> model(
    arima = ARIMA(vistors),
    ets = ETS(vistors),
    nnar = NNETAR(sqrt(vistors)),
    prophet = prophet(vistors ~ season(period = 12, order =2, type = 'multiplicative')),
    mean = MEAN(vistors),
    naive = NAIVE(vistors),
    snaive = SNAIVE(vistors),
    drift = RW(vistors ~ drift())
  )
  
  fcst.accu.all = NULL
  
  for (h in c(1,2,3,6,12,18,24)) {
    print(h)
    fcst = fit.all |> forecast(h = h, times = 100)
    fcst.accu = fcst |> accuracy(tourism.test) |> 
      select(.model, series_name, RMSE, MAE, MAPE)
    fcst.accu$h = h
    if (is.null(fcst.accu.all)) {
      fcst.accu.all = fcst.accu
    } else {
      fcst.accu.all = rbind(fcst.accu.all, fcst.accu)
    }
  }
  fcst.accu.all
}

accu.all.series = NULL
for (series_name in series.names) {
  tictoc::tic()
  if (is.null(accu.all.series)) {
    accu.all.series = forecast.single(series_name)
  } else {
    accu.all.series = rbind(accu.all.series, forecast.single(series_name))
  }
  tictoc::toc()
}

#save(accu.all.series, file = 'accu.all.series.RObject')

accu.summarise = accu.all.series |> group_by(.model, h) |> 
  summarise(RMSE = mean(RMSE), MAE = mean(MAE), MAPE=mean(MAPE))

ggplot(accu.summarise, mapping = aes(x = as.factor(h), y = MAPE)) + 
  geom_bar(aes(fill = .model), stat = 'identity', position = 'dodge')

accu.with.best.model = accu.summarise |> filter(
  .model != 'mean', .model != 'drift', .model != 'naive', .model != 'prophet'
)

accu.with.best.model$.model[accu.with.best.model$.model=='arima'] = 'ARIMA'
accu.with.best.model$.model[accu.with.best.model$.model=='ets'] = 'EWMA'
accu.with.best.model$.model[accu.with.best.model$.model=='snaive'] = 'SNaive'
accu.with.best.model$.model[accu.with.best.model$.model=='nnar'] = 'NNAR'

ggplot(accu.with.best.model, mapping = aes(x = as.factor(h), y = MAPE)) + 
  geom_bar(aes(fill = .model), stat = 'identity', position = 'dodge') +
  labs(x = 'h', fill = 'Model') +
  ggtitle('Forecast MAPE Comparison with Different Horizons')

# mapes table
mapes = pivot_wider(data = accu.summarise, names_from = h, values_from = 'MAPE', id_cols = .model)

# average model rank
rank.mat = apply(mapes |> ungroup() |> select(`1`, `2`, `3`, `6`, `12`, `18`, `24`), 2, rank)
model.rank = rowSums(rank.mat)/7
tibble(mapes |> select(.model), rank = model.rank)


# check EWMA parameter on T1 series

T1 = tsibble_data |> filter(series_name == 'T1')
nb.all = nrow(T1)
nb.test = 24
nb.train = nb.all - nb.test

T1.train = T1 |> filter(row_number() <= nb.train)
T1.test = T1 |> filter(row_number() > nb.train)

fit.best = T1.train |> model(
  ets = ETS(vistors),
  arima = ARIMA(vistors)
)
fcst.best = fit.best |> forecast(new_data = T1.test)

#ets model
report(fit.best |> select(arima))
report(fit.best |> select(ets))

# component and residual
fit.best |> select(ets) |> components() |> autoplot()
fit.best |> select(ets) |> gg_tsresiduals()
fit.best |> select(arima) |> gg_tsresiduals()

#forecast plot
fcst.best |> autoplot(T1) + facet_grid(.model ~ .)

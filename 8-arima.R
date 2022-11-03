library(fpp3)


# GOOG

gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  autoplot(Close) +
  labs(y = "Google closing stock price ($US)")

gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2018) |>
  autoplot(difference(Close)) +
  labs(y = "Daily change in Google closing stock price")


# Population forecasting

fit <- global_economy |>
  model(arima = ARIMA(Population))

fit |>
  filter(Country == "Australia") |>
  report()

fit |>
  forecast(h = 10) |>
  filter(Country == "Australia") |>
  autoplot(global_economy)

# H02

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |> autoplot(Cost) +
  labs(x = "Year", y = "", title = "Cortecosteroid drug scripts")

h02 |> autoplot(log(Cost) |> difference(12) |> difference(1))

h02 |>
  model(arima = ARIMA(log(Cost))) |>
  report()

h02 |>
  model(arima = ARIMA(log(Cost))) |>
  forecast(h = "3 years") |>
  autoplot(h02)

fit <- h02 |>
  model(best = ARIMA(log(Cost),
    stepwise = FALSE,
    approximation = FALSE,
    order_constraint = p + q + P + Q <= 9
  ))
report(fit)

fit |>
  forecast() |>
  autoplot(h02) +
  labs(y = "H02 Expenditure ($AUD)", x = "Year")


# Tourism

train <- tourism |>
  filter(year(Quarter) <= 2014)
fit <- train |>
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips),
    snaive = SNAIVE(Trips)
  ) |>
  mutate(mixed = (ets + arima + snaive) / 3)

fc <- fit |>
  forecast(h = "3 years")
fc |>
  filter(Region == "Snowy Mountains", Purpose == "Holiday") |>
  autoplot(tourism, level = NULL)

accuracy(fc, tourism) |>
  group_by(.model) |>
  summarise(
    RMSE = mean(RMSE),
    MAE = mean(MAE),
    MASE = mean(MASE)
  ) |>
  arrange(RMSE)

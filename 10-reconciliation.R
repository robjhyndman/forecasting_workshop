library(fpp3)

tourism_agg <- tourism |>
  aggregate_key(Purpose * (State / Region),
    Trips = sum(Trips)
  )

fc <- tourism_agg |>
  filter_index(. ~ "2015 Q4") |>
  model(ets = ETS(Trips)) |>
  reconcile(ets_adjusted = min_trace(ets)) |>
  forecast(h = "2 years")

fc |>
  filter(is_aggregated(Purpose) & is_aggregated(State)) |>
  autoplot(tourism_agg, level = 95)

fc <- tourism_agg |>
  filter_index(. ~ "2015 Q4") |>
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) |>
  mutate(
    comb = (ets + arima) / 2
  ) |>
  reconcile(
    ets_adj = min_trace(ets),
    arima_adj = min_trace(arima),
    comb_adj = min_trace(comb)
  ) |>
  forecast(h = "2 years")

fc |> accuracy(tourism_agg)

fc |>
  accuracy(tourism_agg) |>
  group_by(.model) |>
  summarise(MASE = mean(MASE)) |>
  arrange(MASE)

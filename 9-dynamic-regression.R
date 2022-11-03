library(fpp3)

us_change |>
  pivot_longer(-Quarter, names_to = "variable", values_to = "value") |>
  ggplot(aes(y = value, x = Quarter, group = variable)) +
  geom_line() +
  facet_grid(variable ~ ., scales = "free_y") +
  labs(
    x = "Year", y = "",
    title = "Quarterly changes in US consumption and personal income"
  )

us_change |>
  as_tibble() |>
  select(-Quarter) |>
  GGally::ggpairs()

fit <- us_change |>
  model(regarima = ARIMA(Consumption ~ Income + Production + Savings + Unemployment))
report(fit)

gg_tsresiduals(fit)

augment(fit) |>
  features(.resid, ljung_box, dof = 6, lag = 12)

us_change_future <- new_data(us_change, 8) |>
  mutate(
    Income = tail(us_change$Income, 1),
    Production = tail(us_change$Production, 1),
    Savings = tail(us_change$Savings, 1),
    Unemployment = tail(us_change$Unemployment, 1)
  )
forecast(fit, new_data = us_change_future) |>
  autoplot(us_change) +
  labs(x = "Year", y = "Percentage change", title = "Forecasts from dynamic regression")


# Vic electricity daily

vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily |>
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")

vic_elec_daily |>
  pivot_longer(c(Demand, Temperature)) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y")

fit <- vic_elec_daily |>
  model(
    fit = ARIMA(Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday"))
  )
report(fit)

augment(fit) |>
  gg_tsdisplay(.resid, plot_type = "histogram")

augment(fit) |>
  features(.resid, ljung_box, dof = 9, lag = 14)

vic_next_day <- new_data(vic_elec_daily, 1) |>
  mutate(Temperature = 26, Day_Type = "Holiday")
forecast(fit, vic_next_day)

vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(fit, vic_elec_future) |>
  autoplot(vic_elec_daily) + labs(y = "Electricity demand (GW)")


# Aus Cafe

aus_cafe <- aus_retail |>
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) |>
  summarise(Turnover = sum(Turnover))
aus_cafe |> autoplot(Turnover)

fit <- aus_cafe |> model(
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0, 0, 0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0, 0, 0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0, 0, 0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0, 0, 0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0, 0, 0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0, 0, 0))
)

glance(fit)


# US Gasoline

fit <- us_gasoline |>
  model(ARIMA(Barrels ~ fourier(K = 13) + PDQ(0, 0, 0)))
report(fit)

forecast(fit, h = "3 years") |>
  autoplot(us_gasoline)

# Insurance

insurance |>
  pivot_longer(c(Quotes, TVadverts)) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(x = "Year", y = NULL, title = "Insurance advertising and quotations")

insurance |>
  mutate(
    lag1 = lag(TVadverts),
    lag2 = lag(lag1)
  ) |>
  as_tibble() |>
  select(-Month) |>
  rename(lag0 = TVadverts) |>
  pivot_longer(-Quotes, names_to = "Lag", values_to = "TV_advert") |>
  ggplot(aes(x = TV_advert, y = Quotes)) +
  geom_point() +
  facet_grid(. ~ Lag) +
  labs(title = "Insurance advertising and quotations")

fit <- insurance |>
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |>
  model(
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts +
      lag(TVadverts)),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts +
      lag(TVadverts) +
      lag(TVadverts, 2)),
    ARIMA(Quotes ~ pdq(d = 0) + TVadverts +
      lag(TVadverts) +
      lag(TVadverts, 2) +
      lag(TVadverts, 3))
  )

glance(fit)

# Re-fit to all data
fit <- insurance |>
  model(ARIMA(Quotes ~ TVadverts + lag(TVadverts) + pdq(d = 0)))
report(fit)

advert_a <- new_data(insurance, 20) |>
  mutate(TVadverts = 10)
forecast(fit, advert_a) |> autoplot(insurance)

advert_b <- new_data(insurance, 20) |>
  mutate(TVadverts = 8)
forecast(fit, advert_b) |> autoplot(insurance)

advert_c <- new_data(insurance, 20) |>
  mutate(TVadverts = 6)
forecast(fit, advert_c) |> autoplot(insurance)

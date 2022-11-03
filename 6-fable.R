library(fpp3)

# Brick production

brick_fit <- aus_production |>
  filter(!is.na(Bricks)) |>
  model(
    `Seasonal_naïve` = SNAIVE(Bricks),
    `Naïve` = NAIVE(Bricks),
    Drift = RW(Bricks ~ drift()),
    Mean = MEAN(Bricks)
  )

brick_fc <- brick_fit |>
  forecast(h = "5 years")

brick_fc |>
  autoplot(aus_production, level = NULL) +
  labs(
    title = "Forecasts for quarterly clay brick production",
    x = "Year", y = "Millions of bricks"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

brick_fc |>
  hilo(level = c(50, 75)) |>
  unpack_hilo(c("50%", "75%"))


# FB stock

fb_stock <- gafa_stock |>
  filter(Symbol == "FB")
fb_stock |> autoplot(Close)

fb_stock <- fb_stock |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE)
fit <- fb_stock |>
  model(NAIVE(Close))

augment(fit) |>
  filter(trading_day > 1100) |>
  ggplot(aes(x = trading_day)) +
  geom_line(aes(y = Close, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted"))

augment(fit) |>
  autoplot(.resid) +
  labs(x = "Day", y = "", title = "Residuals from naïve method")

augment(fit) |>
  ggplot(aes(x = .resid)) +
  geom_histogram(bins = 150) +
  labs(title = "Histogram of residuals")
augment(fit) |>
  ACF(.resid) |>
  autoplot() + labs(title = "ACF of residuals")

fit |>
  gg_tsresiduals()

augment(fit) |>
  ACF(.resid, lag_max = 10) |>
  autoplot() +
  labs(title = "ACF of residuals")

augment(fit) |>
  features(.resid, ljung_box, dof = 0, lag = 10)

# Beer production

beer_fit <- aus_production |>
  filter(between(year(Quarter), 1992, 2007)) |>
  model(
    snaive = SNAIVE(Beer),
    mean = MEAN(Beer)
  )
beer_fit |>
  forecast(h = "3 years") |>
  autoplot(aus_production, level = NULL) +
  labs(
    title = "Forecasts for quarterly beer production",
    x = "Year", y = "Megalitres"
  ) +
  guides(colour = guide_legend(title = "Forecast"))
beer_fc <- forecast(beer_fit, h = "3 years")
accuracy(beer_fc, aus_production)

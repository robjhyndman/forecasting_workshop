library(fpp3)


aus_economy <- global_economy |>
  filter(Code == "AUS") |>
  mutate(Pop = Population / 1e6)

fit <- aus_economy |>
  model(AAN = ETS(Pop))

report(fit)

components(fit) |>
  autoplot()

fit |>
  forecast(h = 20) |>
  autoplot(aus_economy) +
  labs(y = "Population", x = "Year")

aus_economy |>
  model(holt = ETS(Pop ~ trend("Ad"))) |>
  forecast(h = 20) |>
  autoplot(aus_economy)

fit <- global_economy |>
  mutate(Pop = Population / 1e6) |>
  model(ets = ETS(Pop))

fit |>
  forecast(h = 5)

# Aus holidays

holidays <- tourism |>
  filter(Purpose == "Holiday")
fit <- holidays |>
  model(ets = ETS(Trips))
fit |>
  filter(Region == "Snowy Mountains") |>
  report()
fit |>
  filter(Region == "Snowy Mountains") |>
  components(fit) |>
  autoplot()
fit |>
  forecast() |>
  filter(Region == "Snowy Mountains") |>
  autoplot(holidays) +
  labs(x = "Year", y = "Overnight trips (thousands)")

# Aus cafe

vic_cafe <- aus_retail |>
  filter(
    State == "Victoria",
    Industry == "Cafes, restaurants and catering services"
  ) |>
  select(Month, Turnover)
vic_cafe |>
  autoplot(Turnover) + labs(title = "Monthly turnover of Victorian cafes")

vic_cafe |>
  autoplot(box_cox(Turnover, lambda = 0.2))

fit <- vic_cafe |>
  model(ets = ETS(box_cox(Turnover, 0.2)))

fc <- fit |>
  forecast(h = "3 years")

fc |> autoplot(vic_cafe)

sim <- fit |>
    generate(h = "3 years", times = 5, bootstrap = TRUE)
vic_cafe |>
  filter(year(Month) >= 2008) |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Turnover)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)), data = sim) +
  labs(title = "Monthly turnover of Victorian cafes") +
  guides(col = FALSE)
fc <- fit |>
  forecast(h = "3 years", bootstrap = TRUE)
fc |> autoplot(vic_cafe) +
  labs(title = "Monthly turnover of Victorian cafes")

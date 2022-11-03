library(fpp3)

beer <- aus_production |>
  select(Quarter, Beer) |>
  filter(year(Quarter) >= 1992)
beer |> autoplot(Beer)

beer |> gg_season(Beer, labels = "right")

vic_elec

vic_elec |> gg_season(Demand)
vic_elec |> gg_season(Demand, period = "week")
vic_elec |> gg_season(Demand, period = "day")

beer |> gg_subseries(Beer)

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays

holidays |>
  autoplot(Trips) +
  labs(
    x = "Year", y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

## ----graphics1, fig.width=4, fig.height=5, out.width="45%"------------------
holidays |>
  gg_season(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

holidays |>
  gg_subseries(Trips) +
  labs(
    y = "thousands of trips",
    title = "Australian domestic holiday nights"
  )

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity) +
  labs(y = "GWh", title = "Australian electricity production")

aus_production |>
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    x = "Year", y = "million units"
  )

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(title = "Retail employment, USA", y = "Million people")

gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price",
    x = "Day", y = "$"
  )

pelt |>
  autoplot(Lynx) +
  labs(
    title = "Annual Canadian Lynx Trappings",
    x = "Year", y = "Number trapped"
  )

library(sugrrants)
vic_elec |>
  filter(year(Date) == 2014) |>
  mutate(Hour = hour(Time)) |>
  frame_calendar(
    x = Hour, y = Demand, date = Date,
    nrow = 4
  ) |>
  ggplot(aes(x = .Hour, y = .Demand, group = Date)) +
  geom_line() -> p1
prettify(p1,
  size = 3,
  label.padding = unit(0.15, "lines")
)

new_production <- aus_production |>
  filter(year(Quarter) >= 1992)
new_production

new_production |> gg_lag(Beer)
new_production |> gg_lag(Beer, geom = "point")

new_production |> ACF(Beer, lag_max = 9)

new_production |>
  ACF(Beer, lag_max = 9) |>
  autoplot()
new_production |>
  ACF(Beer) |>
  autoplot()

holidays |> ACF(Trips)
holidays |>
  ACF(Trips) |>
  autoplot()

retail <- us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980)
retail |> autoplot(Employed)
retail |>
  ACF(Employed, lag_max = 48) |>
  autoplot()

google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) == 2015) |>
  select(Date, Close)
google_2015
google_2015 |> autoplot(Close)
google_2015 <- google_2015 |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE)
google_2015
google_2015 |>
  ACF(Close, lag_max = 100) |>
  autoplot()

pigs <- aus_livestock |>
  filter(
    State == "Victoria", Animal == "Pigs",
    year(Month) >= 2014
  )
pigs |> autoplot(Count / 1e3) +
  labs(
    x = "Year", y = "Thousands",
    title = "Number of pigs slaughtered in Victoria"
  )

pigs |>
  ACF(Count) |>
  autoplot()

library(fpp3)
library(purrr)

tourism <- tourism |>
  mutate(
    State = recode(State,
      "Australian Capital Territory" = "ACT",
      "New South Wales" = "NSW",
      "Northern Territory" = "NT",
      "Queensland" = "QLD",
      "South Australia" = "SA",
      "Tasmania" = "TAS",
      "Victoria" = "VIC",
      "Western Australia" = "WA"
    )
  )

# US Retail trade

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)

us_retail_employment |>
  autoplot(Employed) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))

components(dcmp) |> autoplot()

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), trend, color = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

components(dcmp) |> gg_subseries(season_year)

us_retail_employment |>
  autoplot(Employed, color = "gray") +
  autolayer(components(dcmp), season_adjust, color = "#0072B2") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

us_retail_employment |>
  model(STL(Employed ~ trend(window = 15) + season(window = "periodic"),
    robust = TRUE
  )) |>
  components()

us_retail_employment |>
  model(STL(Employed)) |>
  components() |>
  autoplot()

# Australian holidays

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays |>
  autoplot(Trips) +
  labs(
    y = "thousands of trips", x = "Year",
    title = "Australian domestic holiday nights"
  )

holidays |>
  model(stl = STL(Trips)) |>
  components() |>
  autoplot()

dcmp <- holidays |>
  model(stl = STL(Trips)) |>
  components()

dcmp |> gg_subseries(season_year)

autoplot(dcmp, trend, scale_bars = FALSE) +
  autolayer(holidays, alpha = 0.4)

# Vic electricity

vic_elec |>
  model(STL(Demand)) |>
  components() |>
  autoplot()

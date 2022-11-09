library(fpp3)

# Lab Session 1

download.file("http://robjhyndman.com/data/tourism.xlsx",
              tourism_file <- tempfile())
my_tourism <- readxl::read_excel(tourism_file) |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )

my_tourism |>
  as_tibble() |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips), .groups="drop") |>
  filter(Trips == max(Trips))

state_tourism <- my_tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

# Lab Session 2

aus_production |> autoplot(Bricks)

pelt |> autoplot(Lynx)

gafa_stock |> autoplot(Close)

vic_elec |> autoplot(Demand) +
  labs(
    y = "Demand (MW)",
    title = "Half-hourly electricity demand",
    subtitle = "Victoria, Australia"
  )


# Dygraphs example

library(dygraphs)
library(tsbox)
vic_elec |>
  select(Demand) |>
  tsbox::ts_xts() |>
  dygraph() |>
  dyRangeSelector()


# Lab Session 3

snowy <- tourism |>
  filter(Region == "Snowy Mountains") |>
  select(-State, -Region)
snowy |> autoplot(Trips)
snowy |> gg_season(Trips)
snowy |> gg_subseries(Trips)

# Produce a calendar plot for the `pedestrian` data from one location and one year.
library(sugrrants)
(tsibble::pedestrian |>
  filter(year(Date) == 2016, Sensor == "Southern Cross Station") |>
  frame_calendar(x = Time, y = Count, date = Date) |>
  ggplot(aes(x = .Time, y = .Count, group = Date)) +
  geom_line()) |>
  prettify()


# Lab Session 4

aus_production |> gg_lag(Bricks)
aus_production |>
  ACF(Bricks) |>
  autoplot()

pelt |> gg_lag(Lynx)
pelt |>
  ACF(Lynx) |>
  autoplot()

amzn_stock <- gafa_stock |>
  filter(Symbol == "AMZN")
amzn_stock |> gg_lag(Close)
amzn_stock |>
  ACF(Close) |>
  autoplot()

vic_elec |> gg_lag(Demand, period = 1, lags = c(1, 2, 24, 48, 336, 17532))
vic_elec |>
  ACF(Demand, lag_max = 672) |>
  autoplot()

# Lab Session 5

dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(diff = difference(Close))
dgoog |> autoplot(diff)
dgoog |>
  ACF(diff) |>
  autoplot()

gafa_stock |>
  mutate(diff = difference(Close)) |>
  ACF(diff) |> autoplot()







# Lab Session 6

global_economy |>
  autoplot(GDP / Population, alpha = 0.3) +
  guides(colour = "none")

avg_gdp_pc <- global_economy |>
  as_tibble() |>
  group_by(Country) |>
  summarise(
    # Average GDP per capita for each country
    gdp_pc = mean(GDP / Population, na.rm = TRUE),
    # Most recent GDP per capita for each country
    last = last((GDP / Population)[!is.na(GDP / Population)])
  )
avg_gdp_pc |>
  arrange(desc(gdp_pc))

max_gdp_pc <- global_economy |>
  semi_join(
    avg_gdp_pc |>
      filter(gdp_pc == max(gdp_pc, na.rm = TRUE)),
    by = "Country"
  )

# install.packages("ggrepel")
# Using goem_label_repel() gives nicer label positions than geom_label()
# If the ggrepel package is not available, you can use geom_label() instead
library(ggrepel)
global_economy |>
  ggplot(aes(x = Year, y = GDP / Population, group = Country)) +
  geom_line(alpha = 0.3) +
  geom_line(colour = "red", data = max_gdp_pc) +
  geom_label_repel(
    aes(label = Country, x = 2020, y = last),
    data = top_n(avg_gdp_pc, 5, last),
  )

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

holidays |>
  model(stl = STL(Trips ~ season(window = 13) + trend(window = 21))) |>
  components() |>
  autoplot()

# Lab Session 7

global_economy |>
  filter(Code == "USA") |>
  autoplot(box_cox(GDP, 0.3))

aus_livestock |>
  filter(
    State == "Victoria",
    Animal == "Bulls, bullocks and steers"
  ) |>
  autoplot(log(Count))

vic_elec |>
  autoplot(log(Demand))

aus_production |>
  autoplot(box_cox(Gas, 0.1))

canadian_gas |> autoplot()






# Lab Session 8

## Changing the size of the windows changes the trend and seasonal components
## A smaller window gives a more flexible (fast changing) component
## A longer window gives a smoother (slow changing) component

canadian_gas |>
  model(STL(Volume)) |>
  components() |>
  autoplot()

canadian_gas |>
  model(STL(Volume ~ season(window =7) + trend(window = 21))) |>
  components() |>
  autoplot()

canadian_gas |>
  model(STL(Volume ~ season(window = 7) + trend(window = 21))) |>
  components() |>
  gg_season(season_year)

canadian_gas |>
  model(STL(Volume ~ season(window = 7) + trend(window = 21))) |>
  components() |>
  select(Month, season_adjust) |>
  autoplot(season_adjust)

# Lab Session 9

library(GGally)
tourism |>
  features(Trips, feat_stl) |>
  select(-Region, -State, -Purpose) |>
  mutate(
    seasonal_peak_year = factor(seasonal_peak_year),
    seasonal_trough_year = factor(seasonal_trough_year),
  ) |>
  ggpairs()

tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips)) |>
  features(Trips, feat_stl) |>
  select(State, seasonal_peak_year)

# Lab Session 10

library(broom)

## Compute features
PBS_feat <- PBS |>
  features(Cost, feature_set(pkgs = "feasts")) |>
  select(-`...26`) |>
  na.omit()

## Compute principal components
PBS_prcomp <- PBS_feat |>
  select(-Concession, -Type, -ATC1, -ATC2) |>
  prcomp(scale = TRUE) |>
  augment(PBS_feat)

## Plot the first two components
PBS_prcomp |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2)) +
  geom_point()

## Pull out most unusual series from first principal component
outliers <- PBS_prcomp |>
  filter(.fittedPC1 > 7)
outliers

## Visualise the unusual series
PBS |>
  semi_join(outliers, by = c("Concession", "Type", "ATC1", "ATC2")) |>
  autoplot(Cost) +
  facet_grid(vars(Concession, Type, ATC1, ATC2),
             scales = "free_y") +
  labs(title = "Outlying time series in PC space")

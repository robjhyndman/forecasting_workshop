# _targets.R file
library(targets)
library(tarchetypes)

# Files called by all qmd files
extra_files <- c(
  "setup.R",
  "_quarto.yml",
  "header.tex",
  "beamerthememonash.sty",
  "beamercolorthememonashwhite.sty"
)

list(
  tar_quarto(readme, "README.qmd"),
  tar_quarto(ch0, "0-intro.qmd", extra_files = extra_files),
  tar_quarto(ch1, "1-tsibbles.qmd", extra_files = extra_files),
  tar_quarto(ch2, "2-graphics.qmd", extra_files = extra_files),
  tar_quarto(ch3, "3-transformations.qmd", extra_files = extra_files),
  tar_quarto(ch4, "4-decompositions.qmd", extra_files = extra_files),
  tar_quarto(ch5, "5-feasts.qmd", extra_files = extra_files),
  tar_quarto(ch6, "6-fable.qmd", extra_files = extra_files),
  tar_quarto(ch7, "7-ets.qmd", extra_files = extra_files),
  tar_quarto(ch8, "8-arima.qmd", extra_files = extra_files),
  tar_quarto(ch9, "9-dynamic-regression.qmd", extra_files = extra_files),
  tar_quarto(ch10, "10-reconciliation.qmd", extra_files = extra_files)
)

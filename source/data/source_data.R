# ==============================================================================
# set-up
# ==============================================================================
pacman::p_load(
  "tidyverse",
  "purrr",
  "data.table",
  "R.utils",
  "here",
  "parallel",
  "magrittr"
)

set.seed(1789)

source(
  here("source", "data", "utils.R")
)

run_module <- partial(
  run_module,
  domain = 'data'
)
# ==============================================================================
# build data repos
# ==============================================================================
repos <- c(
  "censo_br",
  "censo_escolar",
  "finbra",
  "rais",
  "saeb",
  "tse",
  "spaece",
  "worldbank"
)

walk(
  repos,
  ~ run_module(.)
)
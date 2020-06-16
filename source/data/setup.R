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
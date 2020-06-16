# ==============================================================================
# set-up
# ==============================================================================
random_seed <- 1789

pacman::p_load(
  "tidyverse",
  "purrr",
  "data.table",
  "R.utils",
  "here",
  "parallel",
  "magrittr"
)

set.seed(random_seed)

source(
  here("source", "data", "utils.R")
)

run_module <- partial(
  run_module,
  domain = 'data'
)
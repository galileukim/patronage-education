# ==============================================================================
# set-up
# ==============================================================================
random_seed <- 1789

pacman::p_load(
  "magrittr",
  "tidyverse",
  "purrr",
  "data.table",
  "R.utils",
  "here",
  "parallel",
  "magrittr",
  "future",
  "furrr"
)

set.seed(random_seed)

source(
  here("source", "data", "utils.R")
)

run_module <- partial(
  run_module,
  domain = 'data'
)

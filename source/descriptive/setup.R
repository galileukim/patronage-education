# ==============================================================================
# set-up
# ==============================================================================
pacman::p_load(
  tidyverse,
  data.table,
  here,
  furrr
)

knitr::opts_chunk$set(
  eval = T,
  cache = T,
  cache.lazy = F,
  message = F,
  warning = F,
  echo = F,
  fig.height = 3,
  fig.width = 4,
  fig.align = "center"
)

# connect to database
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "PostgreSQL Unicode",
  database = "rais",
  UID = "gali",
  PWD = 'gali1789!',
  port = 5432
)

set.seed(1789)

# ==============================================================================
# load auxiliary utils
# ==============================================================================
source(
  here("source", "descriptive", "utils.R")
)

run_module <- partial(
  run_module,
  domain = "replication"
)

read_data <- partial(
  read_data,
  type = "clean"
)

theme_set(
  theme_minimal() +
    theme_clean
)
# ==============================================================================
# build data repos
# ==============================================================================
source(
  here::here("source", "data", "setup.R")
)

repos <- c(
  "censo_br",
  "censo_escolar",
  "censo_escolar_turnover",
  "finbra",
  "rais",
  "saeb",
  "tse",
  "spaece",
  "worldbank",
  "censo_magisterio"
)

walk(
  repos,
  ~ run_module(.)
)
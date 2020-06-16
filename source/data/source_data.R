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
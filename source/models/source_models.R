# ==============================================================================
# generate objects and visualization for statistical modeling
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

# run tasks
modules <- c(
  # "model_turnover_saeb",
  # "model_turnover_spaece",
  # "model_coalition_turnover"
  )

walk(
  modules,
  ~run_module(.)
)
# ==============================================================================
# run modules
# ==============================================================================
source(
  here::here("source", "descriptive", "setup.R")
)

# run tasks
modules <- c(
  # "visual_map",
  # "visual_global_edu",
  # "visual_budget",
  # "visual_bureaucracy",
  # "visual_electoral",
  # "visual_turnover",
  "visual_turnover_score"
  # "model_turnover_saeb",
  # "model_turnover_spaece",
  # "visual_model_turnover"
  )

walk(
  modules,
  ~run_module(.)
)

# make sure to create unit tests which sample data randomly (~2%)
# and ensure that the entire source code compiles

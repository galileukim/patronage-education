# ==============================================================================
# run modules
# ==============================================================================
source(
  here::here("source", "descriptive", "setup.R")
)

# run tasks
modules <- c(
  "visual_budget",
  "visual_bureaucracy",
  "visual_electoral",
  "visual_global_edu",
  "visual_map",
  "visual_staff_turnover",
  "visual_test_scores",
  "visual_turnover_score",
  "visual_electoral_diff"
  )

plan(multicore)
future_map(
  modules,
  ~run_module(.)
)

# make sure to create unit tests which sample data randomly (~2%)
# and ensure that the entire source code compiles

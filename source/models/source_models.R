# ==============================================================================
# generate objects and visualization for statistical modeling
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

# run tasks
preprocess_modules <- c(
  "preprocess_saeb_hierarchical",
  "preprocess_patronage_reelection"
)

model_modules <- c(
  "model_turnover_saeb",
  "model_turnover_spaece",
  "model_turnover_ivreg",
  "model_coalition_rais",
  "model_coalition_turnover",
  "model_accountability",
  "model_patronage_reelection"
  )
  
visual_modules <- c(
  "visual_covariate_balance",
  "visual_model_coalition",
  "visual_model_turnover",
  "visual_regression_discontinuity"
)

plan(multicore, workers = 4)
walk(
  c(
    preprocess_modules,
    model_modules,
    visual_modules
  ),
  future_map(., run_module)
)
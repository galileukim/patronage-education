# ==============================================================================
# construct data to estimate effect of coalition on edu staff turnover
# ==============================================================================
rais_edu_mun <- read_data(
  "rais",
  "rais_mun_edu.rds"
)

# ==============================================================================
print("model specification")
# ==============================================================================
rais_covariates <- c("rais_higher_edu", "rais_wage", "rais_permanent")
robustness_covariates <- c(
    "censo_log_pop", "censo_median_wage",
    "censo_rural", "mayor_coalition_size"
  )
municipal_covariates <- c(mun_covariates)

controls <- c(
  municipal_covariates, rais_covariates, mayor_covariates, chamber_covariates
)

formulae_baseline <- formulate_models(
  "rais_hired",
  "coalition_share*rais_category",
  c("cod_ibge_6", controls),
  fe = NULL
)

robustness_specification <- sprintf(
  "%s*coalition_share*rais_category",
  robustness_covariates
) %>%
  map(
    ~ c(., c(controls, "year", "state"))
  ) %>%
  set_names(
    robustness_covariates
  )

formulae_robustness <- robustness_specification %>%
  map(
    ~ reformulate(., "rais_hired")
  )

formulae_felm <- formulae_baseline %>%
  map(
    ~ add_felm(., fe = "state + year", cluster = "cod_ibge_6")
  )

formula_felm_second_term <- reformulate(
  c("coalition_share*rais_category", controls),
  "rais_hired"
) %>%
  update(. ~ . - mayor_reelected) %>%
  add_felm(., fe = "state + year", cluster = "cod_ibge_6")

# ==============================================================================
print("reshape data for estimation")
# ==============================================================================
model_rais_mun <- rais_edu_mun %>%
  join_covariate() %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    rais_category = fct_relevel(rais_category, "teacher"),
    budget_education_capita = budget_education / censo_pop,
    across(c(state, year), as.factor)
  ) %>%
  model.frame(
    formula = update(
      formulae_baseline[["controls"]],
      . ~ . + state + year
    ),
    data = .
  ) %>%
  fix_scale()

model_rais_mun_second_term <- model_rais_mun %>%
  filter(
    mayor_reelected == 1
  )

# ==============================================================================
# estimation of effect of coalition share on staff turnover
# ==============================================================================
# municipal fe model
fit_felm <- map(
  formulae_felm,
  ~ lfe::felm(., data = model_rais_mun)
)

fit_felm_robustness <- map(
  formulae_robustness,
  ~ lm(., data = model_rais_mun)
)

fit_felm_second_term <- lfe::felm(
  formula_felm_second_term,
  data = model_rais_mun_second_term
)

# ==============================================================================
print("write-out data")
# ==============================================================================
fit_felm %>%
  write_model("fit_felm_turnover_coalition.rds")

fit_felm_robustness %>%
  write_model("fit_felm_turnover_coalition_robustness.rds")

fit_felm_second_term %>%
  write_model("fit_felm_turnover_coalition_second_term.rds")
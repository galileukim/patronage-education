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
municipal_covariates <- c("cod_ibge_6", mun_covariates)

controls <- c(
  municipal_covariates, rais_covariates, mayor_covariates, chamber_covariates
)

formulae_no_fe <- formulate_models(
  "rais_hired",
  "coalition_share*rais_category",
  controls,
  fe = NULL
)

formulae_felm <- formulae_no_fe %>%
  map(
    ~add_felm(., fe = "state + year", cluster = "cod_ibge_6")
  )

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
      formulae_no_fe[["controls"]],
      . ~ . + state + year
    ),
    data = .
  ) %>%
  fix_scale()

# ==============================================================================
# estimation of effect of coalition share on staff turnover
# ============================================================================== 
# municipal fe model
fit_felm <- map(
  formulae_felm,
  ~lfe::felm(., data = model_rais_mun)
)

# ==============================================================================
print("write-out data")
# ==============================================================================
fit_felm %>%
  write_model("fit_felm_turnover_coalition.rds")
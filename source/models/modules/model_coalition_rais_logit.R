# ==============================================================================
# construct data to estimate effect of coalition on edu staff turnover
# ==============================================================================
rais_edu <- read_data(
  "rais",
  "rais_edu.rds"
)

# ==============================================================================
print("model specification")
# ==============================================================================
controls <- c(
  mun_covariates, rais_covariates, mayor_covariates, chamber_covariates
)

f_logit <- formulate(
  "rais_hired",
  "coalition_share*rais_category",
  controls,
  fe = c("state", "year")
)

formulae_logit <- c(
  f_logit,
  update(f_logit, rais_fired ~ .)
)
# ==============================================================================
print("reshape data for estimation")
# ==============================================================================
model_rais_micro <- rais_edu %>%
  sample_frac(1/3) %>%
  join_covariate() %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    rais_category = fct_relevel(rais_category, 'teacher'),
    budget_education_capita = budget_education / censo_pop,
    across(c(state, year), as.factor)
    ) %>%
  model.frame(
    formula = update(f_logit, . ~ . + rais_fired),
    data = .
  ) %>%
  fix_scale()

# ==============================================================================
# estimation of effect of coalition share on staff turnover
# ============================================================================== 
fit_turnover <- map(
    formulae_logit,
    ~logit(., data = model_rais_micro)
  )

# ==============================================================================
print("write-out data")
# ==============================================================================
fit_turnover %>%
  write_model("fit_logit_turnover_coalition.rds")
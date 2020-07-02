# ==============================================================================
# explaining staff turnover as a result of executive-legislative bargain
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

# construct data for estimation of effect of coalition on teacher turnover
censo_school_turnover <- read_data(
  "censo_escolar",
  "censo_school_turnover.rds"
)

censo_school <- read_data(
  "censo_escolar",
  "censo_school.rds"
)

model_school_turnover <- censo_school_turnover %>%
  left_join(
    censo_school %>%
      mutate(
        school_id = as.integer(school_id)
      ),
    by = c("cod_ibge_6", "year", "school_id")
  ) %>%
  join_covariate() %>%
  mutate(
    budget_education_capita = budget_education / censo_pop
  )

# ==============================================================================
# estimation of the effect of coalition share on staff turnover
# ==============================================================================
controls_turnover <- c(
  mun_covariates, school_covariates
)

formulae_turnover <- formulate_models(
  "turnover_index",
  "coalition_share",
  fe = NULL,
  controls_turnover
) %>%
  map(~
  add_felm(
    .,
    fe = "year + state",
    cluster = "cod_ibge_6"
  ))

fit_turnover <- map(
  formulae_turnover,
  ~ lfe::felm(., data = model_school_turnover)
)

write_model(
  fit_turnover,
  "fit_coalition_turnover.rds"
)
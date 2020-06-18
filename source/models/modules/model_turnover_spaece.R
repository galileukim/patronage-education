# ==============================================================================
# spaece: annual standardized test data for the state of ceara
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

print("pre-processing data")

spaece <- read_data(
  "spaece",
  "spaece.rds"
) %>%
  filter(
    dep == "municipal",
    grade_level != 2
  )

finbra <- read_data(
  "finbra",
  "finbra.rds"
)

censo_school <- read_data(
  "censo_escolar",
  "censo_school.rds"
) %>%
  mutate(
    school_id = as.integer(school_id)
  )

censo_school_turnover <- read_data(
  "censo_escolar",
  "censo_school_turnover.rds"
) %>%
  select(
    cod_ibge_6,
    year,
    school_id,
    turnover_index,
    starts_with("percent"),
    n_teacher = n
  )

censo_school_ceara <- censo_school_turnover %>% 
  filter(
    str_sub(cod_ibge_6, 1, 2) == "23" # state of ceara
  )

spaece_turnover <- spaece %>% 
  left_join(
    censo_school %>% 
      filter(dep == 'municipal', year >= 2007) %>%
      select(-dep),
    by = c('cod_ibge_6', 'school_id', 'year')
  ) %>% 
  left_join(
    censo_school_ceara,
    by = c("cod_ibge_6", "school_id", "year")
  ) %>% 
  join_covariate() %>% 
  mutate(
    mandate_year = if_else(
      year %in% seq(2005, 2013, 4), 1, 0
    )
  )

spaece_turnover <- spaece_turnover %>% 
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    budget_education_capita = budget_education / censo_pop
  ) %>% 
  mutate_at(
    vars(grade_level, year, state),
    as.factor
  ) %>% 
  mutate_if(
    is.double,
    scale_z
  ) %>%
  filter(
    grade_level != 2
  )

  print("pre-processing complete!")

# ==============================================================================
# estimate effect of turnover on mean student test scores
# ==============================================================================
controls <- c(school_covariates, mun_covariates)

formulae_spaece <- formulate_models(
  "spaece_mean",
  "turnover_index + grade_level + participation_rate",
  fe = NULL,
  controls
) %>%
  map(
    ~add_felm(., fe = "year", cluster = "0" )
  )

fit_spaece <- map(
  formulae_spaece,
  ~ lfe::felm(
    formula = .,
    data = spaece_turnover
  )
)

names(fit_spaece) <- c(
  "turnover_baseline",
  "turnover_controls"
)

fit_spaece %>%
    write_model(
        "fit_spaece.rds"
    )
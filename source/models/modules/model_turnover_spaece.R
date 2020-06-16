# ==============================================================================
# spaece: annual standardized test data for the state of ceara
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

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
    grade_level,
    turnover_index,
    starts_with("percent"),
    n_teacher = n
  ) %>%
  filter(
    n_teacher >= 5,
    grade_level %in% c(5, 9)
  )

censo_school_ceara <- censo_school_turnover %>% 
  filter(
    str_sub(cod_ibge_6, 1, 2) == "23", # state of ceara
    grade_level %in% c(5, 9)
  )

spaece_turnover <- censo_school_ceara %>% 
  left_join(
    censo_school %>% 
      filter(dep == 'municipal', year >= 2007) %>%
      select(-dep),
    by = c('cod_ibge_6', 'school_id', 'year')
  ) %>% 
  left_join(
    spaece,
    by = c("cod_ibge_6", "school_id", "year", "grade_level")
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
    scale
  ) %>%
  filter(
    grade_level != 2
  )

# ==============================================================================
# estimate effect of turnover on mean student test scores
# ==============================================================================
controls <- c(school_cov, mun_cov)

formulae_spaece <- formulate_models(
  "spaece_mean",
  "turnover_index * grade_level",
  fe = "as.factor(year)",
  controls
)

fit_spaece <- map(
  formulae_spaece,
  ~ felm(
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
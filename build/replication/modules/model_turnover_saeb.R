# ==============================================================================
# hierarchical estimation
# ==============================================================================
f <- stats::as.formula

saeb <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
) %>%
  filter(
    year >= 2007
  ) %>%
  sample_frac(0.2)

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
    grade_level %in% c(5, 9)
  )

# prepare data for estimation
saeb_hierarchical <- list(
  saeb,
  finbra,
  censo_school,
  censo_school_turnover
) %>%
  reduce(
    left_join
  ) %>%
  mutate(
    cod_ibge_6 = as.factor(cod_ibge_6),
    year = as.factor(year),
    saeb_principal_experience = fct_relevel(saeb_principal_experience, "2 to 10"),
    saeb_teacher_work_school = fct_relevel(saeb_teacher_work_school, "2 to 10"),
    censo_log_pop = log(censo_pop),
    budget_education_capita = budget_education / censo_pop
  )

# fix blank strings
saeb_hierarchical <- saeb_hierarchical %>%
  fix_na %>%
  mutate_if(
    is.double,
    scale_z
  )

saeb_hierarchical %>% gg_miss_var()

# fix problem with turnover index: why is there so much missingness
turnover_test <- censo_school_turnover %>%
  # filter(year == 2007) %>%
  select(
    cod_ibge_6,
    school_id,
    year,
    grade_level
  )

saeb_test  <- saeb %>%
  # filter(year == 2007) %>%
  select(
    cod_ibge_6,
    school_id,
    cod_classroom,
    year,
    grade_level
  )

list(turnover_test, saeb_test) %>%
map(
  ~distinct(., school_id) %>% 
  arrange(school_id)
) %>%
  reduce(
    inner_join
  )

# check how many school ids are present
unjoin  <- saeb_test %>% 
  distinct(cod_ibge_6, school_id, grade_level) %>% 
  anti_join(turnover_test %>% distinct(cod_ibge_6, school_id, grade_level))

saeb_test %>% inner_join(unjoin) %>% glimpse

# estimation of hierarchical linear models to assess effect of teacher turnover on student learning
fe <- c(
  "(1 | year)",
  "(1 | state)"
)

teacher_cov <- c(
  "saeb_wage_teacher",
  "education_teacher",
  "gender_teacher"
  # "year_as_teacher"
)

principal_cov <- c(
  "saeb_principal_female",
  "saeb_principal_education",
  "saeb_principal_appointment"
)

student_cov <- c(
  "parent_attend_college_student",
  "failed_school_year_student"
)

school_cov <- c(
  "access_water",
  "access_electricity",
  "library",
  "meal"
)

mun_cov <- c(
  "censo_median_wage",
  "censo_log_pop",
  "censo_rural",
  "censo_lit_rate",
  "budget_education_capita"
)

controls <- c(
  teacher_cov,
  principal_cov,
  student_cov,
  school_cov,
  mun_cov,
  fe
)

formulae <- c(
  formulate_models(
    "turnover_index * grade_level",
    response = "grade_exam",
    fe = fe,
    controls = controls
  ),
  formulate_models(
    "saeb_principal_experience * grade_level + saeb_teacher_work_school * grade_level",
    response = "grade_exam",
    fe = fe,
    controls = controls
  )
)

fit_lmer <- map(
  formulae,
  ~ lme4::lmer(
    formula = .x,
    data = saeb_hierarchical
  )
)

names(fit_lmer) <- map(
  c("turnover", "principal_teacher"),
  ~paste(., c("baseline", "controls"), sep = "_")
) %>%
flatten_chr

fit_lmer %>%
  write_model(
    "fit_saeb_hierarchical.rds"
  )
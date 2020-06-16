# ==============================================================================
# hierarchical estimation
# ==============================================================================
print("loading datasets")

f <- stats::as.formula

saeb <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
) %>%
  filter(
    year >= 2007
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
    grade_level %in% c(5, 9)
  )

# prepare data for estimation
print("joining datasets")
saeb_hierarchical <- list(
  saeb,
  finbra,
  censo_school,
  censo_school_turnover
) %>%
  reduce(
    left_join
  ) %>%
  mutate_at(
    vars(
      state, year, grade_level,
      starts_with("access")
      ),
      as.factor
  ) %>%
  mutate(
    saeb_principal_higher_education = if_else(
      saeb_principal_education == "higher education", 1L, 0L
    ) %>% as.factor,
    saeb_principal_experience = fct_relevel(
      saeb_principal_experience, "2 to 10"
    ),
    saeb_teacher_work_school = fct_relevel(
      saeb_teacher_work_school, "2 to 10"
    ),
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

# ==============================================================================
# estimation of hierarchical linear models to assess effect of teacher 
# turnover on student learning
# ==============================================================================
print("begin estimation of hierarchical linear model")

fe <- c(
  "(1 | year)",
  "(1 | state)"
)

controls <- c(
  teacher_cov,
  principal_cov,
  student_cov,
  school_cov,
  mun_cov
)

formulae <- c(
  formulate_models(
    "turnover_index",
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
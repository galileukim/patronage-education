# hierarchical model ------------------------------------------------------
f <- stats::as.formula

saeb <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
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
    n_teacher >= 5
  )

# prepare data for estimation
saeb_hierarchical <- lst(
  saeb,
  finbra,
  censo_school,
  censo_school_turnover
) %>%
  reduce(
    left_join
  ) %>%
  mutate(
    saeb_teacher_work_school = fct_relevel(saeb_teacher_work_school, "2 to 10"),
    censo_log_pop = log(censo_pop),
    budget_education_capita = budget_education / censo_pop
  )

# fix blank strings
saeb_hierarchical <- saeb_hierarchical %>%
  mutate_all(
    ~ na_if(., "")
  ) %>%
  mutate_if(
    is.double,
    scale
  )

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
  "saeb_principal_education"
  # "saeb_principal_appointment"
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

fm_hierarchical <- purrr::partial(
  stats::reformulate,
  response = "grade_exam"
)

formulae <- c(
  fm_hierarchical(
    c("turnover_index*grade_level", fe)
  ),
  fm_hierarchical(
    c("turnover_index*grade_level", controls)
  ),
  fm_hierarchical(
    c("saeb_teacher_work_school*grade_level", fe)
  ),
  fm_hierarchical(
    c("saeb_teacher_work_school*grade_level", controls)
  )
)

fit_lmer <- map(
  formulae,
  ~ lme4::lmer(
    formula = .x,
    data = saeb_hierarchical
  )
)

fit_lmer %>%
  write_model(
    "fit_saeb_hierarchical.rds"
  )

sink(
  here("replication", "results", "model_hierarchical.tex")
)
mstar(
  fit_lmer,
  keep = c("turnover_index", "saeb_principal_experience", "saeb_teacher_work_school"),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 2))),
  covariate.labels = c(
    "Turnover index", "Turnover index $\\times$ Grade 9",
    "Teacher experience (2 years)", "Teacher experience (10 years)",
    "School principal experience (2 years)", "School principal experience (10 years)"
  ),
  dep.var.caption = "Student learning",
  dep.var.labels = "SAEB average test scores"
)
sink()
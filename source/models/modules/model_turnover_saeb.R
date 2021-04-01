# ==============================================================================
# hierarchical estimation
# ==============================================================================
print("loading datasets")

f <- stats::as.formula

source(
    here("source/models/modules/preprocess_saeb_hierarchical.R")
)

# fix categories for teacher/principal experience
saeb_hierarchical <- saeb_hierarchical %>%
  mutate(
    across(
      c(saeb_teacher_work_school, saeb_principal_experience),
      ~relevel(., "more than 10")
    )
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
  teacher_covariates,
  principal_covariates,
  student_covariates,
  school_covariates,
  mun_covariates
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

print("estimating hierarchical linear model")

fit_lmer <- map(
  formulae,
  ~ lme4::lmer(
    formula = .,
    data = saeb_hierarchical
  )
)

names(fit_lmer) <- map(
  c("turnover", "principal_teacher"),
  ~ paste(., c("baseline", "controls"), sep = "_")
) %>%
  flatten_chr()

print("writing out model output")

fit_lmer %>%
  write_model(
    "fit_saeb_hierarchical.rds"
  )
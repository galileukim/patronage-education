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

# ==============================================================================
# spaece: annual standardized test data for the state of ceara
# ==============================================================================
spaece <- fread(
  here("data/spaece/spaece.csv")
)

censo_turnover_ce <- censo_school_turnover %>% 
  filter(
    str_sub(cod_ibge_6, 1, 2) == "23",
    grade_level %in% c(2, 5, 9)
  )

censo_turnover_score <- censo_turnover_ce %>% 
  left_join(
    censo_school %>% 
      filter(dep == 'municipal', year >= 2007) %>% 
      transmute(school_id = as.integer(school_id), year, toilet, meal),
    by = c('school_id', 'year')
  ) %>% 
  left_join(
    spaece,
    by = c("cod_ibge_6", "school_id", "year", "grade_level" = "grade")
  ) %>% 
  left_join(
    saeb_exam_school,
    by = c("cod_ibge_6", "school_id", "year", "grade_level" = "grade")
  ) %>% 
  join_covariate() %>% 
  mutate(
    mandate_year = if_else(
      year %in% seq(2005, 2013, 4), 1, 0
    )
  )

censo_turnover_score <- censo_turnover_score %>% 
  mutate(
    state = str_sub(cod_ibge_6, 1, 2)
  ) %>% 
  mutate_at(
    vars(grade_level, year, state),
    as.factor
  ) %>% 
  mutate_if(
    is.double,
    scale
  )

formulae_spaece <- c(
  as.formula(
    spaece_mean ~ turnover_index * grade + as.factor(year)
  ),
  as.formula(
    spaece_mean ~ turnover_index * grade + as.factor(year) + toilet + meal +
      censo_rural + censo_log_pop + censo_lit_rate
  )
)

fit_spaece <- map(
  formulae_spaece,
  ~ lm(
    formula = .,
    data = censo_turnover_score %>%
      mutate_if(is.double, rescale) %>%
      rename(grade = grade_level) %>%
      filter(grade != 2)
  )
)

sink(p_file_here("results", "spaece_result.tex"))
mstar(
  fit_spaece,
  keep = c("turnover_index"),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 1))),
  covariate.labels = c("Turnover index", "Turnover index $\\times$ Grade 9"),
  dep.var.caption = "Student learning",
  dep.var.labels = "SPAECE average test scores"
)
sink()

sink(p_file_here("results", "student_learning.tex"))
mstar(
  c(fit_lmer, fit_spaece),
  keep = c("turnover_index", "saeb_principal_experience", "saeb_teacher_work_school"),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 3))),
  dep.var.labels.include = F,
  covariate.labels = c(
    "Turnover index", "Turnover index $\\times$ Grade 9",
    "Teacher experience (2 years)", "Teacher experience (10 years)",
    "School principal experience (2 years)", "School principal experience (10 years)"
  ),
  dep.var.caption = "Student learning",
  column.labels = c("SAEB test score", "SPAECE test score"),
  column.separate = c(4, 2),
  model.names = FALSE,
  keep.stat = "n"
)
sink()

# visualization
mods <- c(
  fit_lmer[c(2, 4)],
  fit_spaece[2]
)

names(mods) <- c("model turnover index", "model teacher and principal", "model spaece")

estimate_hlm <- map2_dfr(
  mods,
  names(mods),
  ~ tidyfit(
    .x
  ) %>%
    mutate(
      type = .y
    )
)

estimate_hlm <- map2_dfr(
  mods,
  names(mods),
  ~ tidyfit(
    .x
  ) %>%
    mutate(
      type = .y
    )
)

plot_hlm <- estimate_hlm %>%
  ggplot(
    aes(
      y = term,
      x = estimate,
      color = type,
      group = type
    )
  ) +
  geom_point(
    position = ggstance::position_dodgev(height = 0.3),
    size = 2
  ) +
  geom_errorbar_tidy +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  ) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    limits = rev(c(
      "turnover_index",
      "saeb_principal_experienceless than 2 years",
      "saeb_teacher_work_schoolless than 2",
      "censo_rural",
      "censo_log_pop",
      "education_teacherhigher education",
      "saeb_wage_teachermore than 3000"
    )),
    labels = c(
      "turnover_index" = "Teacher turnover index",
      "saeb_principal_experienceless than 2 years" = "Principal: less than 2 years of exp.",
      "saeb_teacher_work_schoolless than 2" = "Teacher: less than 2 years in school",
      "censo_rural" = "Rural population",
      "censo_log_pop" = "Logged population",
      "education_teacherhigher education" = "Teacher: higher education",
      "saeb_wage_teachermore than 3000" = "Teacher: wage more than $750"
    )
  ) +
  coord_cartesian(
    xlim = c(-.1, .1)
  )

ggsave(
  plot_hlm,
  filename = p_file_here("figs", "turnover_fit.pdf")
)
#
# fit_hlm <- invoke_map(
#   list(
#     lm,
#     lmer
#   ),
#   list(
#     list(
#       formula = update.formula(
#         formula_hlm,
#         . ~ percent_entry + percent_exit + as.factor(uf) + as.factor(year) + .
#       ),
#     data = saeb_hierarchical
#     ),
#     list(
#       formula = update.formula(
#         formula_hlm,
#         . ~ percent_entry + percent_exit + (1|uf) + (1|year) + .
#     ),
#     data = saeb_hierarchical
#     )
#   )
# )
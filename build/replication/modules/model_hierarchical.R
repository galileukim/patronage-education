# hierarchical model ------------------------------------------------------
f <- stats::as.formula

saeb_hierarchical <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
)

finbra <- read_data(
  "finbra",
  "finbra.rds"
)

# censo_school <- read_data(
#   "censo_escolar",
#   "censo_school_mun.rds"
# )

censo_school_turnover <- read_data(
  "censo_escolar",
  "censo_school_turnover.rds"
)

# prepare data for estimation
saeb_hierarchical <- list(
  saeb_hierarchical,
  finbra,
) %>%
  reduce(
    left_join
  ) %>%
  mutate(
    budget_education_capita = budget_education / censo_pop
  )

# join school covariates
# censo_school %<>% 
#   filter(
#     dep == "municipal", year >= 2001
#   ) %>%
#   select(
#     state,
#     cod_ibge_6,
#     year,
#     location,
#     school_id,
#     school_name,
#     internet,
#     kitchen,
#     library,
#     lab_info,
#     meal,
#     starts_with('principal'),
#     sewer_grid,
#     toilet,
#     water_grid
#   )

# join_cols <- c("state", "cod_ibge_6", "school_id", "year")

# saeb_hlm %<>%
#   mutate(state = str_sub(cod_ibge_6, 1, 2)) %>% 
#   rename(school_id = cod_school) %>% 
#   mutate_at(join_cols, as.character) %>% 
#   left_join(
#     censo_school %>%
#       mutate_at(join_cols, as.character) %>% 
#       select(-location),
#     by = join_cols
#   )

# turnover
censo_school_turnover <- fread(
  here("data/censo_escolar/censo_school_turnover.csv.gz")
) %>% 
  rename(
    grade = grade_level,
    n_teacher = n,
    n_teacher_lag = n_lag
  )

join_cols <- c(join_cols, "grade")

saeb_hlm %<>%
  mutate_at(join_cols, as.character) %>%
  tidylog::left_join(
    censo_school_turnover %>% mutate_at(join_cols, as.character),
    by = join_cols
  )

# create log pop
saeb_hlm %<>%
  mutate(
    censo_log_pop = log(censo_pop)
  )

# break down teacher wages into quantiles
saeb_hlm %<>%
  mutate(
    saeb_teacher_work_school = if_else(year_working_school_teacher == "", NA_character_, year_working_school_teacher),
    grade = recode(grade, `4` = "5", `8` = "9")
  )

saeb_hlm %>% 
  fwrite_gz(
    here("data/saeb/saeb_hierarchical.csv")
  )

# estimation of hierarchical linear models to assess effect of teacher turnover on student learning
fe <- c(
  "(1 | year)",
  "(1 | state)"
)

teacher_cov <- c(
  "saeb_wage_teacher",
  "education_teacher",
  "year_as_teacher"
)

principal_cov <- c(
  "saeb_principal_female",
  "saeb_principal_education",
  "saeb_principal_appointment"
)

student_cov <- c(
  "parent_attend_college_student",
  "parents_school_meeting_student",
  "fridge_student",
  "failed_school_year_student"
)

school_cov <- c(
  "toilet",
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
    c("turnover_index*grade", fe)
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

sink(p_file_here("results", "hlm_result.tex"))
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
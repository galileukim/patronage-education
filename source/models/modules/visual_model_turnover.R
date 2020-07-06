# ==============================================================================
# visualize combined results from saeb hierarchical and spaece models
# ==============================================================================
library(stargazer)

fit_lmer <- read_model("fit_saeb_hierarchical.rds")
fit_spaece <- read_model("fit_spaece.rds")
fit_ivreg <- map(
  c("fit_ivreg_coalition.rds", "fit_ivreg_first_term.rds"),
  read_model
)

sink(here("figures", "tables", "student_learning.tex"))
mstar(
  fit_lmer, fit_spaece, fit_spaece, fit_ivreg,
  keep = c("turnover_index", "saeb_principal_experience", "saeb_teacher_work_school"),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 4))),
  dep.var.labels.include = F,
  covariate.labels = c(
    "Turnover index", "Turnover index $\\times$ Grade 9",
    "Teacher experience (2 years)", "Teacher experience (10 years)",
    "School principal experience (2 years)", "School principal experience (10 years)"
  ),
  dep.var.caption = "Student learning",
  column.labels = c("SAEB test score", "SPAECE test score"),
  column.separate = c(6, 2),
  model.names = FALSE,
  keep.stat = "n"
)
sink()

# visualization
mods <- c(
  fit_lmer[c("turnover_controls", "principal_teacher_controls")],
  fit_spaece["turnover_controls"]
)

names(mods) <- c("model turnover saeb", "model principal_teacher", "model turnover_spaece")

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

plot_model_turnover <- estimate_hlm %>%
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
    limits = c(
      "turnover_index",
      "saeb_principal_experienceless than 2 years",
      "saeb_teacher_work_schoolless than 2",
      "saeb_principal_appointmentpublic exam",
      "saeb_principal_educationhigher education",
      "education_teacherhigher education",
      "saeb_wage_teachermore than 3000"
    ),
    labels = c(
      "turnover_index" = "Turnover index",
      "saeb_principal_experienceless than 2 years" = "Principal rotation",
      "saeb_teacher_work_schoolless than 2" = "Teacher rotation",
      "saeb_principal_appointmentpublic exam" = "Principal public exam",
      "saeb_principal_educationhigher education" = "Principal higher education",
      "education_teacherhigher education" = "Teacher higher education",
      "saeb_wage_teachermore than 3000" = "Teacher wage more than $750"
    )
  ) +
  coord_flip(
    xlim = c(-0.2, 0.2)
  )
  
save_fig(
  plot_model_turnover,
  file = "model_turnover_learning.pdf"
)
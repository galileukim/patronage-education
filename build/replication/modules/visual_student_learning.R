# ==============================================================================
# visualize combined results from saeb hierarchical and spaece models
# ==============================================================================
fit_lmer <- read_model("fit_saeb_hierarchical.rds")
fit_spaece <- read_model("fit_spaece.rds")

sink(here("replication", "results", "student_learning.tex"))
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
  ) 
#   +
#   coord_cartesian(
#     xlim = c(-.1, .1)
#   )

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
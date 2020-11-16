# test scores -------------------------------------------------------------
# note that this data only includes info on muncipal public schools
saeb_school <- read_data(
  "saeb",
  "saeb_school.rds"
)

censo_school_turnover <- read_data(
  "censo_escolar",
  "censo_school_turnover.rds"
)

# trim data
saeb_school <- saeb_school %>%
  mutate(
    grade_level = as.numeric(as.character(grade_level))
  )

censo_school_turnover <- censo_school_turnover %>%
  filter(n > 2)

saeb_school <- saeb_school %>%
  filter(
    !is.na(grade_level),
    year >= 2003
  )

saeb_school %>%
  gg_summary(
    year,
    grade_exam
  )

saeb_school %>%
  ggplot(
    aes(
      year, grade_exam,
      group = dep, col = dep
    )
  ) +
  geom_line() +
  geom_point() +
  labs(
    caption = "Average test scores, by administrative level. Includes math and Portuguese test scores."
  ) +
  facet_wrap(
    grade ~ subject
  )

saeb_descriptive <- saeb_school %>%
  join_covariate()

saeb_descriptive %>%
  gg_summary(
    censo_pop,
    grade_exam
  ) +
  scale_x_log10()

saeb_descriptive %>%
  gg_summary(
    I(budget_education / censo_pop),
    grade_exam
  )

plot_turnover_exam <- saeb_school %>%
  inner_join(
    censo_school_turnover,
    by = c("cod_ibge_6", "year", "school_id", "grade_level")
  ) %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2)
  ) %>%
  group_by(
    state,
    year
  ) %>%
  mutate(
    turnover_index = turnover_index - mean(turnover_index, na.rm = T),
    grade_exam = grade_exam - mean(grade_exam, na.rm = T)
  ) %>%
  ungroup() %>%
  gg_summary(
    turnover_index,
    grade_exam,
    smooth = FALSE
  ) +
  geom_smooth(
    method = "lm"
  ) +
  coord_cartesian(
    ylim = c(-4, -0.5)
  )

save_fig(
  plot_turnover_exam,
  "plot_turnover_exam.pdf"
)
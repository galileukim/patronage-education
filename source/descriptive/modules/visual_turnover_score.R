# ==============================================================================
# visualize the correlation between staffturnover and student test scores
# ==============================================================================
print("load-in data")
saeb <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
) %>%
  select(
    state,
    cod_ibge_6,
    year,
    school_id,
    grade_level,
    grade_exam
  )

spaece <- read_data(
  "spaece",
  "spaece.rds"
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

saeb_turnover <- saeb %>%
  left_join(
    censo_school_turnover,
    by = c("cod_ibge_6", "year", "school_id", "grade_level")
  )

spaece_turnover <- spaece %>%
  filter(
    dep == "municipal",
    grade_level != 2
  ) %>%
  left_join(
    censo_school_turnover,
    by = c("cod_ibge_6", "year", "school_id", "grade_level")
  )

print("data visualization")
test_turnover <- list(spaece = spaece_turnover, saeb = saeb_turnover)
exams <- c(quo(spaece_mean), quo(grade_exam))

test_turnover_scaled <- map2_dfr(
  test_turnover,
  exams,
  .f = ~ mutate(.x, score = all_of(!!.y)),
  .id = "test"
) %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    grade_level = as.factor(grade_level),
  ) %>%
  group_by(test, state, year) %>%
  mutate(
    turnover_index = scale_z(turnover_index),
    score = scale_z(score)
  ) %>%
  ungroup()

plot_turnover <- test_turnover_scaled %>%
  ggplot(
    aes(
      turnover_index, score,
      group = grade_level,
      col = grade_level
    )
  ) +
  geom_smooth(
    method = lm,
    formula = y ~ splines::bs(x, 3),
    se = F
  ) +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  facet_wrap(test ~ .) +
  labs(
    x = "Teacher turnover",
    y = "Student-test scores (z-score)"
  )

print("write-out plot")
save_fig(
  plot_turnover,
  "plot_turnover_effect_on_test_score.pdf"
)
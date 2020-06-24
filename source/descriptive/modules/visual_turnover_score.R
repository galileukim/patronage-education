# turnover spaece ---------------------------------------------------------
saeb <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
) %>%
  select(
    cod_ibge_6,
    school_id,
    year,
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

plots_turnover_score <- map2(
  list(
    spaece_turnover,
    saeb_turnover
  ),
  c("spaece_mean", "grade_exam"),
  ~ mutate(
    .x,
    grade_level = as.factor(grade_level)
  ) %>%
    ggplot(
      aes_string(
        "turnover_index",
        .y,
        group = "grade_level",
        col = "grade_level"
      )
    ) +
    geom_smooth(
      method = lm, formula = y ~ splines::bs(x, 3),
      se = F
    )
) %>%
  set_names(
    c("spaece", "saeb")
  )

plot_turnover <- map2_dfr(
  list(spaece_turnover, saeb_turnover),
  c(quo(spaece_mean), quo(grade_exam)),
  .f = ~ mutate(.x, score = all_of(!!.y)),
  .id = "test"
) %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    test = dplyr::recode(test, `1` = "spaece", `2` = "saeb")
  ) %>%
  group_by(state, year, test) %>%
  mutate(
    turnover_index = scale_z(turnover_index),
    score = scale_z(score)
  )

plot_turnover %>%
  ggplot(
    aes(
      turnover_index, score,
      group = grade_level,
      col = as.factor(grade_level)
    )
  ) +
  geom_smooth(
    method = lm, formula = y ~ splines::bs(x, 2),
    se = F
  ) +
  facet_wrap(test ~ .)

# plot_turnover_spaece <- plots_turnover_score %>%
#   pluck("spaece") +
#   annotate(
#     "text",
#     label = "5th grade",
#     size = 4,
#     col = "grey65",
#     0.6, 200
#   ) +
#   annotate(
#     "text",
#     label = "9th grade",
#     size = 4,
#     col = "grey65",
#     0.7, 225
#   ) +
#   labs(
#     x = "Teacher turnover (index)",
#     y = "Test scores (SPAECE)"
#   ) +
#   theme(
#     legend.position = "none"
#   )

# plot_turnover_saeb <- plot_turnover_score %>%
#   pluck("saeb") +
#   annotate(
#     "text",
#     label = "5th grade",
#     size = 4,
#     col = "grey65",
#     0.6, 165
#   ) +
#   annotate(
#     "text",
#     label = "9th grade",
#     size = 4,
#     col = "grey65",
#     0.7, 178
#   ) +
#   labs(
#     x = "Teacher turnover (index)",
#     y = "Test scores (SPAECE)"
#   ) +
#   theme(
#     legend.position = "none"
#   )

plot_turnover_score <- gridExtra::grid.arrange(
  plot_turnover_spaece,
  plot_turnover_saeb,
  ncol = 2
)

ggsave(
  plot_spaece,
  filename = p_file_here("figs", "turnover_spaece.pdf")
)
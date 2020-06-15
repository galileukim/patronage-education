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
    grade_level %in% c(5, 9) &
      n_teacher >= 5
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

plot_turnover_score <- map2(
  list(
    spaece_turnover,
    saeb_turnover
  ),
  c("spaece_mean"),
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
    ) +
    annotate(
      "text",
      label = "2nd grade",
      size = 2,
      col = "grey65",
      0.45, 165
    ) +
    annotate(
      "text",
      label = "5th grade",
      size = 2,
      col = "grey65",
      0.6, 200
    ) +
    annotate(
      "text",
      label = "9th grade",
      size = 2,
      col = "grey65",
      0.7, 225
    ) +
    labs(
      x = "Teacher turnover (index)",
      y = "Test scores (SPAECE)"
    ) +
    theme(
      legend.position = "none"
    )
)

spaece_turnover %>% mutate(
    grade_level = as.factor(grade_level)
  ) %>%
    ggplot(
      aes_string(
        "turnover_index",
        "spaece_mean",
        group = "grade_level",
        col = "grade_level"
      )
    ) +
    geom_smooth(
      method = lm, formula = y ~ splines::bs(x, 3),
      se = F
    ) +
    annotate(
      "text",
      label = "2nd grade",
      size = 2,
      col = "grey65",
      0.45, 165
    ) +
    annotate(
      "text",
      label = "5th grade",
      size = 2,
      col = "grey65",
      0.6, 200
    ) +
    annotate(
      "text",
      label = "9th grade",
      size = 2,
      col = "grey65",
      0.7, 225
    ) +
    labs(
      x = "Teacher turnover (index)",
      y = "Test scores (SPAECE)"
    ) +
    theme(
      legend.position = "none"
    )
ggsave(
  plot_spaece,
  filename = p_file_here("figs", "turnover_spaece.pdf")
)
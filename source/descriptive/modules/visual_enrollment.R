censo_class <- read_data(
  "censo_escolar",
  "censo_class_dep.rds"
)

# student coverage by administratioon
plot_dep <- censo_class %>% 
  mutate(
    stage = case_when(
      between(grade_level, 1, 4) ~ "lower school",
      between(grade_level, 5, 9) ~ "middle school",
      T ~ NA_character_
    )
  ) %>% 
  filter(
    !is.na(stage), 
    dep != "federal"
  ) %>% 
  group_by(
    stage,
    year,
    dep
  ) %>% 
  summarise(
    n = sum(total)
  ) %>% 
  mutate(
    freq = n/sum(n)
  ) %>% 
  ggplot(
    aes(
      year,
      freq,
      col = dep,
      shape = dep
    )
  ) +
  geom_line(
    aes(
      linetype = dep
    )
  ) +
  geom_point(
    size = 3
  ) +
  facet_wrap(
    . ~ stage,
    nrow = 1
  ) +
  labs(
    x = 'Enrollment (percentage)',
    y = 'Year'
  )

save_fig(
  plot_dep,
  "descriptive_enrollment.pdf"
)

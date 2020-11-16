# test scores -------------------------------------------------------------
saeb_school <- read_data(
    "saeb",
    "saeb_school.rds"
)

saeb_public <- saeb_school %>%
  filter(
    dep != "",
    !is.na(grade_level),
    dep != "particular",
    year >= 2003
  ) %>% 
  mutate(
    year = as.factor(year)
  )

saeb_school %>%
    gg_summary(
      year,
      grade_exam
    )

saeb_public %>% 
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

saeb_descriptive <- saeb_exam_mun %>% 
  join_covariate

saeb_descriptive %>% 
  gg_summary(
    censo_pop, 
    mean_grade_exam
  ) +
  scale_x_log10()

saeb_descriptive %>% 
  gg_summary(
    I(budget_education/censo_pop),
    mean_grade_exam
  )

saeb_exam_school %>% 
  inner_join(
    censo_school_turnover,
    by = c('cod_ibge_6', 'year', 'school_id')
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
    mean_grade_exam = mean_grade_exam - mean(mean_grade_exam, na.rm = T)
  ) %>% 
  ungroup() %>% 
  gg_summary(
    turnover_index,
    mean_grade_exam
  )

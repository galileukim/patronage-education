# rais by category
rais_main_categories <- c("education", "administration", "services", "healthcare")

rais_mun <- read_data(
  "rais",
  "rais_mun.rds"
)

rais_category <- rais_mun %>% 
  filter(
    cbo_category != "",
    !is.na(cbo_category),
    year >= 2003
  ) %>% 
  mutate(
    cbo_category = if_else(
      cbo_category %in% rais_main_categories,
      cbo_category,
      'other'
    )
  ) %>% 
  group_by(
    year,
    cbo_category
  ) %>% 
  summarise(
    total_sum = sum(total),
    hired = sum(mean_hired*total)/total_sum*100,
    fired = sum(mean_fired*total)/total_sum*100,
    .groups = "drop"
  ) 
  
rais_category <- rais_category%>%
  rename(
    total = total_sum
  ) %>% 
  pivot_longer(
    cols = c(-year, -cbo_category),
    "type",
    "value"
  ) %>% 
  mutate(
    cbo_category = fct_relevel(cbo_category, c(rais_main_categories, 'other'))
  ) %>% 
  ungroup()

plot_cat_total <- rais_category %>% 
  filter(
    type == 'total',
    year >= 2003
  ) %>% 
  ggplot(
    aes(
      year,
      value/1e6,
      fill = cbo_category
    )
  ) +
  geom_bar(
    stat = 'identity',
    position = 'stack'
  ) +
  # mandate_year(
  #   seq(2005, 2013, 4)
  # ) +
  labs(
    x = 'Year',
    y = 'Total (millions)'
  ) +
  theme(legend.title = element_blank())

save_fig(
  plot_cat_total,
  "descriptive_staff_breakdown.pdf"
)

plot_cat_turnover <- rais_category %>% 
  filter(
    type != 'total',
    year >= 2003,
    cbo_category %in% rais_main_categories
  ) %>% 
  mutate(
    type = fct_relevel(type, 'hired')
  ) %>% 
  gg_point_line(
    aes(
      year,
      value,
      color = type
    )
  ) + 
  mandate_year(
    seq(2005, 2013, 4)
  ) +
  facet_wrap(
    cbo_category ~ .
  ) +
  labs(
    x = "Year",
    y = "Percentage"
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2013, 4)
  ) +
  theme(
    legend.title = element_blank()
  )

  save_fig(
    plot_cat_turnover,
    "descriptive_staff_turnover.pdf"
  )

finbra <- read_data(
    "finbra",
    "finbra.rds"
)

plot_budget <- finbra %>%
  pivot_longer(
    -c(cod_ibge_6, year),
    names_to = "category",
    values_to = "value"
  ) %>% 
  group_by(year, category) %>% 
  summarise(
    value = sum(value, na.rm = T)/1e9
  ) %>% 
  filter(
    !str_detect(category,"total|legislative|industry") 
  ) %>% 
  ggplot() +
  geom_area(
    aes(
      year,
      value,
      fill = category,
      group = category
    )
  ) +
  labs(
    x = "Year",
    y = "Total (billions)"
  )

plot_budget %>% 
  ggsave(
    filename = p_file_here('figs', "budget.pdf")
  )
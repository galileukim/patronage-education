# ==============================================================================
# explaining staff turnover as a result of executive-legislative bargain
# ==============================================================================
# show that irrespective of level of economic development, the same dynamic is present
# turnover index on coalition
source(
  here::here("source", "replication", "setup.R")
)

censo_school_turnover <- read_data(
    "censo_escolar",
    "censo_school_turnover.rds"
)

saeb_turnover <- censo_school_turnover %>% 
  left_join(
    censo_school %>% 
      mutate(
        school_id = as.integer(school_id)
      ) %>% 
      select(-state, -location),
    by = c('cod_ibge_6', 'year', 'school_id')
  ) %>%
  join_covariate

f_turnover <- c(
  as.formula(turnover_index ~ coalition_share + as.factor(year) + as.factor(state)),
  as.formula(
    turnover_index ~ coalition_share + as.factor(year) + as.factor(state) + 
      location +
      toilet + internet + meal +
      censo_log_pop + censo_median_wage + censo_rural
  )
)

fit_turnover <- map(
  f_turnover,
  ~lm(., data = saeb_turnover)
)

# logit model
f_logit <- fm(
  rais_hired,
  coalition_share,
  rais_category,
  election_year,
  state,
  rais_edu,
  rais_wage, 
  censo_log_pop, 
  mayor_reelected,
  censo_median_wage, 
  mayor_campaign, 
  mayor_coalition_size
)

fit_turnover <- c(
  f_logit,
  update(f_logit, rais_fired ~ .)
) %>% 
  map(
    ~logit(., data = rais_fit)
  )
  
plot_model(
  fit_hired,
  type = 'pred',
  terms = c('coalition_share [all]')
) +
  coord_cartesian(
    ylim = c(0.05, 0.07)
  )

plot_model(
  fit_fired,
  type = 'pred',
  terms = c('coalition_share [all]')
)

plot_logit_int <- map(
  c(
    'rural' = 'censo_rural[0.25, 0.5, 0.75]', 
    'population' = 'censo_pop[1e4, 5e4, 1e5]',
    'wage' = 'censo_median_wage [172.12, 250.77, 341.896]'
  ),
  ~plot_model(fit_hired, type = 'pred', title = NULL, terms = c('coalition_share [all]', .))
)

walk2(
  plot_logit_int,
  names(plot_logit_int),
  ~ggsave(
    .x,
    filename = p_file_here('figs', paste0('plot_logit_', .y, '.pdf'))
  )
)

# print plot
plot_logit <- fit_hired %>%
  tidycoef(
    vars = "coalition|mayor_reelected|censo_log_pop|censo_median_wage"
  ) +
  theme(
    axis.title = element_blank()
  )

ggsave(
  plot_logit,
  filename = p_file_here('figs', "fit_logit.pdf")
)

# municipal fe model
fit_hired_mun <- fit_felm(
  fit_hired_mun,
  "rais_hired",
  predictor = c("coalition_share + as.factor(rais_category) + mayor_reelected"),
  control = controls,
  data = rais_edu_mun %>% 
    mutate(
        rais_category = fct_relevel(rais_category, 'teacher')
    )
)

fit_fired_mun <- fit_felm(
  fit_hired_mun,
  "rais_fired",
  predictor = c("coalition_share + as.factor(rais_category) + mayor_reelected"),
  control = controls,
  data = rais_edu_mun %>% 
    mutate(
      rais_category = fct_relevel(rais_category, 'teacher')
    )
)

# table of results
sink(p_file_here('results', 'turnover_result.tex'))
mstar(
  list(
    fit_turnover[[2]],
    fit_hired,
    fit_fired,
    fit_hired_mun,
    fit_fired_mun
  ),
  keep = c('coalition_share', 'rais_category'),
  add.lines = list(c("Controls", rep("\\checkmark", 5))),
  covariate.labels = c('Executive share of seats', 'School principal', 'Executive share of seats X School principal'),
  dep.var.labels.include = F,
  column.labels = c('Turnover index', 'Hired (Logit)', 'Fired (Logit)', 'Hired (FE)', 'Fired (FE)'),
  column.separate = rep(1, 5),
  model.names = FALSE
)
sink()

plot_mun <- fit_hired_mun%>% 
  tidycoef(
    vars = "coalition|mayor_reelected|censo_log_pop|censo_median_wage"
  ) +
  # scale_y_discrete(
  #   labels = rev(c("Share of legislative seats", "Population size (logged)", "Median wage", "Size of coalition", "Reelected", "Share of legislative seats (sq.)"))
  # ) +
  theme(
    axis.title = element_blank()
  )

ggsave(
  plot_mun,
  filename = p_file_here('figs', "fit_mun.pdf")
)

plot_hired <- plot_grid(
  plot_logit,
  plot_mun,
  nrow = 2,
  labels = c("Logit", "OLS"),
  label_size = 7.5
)

ggsave(
  plot_hired,
  filename = p_file_here('figs', "hired_fit.pdf")
)

# predicted vals
model_fit_hired_mun <- model.frame(
  fit_hired_mun
) 

plot_hired <- model_fit_hired_mun %>% 
  mutate(
    predict = fit_hired_mun$fitted.values
  ) %>% 
  group_by(coalition_share) %>% 
  mutate(predict_mean = mean(rais_hired, na.rm = T)) %>% 
  ggplot(
    aes(x = coalition_share)
  ) +
  geom_count(
    aes(y = predict_mean),
    alpha = 0.1
  ) +
  geom_smooth(
    aes(y = predict),
    method = lm,
    formula = y ~ x,
    col = "coral3"
  ) +
  coord_cartesian(
    ylim = c(0.05, 0.125)
  ) +
  labs(
    x = "Share of legislative seats",
    y = "Proportion of new hires"
  ) +
  theme(legend.position = 'none')

model_fit_fired_mun <- model.frame(
  fit_fired_mun
) 

plot_fired <- model_fit_fired_mun %>% 
  mutate(
    predict = fit_fired_mun$fitted.values
  ) %>% 
  group_by(coalition_share) %>% 
  mutate(predict_mean = mean(rais_fired, na.rm = T)) %>% 
  ggplot(
    aes(x = coalition_share)
  ) +
  geom_count(
    aes(y = predict_mean),
    alpha = 0.1
  ) +
  geom_smooth(
    aes(y = predict),
    method = lm,
    formula = y ~ x,
    col = "coral3"
  ) +
  coord_cartesian(
    ylim = c(0, 0.075)
  ) +
  labs(
    x = "Share of legislative seats",
    y = "Proportion of dismissals"
  ) +
  theme(legend.position = 'none')

plot_turnover <- grid.arrange(
  grobs = list(
    plot_hired,
    plot_fired
  ),
  ncol = 2
)

ggsave(
  plot_turnover,
  filename = p_file_here('figs', "plot_pred.pdf")
)
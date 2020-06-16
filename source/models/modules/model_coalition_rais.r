# ==============================================================================
# set-up
# ==============================================================================
# construct data to estimate effect of coalition on edu staff turnover
source(
    here::here("source", "models", "setup.R")
)

rais_edu <- read_data(
  "rais",
  "rais_edu.rds"
)

model_rais_micro <- rais_edu %>% 
  filter(
    year >= 2003
  ) %>%
  join_covariate() %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    rais_category = fct_relevel(rais_category, 'teacher'),
    budget_education_capita = budget_education / censo_pop
    ) %>%
  fix_scale()

rais_edu_mun <- read_data(
  "rais",
  "rais_mun_edu.rds"
)

model_rais_mun <- rais_edu %>% 
  join_covariate() %>%
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    rais_category = fct_relevel(rais_category, "teacher")
  )

# ==============================================================================
# estimation of effect of coalition share on staff turnover
# ============================================================================== 
controls_logit <- c(
  mun_cov, rais_cov, mayor_cov, chamber_cov
)

f_logit <- formulate(
  "rais_hired",
  "coalition_share",
  controls_logit,
  fe = c("state", "year")
)

fit_turnover <- c(
  f_logit,
  update(f_logit, rais_fired ~ .)
) %>% 
  map(
    ~logit(., data = model_rais_micro)
  )
  
plot_model(
  fit_hired,
  type = "pred",
  terms = c("coalition_share [all]")
) +
  coord_cartesian(
    ylim = c(0.05, 0.07)
  )

plot_model(
  fit_fired,
  type = "pred",
  terms = c("coalition_share [all]")
)

plot_logit_int <- map(
  c(
    "rural" = "censo_rural[0.25, 0.5, 0.75]", 
    "population" = "censo_pop[1e4, 5e4, 1e5]",
    "wage" = "censo_median_wage [172.12, 250.77, 341.896]"
  ),
  ~plot_model(fit_hired, type = "pred", title = NULL, terms = c("coalition_share [all]", .))
)

walk2(
  plot_logit_int,
  names(plot_logit_int),
  ~ggsave(
    .x,
    filename = p_file_here("figs", paste0("plot_logit_", .y, ".pdf"))
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
  filename = p_file_here("figs", "fit_logit.pdf")
)

# municipal fe model
fit_hired_mun <- fit_felm(
  fit_hired_mun,
  "rais_hired",
  predictor = c("coalition_share + as.factor(rais_category) + mayor_reelected"),
  control = controls,
  data = rais_edu_mun %>% 
    mutate(
        rais_category = fct_relevel(rais_category, "teacher")
    )
)

fit_fired_mun <- fit_felm(
  fit_hired_mun,
  "rais_fired",
  predictor = c("coalition_share + as.factor(rais_category) + mayor_reelected"),
  control = controls,
  data = rais_edu_mun %>% 
    mutate(
      rais_category = fct_relevel(rais_category, "teacher")
    )
)
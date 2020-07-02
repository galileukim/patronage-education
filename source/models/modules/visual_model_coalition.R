# ==============================================================================
# visualize conditional probability of being fired/hired by bureaucratic cat.
# ==============================================================================
fit_logit <- read_model(
  "fit_logit_turnover_coalition.rds"
)

fit_felm <- read_model(
  "fit_felm_turnover_coalition.rds"
)

fit_turnover <- read_model(
  "fit_coalition_turnover.rds"
) %>%
pluck(2) # extract model with controls

# turn into debugging
fit_logit <- fit_logit %>%
  map(
    . %>%
      modify_at(c("model"), ~sample_frac(., 0.01))
  )

models_table <- list(
    fit_turnover,
    fit_logit,
    fit_felm
  )

sink(here("replication", "results", "turnover_result.tex"))
mstar(
  type = "text",
  models_table,
  keep = c('coalition_share', 'rais_category'),
  add.lines = list(c("Controls", rep("\\checkmark", 5))),
  covariate.labels = c('Executive share of seats', 'School principal', 'Executive share of seats X School principal'),
  dep.var.labels.include = F,
  column.labels = c('Turnover index', 'Hired (Logit)', 'Fired (Logit)', 'Hired (FE)', 'Fired (FE)'),
  column.separate = rep(1, 5),
  model.names = FALSE
)
sink()

plot_interactions_logit <- map2(
  fit_logit,
  c("hired", "fired"),
  ~ sjPlot::plot_model(
    .,
    type = "pred",
    terms = c("coalition_share [all]", "rais_category"),
    title = ""
  ) +
  labs(
    x = "share of legislative seats",
    y = "percentage"
  )
)

n <- 100
predict_logit <- tibble(
  coalition_share = seq(0, 1, length.out = n * 2)
  rais_category = 
)

fit_logit[[1]]$model %>% ggplot(aes(coalition_share, rais_hired)) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(. ~ rais_category)

gridExtra::grid.arrange(
  grobs = plot_interactions_logit,
  nrow = 2
)

save_fig(
  plot_interactions_logit,
  "plot_interactions_logit.pdf"
)

# ==============================================================================
# incorporate additional municipal covariates to disentangle effect
# ==============================================================================


# rural_terciles <- fit_logit %>%
#   pluck(1, "model") %>%

# plot_logit_int <- map(
#   c(
#     "rural" = "censo_rural[0.25, 0.5, 0.75]",
#     # "population" = "censo_pop[1e4, 5e4, 1e5]",
#     "wage" = "censo_median_wage[172.12, 250.77, 341.896]"
#   ),
#   ~ sjPlot::plot_model(fit_logit[[1]], type = "pred", title = NULL, terms = c("coalition_share [all]", .))
# )

# walk2(
#   plot_logit_int,
#   names(plot_logit_int),
#   ~ ggsave(
#     .x,
#     filename = p_file_here("figs", paste0("plot_logit_", .y, ".pdf"))
#   )
# )

print("plots of coefficients")
plot_logit <- fit_logit %>%
  map(
    ~ tidycoef(
      .,
    vars = "coalition|mayor_reelected|censo_log_pop|censo_median_wage"
  ) +
  theme(
    axis.title = element_blank()
  )
  )


# ==============================================================================
print("visualize association between coalition share and patronage (municipal)")
# ==============================================================================
coef_mun <- fit_felm %>%
  set_names(c("hired", "fired")) %>%
  map_dfr(broom::tidy, conf.int = T, .id = "model")

plot_coef_mun <- coef_mun %>%
  ggplot(
    aes(
      x = term,
      y = estimate,
      color = model
    )
  ) +
  geom_point() +
  geom_pointrange(
    aes(
      ymin = conf.low,
      ymax = conf.high
    )
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  coord_flip()

save_fig(
  plot_coef_mun,
  "plot_coalition_coef_mun.pdf"
)
# ---------------------------------------------------------------------------- #
message("visualize effects for the second term")

message("read in data")
fit_logit_second_term <- read_model(
  "fit_logit_turnover_coalition_second_term.rds"
)

fit_felm_second_term <- read_model(
  "fit_felm_turnover_coalition_second_term.rds"
)

fit_turnover_second_term <- read_model(
  "fit_coalition_turnover_second_term.rds"
)

fit_felm_robustness <- read_model(
  "fit_felm_turnover_coalition_robustness.rds"
)

# ---------------------------------------------------------------------------- #
models_complete <- list(
  fit_logit_second_term,
  fit_felm_second_term,
  fit_turnover_second_term
)

coef_mun_second <- models_complete %>%
  set_names(c("hired (individual)", "hired (municipal)", "turnover")) %>%
  map_dfr(broom::tidy, .id = "model") %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

plot_coef_mun_second <- coef_mun_second %>%
  filter(
    term == "coalition_share"
  ) %>%
  ggplot(
    aes(
      x = model,
      y = estimate
    )
  ) +
  geom_point(
    color = matte_indigo
  ) +
  geom_pointrange(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    color = matte_indigo
    # position = position_dodge(.5)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  )

save_fig(
  plot_coef_mun_second,
  "plot_coalition_coef_mun_second.pdf"
)

# ---------------------------------------------------------------------------- #
message("interaction with municipal covariates")

subgroups <- names(fit_felm_robustness)

predict_interactions <- map2_dfr(
  fit_felm_robustness,
  subgroups,
  ~ ggeffects::ggpredict(
    .x,
    terms = c(
      "coalition_share",
      .y
    )
  ),
  .id = "model"
)

plot_interactions <- predict_interactions %>%
  mutate(
    model = recode(
      model,
      "censo_log_pop" = "population",
      "censo_median_wage" = "economic development",
      "censo_rural" = "rural population",
      "mayor_coalition_size" = "size of coalition"
    )
  ) %>%
  ggplot(
      aes(x, predicted, color = group, group = group)
    ) +
    geom_line() +
    scale_colour_discrete(
      labels = c("low", "medium", "high")
    ) +
    labs(
      x = "Share of allied seats",
      y = "Predicted"
    ) +
    facet_wrap(
      model ~ .
    )

save_fig(
  plot_interactions,
  "plot_coalition_robustness.pdf"
)
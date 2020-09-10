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
  set_names(
    c("baseline", "controls")
  )

# turn into debugging
fit_logit <- fit_logit %>%
  map(
    . %>%
      modify_at(c("model"), ~ sample_frac(., 0.01))
  )

models_table <- list(
  fit_turnover,
  fit_felm,
  fit_logit
)

sink(here("figures", "results", "turnover_result.tex"))
mstar(
  models_table,
  keep = c("coalition_share", "rais_category"),
  add.lines = list(
    c("Controls", rep(c("", "\\checkmark"), 3)),
    c("State and year FE", rep("\\checkmarck", 6))
  ),
  dep.var.caption = "Outcome",
  covariate.labels = c("Share of legislative seats", "School principal", "Executive share of seats X School principal"),
  dep.var.labels.include = F,
  column.labels = c("Turnover index (municipal)", "Hires (municipal)", "Hires (individual)"),
  column.separate = rep(2, 3),
  model.names = FALSE
)
sink()

plot_interactions_logit <- fit_logit[["controls"]] %>%
  sjPlot::plot_model(
    .,
    type = "pred",
    terms = c("coalition_share [all]", "rais_category"),
    title = ""
  ) +
  labs(
    x = "share of legislative seats",
    y = "probability of hire",
    color = "occupation category"
  ) +
  scale_color_discrete()

save_fig(
  plot_interactions_logit,
  "plot_interactions_logit.pdf"
)

# ==============================================================================
# incorporate additional municipal covariates to disentangle effect
# ==============================================================================
# plot_logit_int <- map(
#   c(
#     "rural" = "censo_rural[0.25, 0.5, 0.75]",
#     # "population" = "censo_pop[1e4, 5e4, 1e5]",
#     "wage" = "censo_median_wage[172.12, 250.77, 341.896]"
#   ),
#   ~ sjPlot::plot_model(fit_logit[["controls"]], type = "pred", title = NULL, terms = c("coalition_share [all]", .))
# )

# sjPlot::plot_model(fit_logit[["controls"]], type = "pred", title = NULL, terms = c("coalition_share [all]", "censo_median_wage[172.12, 250.77, 341.896]"))

# walk2(
#   plot_logit_int,
#   names(plot_logit_int),
#   ~ ggsave(
#     .x,
#     filename = p_file_here("plots", paste0("plot_logit_", .y, ".pdf"))
#   )
# )

print("plots of coefficients")
plot_logit <- fit_logit[["controls"]] %>%
  tidycoef(
    vars = "coalition|mayor_reelected|rais|censo_log_pop|censo_median_wage|party"
  ) +
  theme(
    axis.title = element_blank()
  )

# ==============================================================================
print("visualize association between coalition share and patronage (municipal)")
# ==============================================================================
coef_mun <- models_table %>%
  map(pluck("controls")) %>%
  set_names(c("turnover", "hired (logit)", "hired (felm)")) %>%
  map_dfr(broom::tidy, .id = "model") %>%
  mutate(
    conf.low = estimate - 1.96*std.error,
    conf.high = estimate + 1.96*std.error
  )

select_vars <- c("coalition_share", "censo_log_pop", "censo_lit_rate", "censo_median_wage", "chamber_size")

plot_coef_mun <- coef_mun %>%
  filter(
    term %in% select_vars
  ) %>%
  ggplot(
    aes(
      x = term,
      y = estimate,
      color = model
    )
  ) +
  geom_point(
    position = position_dodge(.5)
  ) +
  geom_pointrange(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    position = position_dodge(.5)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  scale_x_discrete(
    limits = rev(c(
      "coalition_share",
      "chamber_size",
      "censo_median_wage",
      "censo_log_pop",
      "censo_lit_rate"
    )),
    labels = c(
      "coalition_share" = "Share of allied seats",
      "chamber_size" = "Number of seats in city council",
      "censo_median_wage" = "Municipal median wage",
      "censo_log_pop" = "Municipal population (logged)",
      "censo_lit_rate" = "Literacy rate"
    )
   ) +
  coord_flip()

save_fig(
  plot_coef_mun,
  "plot_coalition_coef_mun.pdf"
)

# ---------------------------------------------------------------------------- #
message("visualize effects for the second term")

fit_logit_second_term <- read_model(
  "fit_logit_turnover_coalition_second_term.rds"
)

fit_felm_second_term <- read_model(
  "fit_felm_turnover_coalition_second_term.rds"
)

fit_turnover_second_term <- read_model(
  "fit_coalition_turnover_second_term.rds"
)

models_complete <- list(
  fit_logit %>% pluck("controls"),
  fit_logit_second_term,
  fit_felm %>% pluck("controls"),
  fit_felm_second_term,
  fit_turnover %>% pluck("controls"),
  fit_turnover_second_term
)

coef_mun_second <- models_complete %>%
  set_names(c("hired (individual)", "hired (individual - second)", "hired (municipal)", "hired (municipal - second)", "turnover", "turnover (second)")) %>%
  map_dfr(broom::tidy, .id = "model") %>%
  mutate(
    conf.low = estimate - 1.96*std.error,
    conf.high = estimate + 1.96*std.error
  )

plot_coef_mun_second <- coef_mun %>%
  filter(
    term == "coalition_share"
  ) %>%
  ggplot(
    aes(
      x = model,
      y = estimate,
      color = model
    )
  ) +
  geom_point(
    position = position_dodge()
  ) +
  geom_pointrange(
    aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    position = position_dodge(.5)
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) 

save_fig(
  plot_coef_mun_second,
  "plot_coalition_coef_mun_second.pdf"
)
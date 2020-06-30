
# ==============================================================================
# evaluate whether mayors and city councilors face different reelection
# incentives in their political careers 
# ==============================================================================
source("source/models/modules/preprocess_patronage_reelection.R")

library(margins)

print("produce motivating descriptives on differential returns to patronage")

plot_patronage <- patronage_reelection %>%
  gg_summary(
    prop_hired,
    reelected,
    smooth = F
  ) +
  facet_wrap(
    type ~ .
  ) +
  coord_cartesian(ylim = c(0.5, 0.65)) +
  theme_bw() +
  theme_clean

save_fig(
  plot_patronage,
  "patronage_reelection.pdf"
)

print("estimate probability of being reelected differential")

fe <- c("state", "election_year")

politician_covariates <- c(
  "campaign", "edu", "gender", "party_factor"
)

censo_covariates <- c(
  "censo_median_wage", "censo_log_pop",
  "censo_rural", "censo_lit_rate"
)

controls <- c(
  politician_covariates,
  censo_covariates
)

f_reelection <- formulate(
  "reelected",
  "prop_hired",
  controls,
  fe
)

fit_reelection_mayor <- glm(
  f_reelection,
  family = 'binomial',
  data = patronage_reelection %>% 
    filter(position == 'prefeito')
)

f_reelection_councilor <- update(f_reelection, . ~ prop_hired*mayor_coalition_member + .)

fit_reelection_councilor <- glm(
  f_reelection_councilor,
  family = 'binomial',
  data = patronage_reelection %>% 
    filter(position == 'vereador')
)

marginal_mayor <- margins(
  fit_reelection_mayor,
  variables = 'prop_hired'
)

marginal_councilor <- margins(
  fit_reelection_councilor,
  variables = 'prop_hired',
  at = list(
    mayor_coalition_member = c('coalition_member', 'non_coalition_member')
  )
)

average_marginal_effects <- list(
  mayor = marginal_mayor,
  councilor = marginal_councilor
) %>% 
  map_dfr(
    ~summary(.),
    .id = 'type'
  ) %>% 
  mutate(
    type = if_else(
      type == 'mayor', 
      'mayor', 
      as.character(mayor_coalition_member)
    ) %>% 
      fct_relabel(~str_replace_all(., "_", " ")) %>% 
      fct_relevel(
        "mayor"
      )
  )

plot_ame <- average_marginal_effects %>% 
  ggplot() + 
  geom_linerange(
    aes(
      x = type,
      ymin = lower,
      ymax = upper
    ),
    color = matte_indigo
  ) +
  geom_point(
    aes(type, AME),
    size = 3
  ) + 
  geom_hline(
    yintercept = 0,
    linetype = 'dashed',
    color = 'gray55'
  ) + 
  coord_cartesian(ylim = c(-0.025, 0.04))

save_fig(
  plot_ame,
  "reelection_marginal_effect_plots.pdf"
)

# print("estimate reelection probability with vote share")

# patronage_reelection <- patronage_reelection %>%
#   group_by(
#     cod_ibge_6, position, election_year
#   ) %>% 
#   mutate(
#     total_vote = sum(vote)
#   ) %>% 
#   ungroup() %>% 
#   mutate(
#     vote_share = vote/total_vote
#   )

# patronage_reelection_scaled <- patronage_reelection %>%
#   mutate(
#     across(where(is.double), scale_z)
#   )

# f_vote <- update(
#   f_reelection,
#   vote_share ~ prop_hired*mayor_coalition_member + .
# )

# lm_councilor <- lm(
#   f_vote,
#   data = patronage_reelection_scaled %>% 
#     filter(position == 'vereador')
# )

# plot_vote_share <- sjPlot::plot_model(
#   lm_councilor,
#   type = 'pred',
#   terms = c('prop_hired', 'mayor_coalition_member[coalition_member, non_coalition_member]')
# )
# ==============================================================================
# generate objects and visualization for statistical modeling
# ==============================================================================
source(
  here::here("source", "models", "setup.R")
)

# run tasks
modules <- c(
  "model_turnover_saeb",
  "model_turnover_spaece",
  "visual_model_turnover",
  "model_coalition_turnover",
  "visual_regression_discontinuity"
  )

walk(
  modules,
  ~run_module(.)
)

# # ivreg -------------------------------------------------------------------
# fit_iv <- AER::ivreg(
#   grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
#     education_teacher + saeb_wage_teacher +
#     parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
#     toilet + meal + 
#     censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
#     coalition_share + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
#     education_teacher + saeb_wage_teacher + 
#     parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
#     toilet + meal + 
#     censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
#   data = saeb_hierarchical
# )

# fit_iv_2 <- AER::ivreg(
#   grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
#     education_teacher + saeb_wage_teacher +
#     parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
#     toilet + meal + 
#     censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
#     first_year + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
#     education_teacher + saeb_wage_teacher + 
#     parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
#     toilet + meal + 
#     censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
#   data = saeb_hierarchical %>% 
#     mutate(first_year = if_else(year == election_year + 1, 1, 0))
# )

# # patronage and reelection ------------------------------------------------
# patronage_reelection <- patronage_reelection %>% 
#   # filter(cbo_category == 'education') %>% 
#   mutate_at(
#     vars(election_year, state),
#     as.factor
#   ) %>% 
#   mutate(
#     mayor_coalition_member = fct_relevel(mayor_coalition_member, "non_coalition_member")
#   )

# plot_patronage <- patronage_reelection %>%
#   gg_summary(
#     prop_hired,
#     reelected,
#     smooth = T
#   ) +
#   facet_wrap(
#     type ~ .
#   ) +
#   coord_cartesian(ylim = c(0.4, 0.7)) +
#   theme_bw() +
#   theme_clean

# pdf(
#   p_file_here('figs', 'patronage.pdf'),
#   width = 5,
#   height = 3
# )
# plot_patronage
# dev.off()

# # estimation
# f_reelection <- fm(
#   dv = reelected, 
#   predictor = prop_hired,
#   state, election_year,
#   campaign,
#   edu, gender, party_fct,
#   censo_rural, censo_median_wage, censo_log_pop
# )

# fit_reelection_mayor <- glm(
#   f_reelection,
#   family = 'binomial',
#   data = patronage_reelection %>% 
#     filter(position == 'prefeito')
# )

# f_reelection_councilor <- update(f_reelection, . ~ prop_hired*mayor_coalition_member + .)

# fit_reelection_councilor <- glm(
#   f_reelection_councilor,
#   family = 'binomial',
#   data = patronage_reelection %>% 
#     filter(position == 'vereador')
# )

# marginal_mayor <- margins(
#   fit_reelection_mayor,
#   variables = 'prop_hired'
# )

# # marginal_councilor <- margins(
# #   fit_reelection_councilor,
# #   variables = 'prop_hired',
# #   at = list(
# #     mayor_coalition_member = c('coalition_member', 'non_coalition_member')
# #   )
# # )
# # 
# # write_rds(
# #   marginal_councilor,
# #   p_file_here('results', 'marginal_effects_councilor.rds'),
# #   compress = 'gz'
# # )

# marginal_councilor <- read_rds(
#   p_file_here('results', 'marginal_effects_councilor.rds')
# )

# ame <- list(
#   mayor = marginal_mayor,
#   councilor = marginal_councilor
# ) %>% 
#   map_dfr(
#     ~summary(.),
#     .id = 'type'
#   ) %>% 
#   mutate(
#     type = if_else(
#       type == 'mayor', 
#       'mayor', 
#       as.character(mayor_coalition_member)
#     ) %>% 
#       fct_relabel(~str_replace_all(., "_", " ")) %>% 
#       fct_relevel(
#         "mayor"
#       )
#   )

# plot_ame <- ame %>% 
#   ggplot() + 
#   geom_linerange(
#     aes(
#       x = type,
#       ymin = lower,
#       ymax = upper
#     ),
#     color = matte_indigo
#   ) +
#   geom_point(
#     aes(type, AME),
#     size = 3
#   ) + 
#   geom_hline(
#     yintercept = 0,
#     linetype = 'dashed',
#     color = 'gray55'
#   ) + 
#   coord_cartesian(ylim = c(-0.025, 0.04))

# ggsave(
#   p_file_here(
#     'figs', 'patronage_ame.pdf'
#   ),
#   plot_ame
# )

# # vote share
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

# f_vote <- update(
#   f_reelection,
#   vote_share ~ prop_hired*mayor_coalition_member + .
# )

# lm_councilor <- lm(
#   f_vote,
#   data = patronage_reelection %>% 
#     filter(position == 'vereador')
# )

# plot_model(
#   lm_councilor,
#   type = 'pred',
#   terms = c('prop_hired', 'mayor_coalition_member[coalition_member, non_coalition_member]')
# )

# patronage_reelection %>% 
#   filter(elected == 1) %>% 
#   group_by(position) %>% 
#   summarise(
#     vote = sum(vote)
#   )

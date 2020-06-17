# ==============================================================================
# run modules
# ==============================================================================
source(
  here::here("source", "descriptive", "setup.R")
)

# run tasks
modules <- c(
  # "visual_map",
  # "visual_global_edu",
  # "visual_budget",
  # "visual_bureaucracy",
  # "visual_electoral",
  # "visual_turnover",
  "visual_turnover_score"
  # "model_turnover_saeb",
  # "model_turnover_spaece",
  # "visual_model_turnover"
  )

walk(
  modules,
  ~run_module(.)
)

# make sure to create unit tests which sample data randomly (~2%)
# and ensure that the entire source code compiles

# # explaining turnover -----------------------------------------------------

# # covariate balance -------------------------------------------------------
# weight_out <- weightit(
#   turnover_index ~ saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
#     education_teacher + saeb_wage_teacher + year_as_teacher + 
#     parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
#     toilet + meal +
#     censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
#   data = saeb_hierarchical %>% 
#     filter(!is.na(turnover_index)),
#   method = 'ps',
#   missing = 'ind'
# )

# love.plot(
#   weight_out,
#   threshold = .1, 
#   abs = TRUE,
#   var.order = "unadjusted",
#   line = TRUE
# )

# ggsave(
#   filename = p_file_here('figs', 'balance_hlm.pdf')
# )

# weight_out <- weightit(
#   coalition_share ~rais_permanent + rais_size + I(budget_education/censo_pop) + 
#     censo_median_wage+ censo_log_pop+ mayor_edu,
#   data = rais_edu_mun %>% 
#     filter(!is.na(coalition_share)),
#   method = 'ps',
#   missing = 'ind'
# )

# love.plot(
#   weight_out,
#   threshold = .1, 
#   abs = TRUE,
#   var.order = "unadjusted",
#   line = TRUE
# )

# ggsave(
#   filename = p_file_here('figs', 'balance_felm.pdf')
# )

# # rdd ---------------------------------------------------------------------
# formula_rdd <- fm(
#   turnover_index,
#   quotient_margin,
#   cod_ibge_6,
#   mayor_opposition_flip,
#   state,
#   year
# )

# censo_mun_turnover <- censo_school_turnover %>% 
#   filter(
#     n >= 10,
#     between(grade_level, 5, 9),
#     year %in% seq(2005, 2013, 4) # first year of mandate
#   ) %>% 
#   group_by(
#     state,
#     cod_ibge_6,
#     year
#   ) %>% 
#   summarise(
#     turnover_index = weighted.mean(turnover_index, n)
#   ) %>% 
#   ungroup()

# censo_mun_turnover %<>% 
#   add_election %>% 
#   left_join(
#     quotient,
#     by = c("cod_ibge_6", "election_year")
#   )

# censo_turnover_model <- model.frame(
#   formula_rdd,
#   censo_mun_turnover
# ) %>% 
#   join_covariate() %>% 
#   mutate_at(
#     vars(mayor_opposition_flip),
#     ~dplyr::recode(., `0` = 'placebo', `1` = 'mayor_opposition_flip')
#   )

# turnover <- censo_turnover_model %>% 
#   group_split(mayor_opposition_flip) %>% 
#   map(
#     pull,
#     turnover_index
#   )
  
# quotient_margin <- censo_turnover_model %>% 
#   group_split(mayor_opposition_flip) %>%
#   map(
#     pull,
#     quotient_margin
#   )

# covariates <- censo_turnover_model %>% 
#   dplyr::group_split(mayor_opposition_flip) %>% 
#   map(
#     ~select(., state, year)
#   )

# rdd_plots <- pmap(
#   list(
#     turnover = turnover,
#     quotient_margin = quotient_margin,
#     names = names(turnover)
#   ),
#   function(turnover, quotient_margin, names){
#     rdplot(turnover, quotient_margin, title = paste('RDD:', names), y.lim = c(0.4, 0.8))
#   }
# )

# rdd_turnover <- pmap(
#   list(
#     x = turnover,
#     y = quotient_margin,
#     z = covariates
#   ),
#   function(x, y, z){rdrobust(x, y, covs = z)}
# )
  
# lm_turnover <- censo_turnover_model %>% 
#   mutate(
#     treat = if_else(quotient_margin > 0, 1, 0)
#   ) %>% 
#   filter(
#     abs(quotient_margin) <= 0.005
#   ) %>% 
#   group_split(mayor_opposition_flip) %>% 
#   map(
#     ~lm(
#       turnover_index ~ quotient_margin*treat + as.factor(year) + as.factor(state),
#       data = .
#     )
#   )

# censo_turnover_model %>% 
#   ggplot(
#     aes(quotient_margin, turnover_index)
#   ) +
#   stat_summary_bin(
#     fun = 'mean',
#     geom = 'point',
#     binwidth = 0.0005,
#     orientation = 'x',
#     alpha = 0.5
#   ) +
#   geom_vline(
#     xintercept = 0,
#     linetype = 'dashed'
#   ) +
#   facet_wrap(
#     . ~ mayor_opposition_flip,
#     nrow = 2
#   )

# # bottom line: there is no effect of gaining an additional seat for the mayor on patronage.

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

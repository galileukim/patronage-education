# set up ------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  data.table,
  here
  # GGally,
  # lfe,
  # lme4,
  # plm,
  # broom,
  # extrafont,
  # rprojroot,
  # tmap,
  # gridExtra,
  # cowplot,
  # ggdag,
  # scales,
  # kableExtra,
  # RColorBrewer,
  # gghighlight,
  # parallel,
  # foreach,
  # gridExtra,
  # naniar,
  # knitr,
  # gghighlight,
  # egg,
  # WeightIt,
  # cobalt,
  # boot,
  # rdrobust,
  # magrittr,
  # sjPlot
)

knitr::opts_chunk$set(
  eval = T,
  cache = T,
  cache.lazy = F,
  message = F,
  warning = F,
  echo = F,
  fig.height = 3,
  fig.width = 4,
  fig.align = "center"
  # dev = "pdf"
)

# connect to database
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "PostgreSQL Unicode",
  database = "rais",
  UID = "gali",
  PWD = 'gali1789!',
  port = 5432
)

set.seed(1789)

source(
  here("build", "replication", "functions.R")
)

run_module <- partial(
  run_module,
  domain = "replication"
)

read_data <- partial(
  read_data,
  type = "clean"
)

# controls <- c(
#   "rais_permanent", "rais_size", "I(budget_education/censo_pop)", 
#   "censo_median_wage", "censo_log_pop", "mayor_edu", 
#   "mayor_coalition_size", "as.factor(mayor_party)"
# )

# run tasks
modules <- c(
  "visual_map"
  "visual_global_edu",
  "visual_budget",
  "visual_bureaucracy"
  )

walk(
  modules,
  ~run_module(.)
)

# test scores -------------------------------------------------------------
saeb_public <- saeb_dep %>%
  filter(
    dep != "",
    !is.na(grade),
    dep != "particular",
    year >= 2003
  ) %>% 
  mutate(
    year = as.factor(year)
  )

saeb_public %>% 
  ggplot(
    aes(
      year, mean_grade_exam, 
      group = dep, col = dep
    )
  ) + 
  geom_line() +
  geom_point() +
  labs(
    caption = "Average test scores, by administrative level. Includes math and Portuguese test scores."
  ) + 
  facet_wrap(
    grade ~ subject
  )

saeb_descriptive <- saeb_exam_mun %>% 
  join_covariate

saeb_descriptive %>% 
  gg_summary(
    censo_pop, 
    mean_grade_exam
  ) +
  scale_x_log10()

saeb_descriptive %>% 
  gg_summary(
    I(budget_education/censo_pop),
    mean_grade_exam
  )

saeb_exam_school %>% 
  inner_join(
    censo_school_turnover,
    by = c('cod_ibge_6', 'year', 'school_id')
  ) %>% 
  mutate(
    state = str_sub(cod_ibge_6, 1, 2)
  ) %>% 
  group_by(
    state,
    year
  ) %>% 
  mutate(
    turnover_index = turnover_index - mean(turnover_index, na.rm = T),
    mean_grade_exam = mean_grade_exam - mean(mean_grade_exam, na.rm = T)
  ) %>% 
  ungroup() %>% 
  gg_summary(
    turnover_index,
    mean_grade_exam
  )

# turnover ----------------------------------------------------------------
plot_turnover_edu <- rais_edu %>% 
  filter(year >= 2000) %>% 
  group_by(
    rais_category,
    year
  ) %>% 
  summarise_prop(
    "rais_hired"
  ) %>% 
  ggplot(
    aes(
      year,
      prop,
      col = rais_category,
      pch = rais_category
    )
  ) +
  geom_point(
    size = 4
  ) +
  # facet_wrap(
  #   . ~ rais_category,
  #   labeller = labeller(
  #     rais_category = c(principal = "Principal", teacher = "Teacher")
  #   ),
  #   ncol = 2
  # ) +
  mandate_year(
    seq(2001, 2013, 4)
  ) +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  ) +
  labs(
    x = 'Year',
    y = 'Percentage'
  )

ggsave(
  plot_turnover_edu,
  filename = p_file_here('figs', "turnover.pdf")
)

plot_turnover_index <- censo_school_turnover %>% 
  filter(
    n >= 5
  ) %>% 
  gg_histogram(
    aes(turnover_index, y = stat(width*density))
  ) +
  scale_y_continuous(
    labels = percent_format()
  ) +
  labs(
    x = "Distribution of turnover index",
    y = "Count"
  )

ggsave(
  plot_turnover_index,
  filename = p_file_here('figs', "turnover_index.pdf")
)

patronage_category <- patronage %>% 
  mutate(
    year = as.integer(year)
  ) %>% 
  filter(
    cbo_category %in% c('administration', 'education', 'healthcare', 'services')
  )

patronage_category %>% 
  group_by(year, cbo_category) %>%
  summarise(
    prop_hired = sum(prop_hired*total)/sum(total)
  ) %>% 
  ungroup() %>% 
  ggplot(
    aes(year, prop_hired, group = cbo_category, color = cbo_category)
  ) + 
  labs(x = 'year', y = 'proportion of hires') +
  geom_line() +
  geom_point(
    size = 2
  ) +
  geom_vline(
    xintercept = seq(2005, 2013, 4),
    linetype = 'dotted'
  ) +
  scale_x_continuous(
    breaks = seq(2003, 2016, 2)
  ) + 
  ggsave(
    p_file_here(
      'figs', 'hire_by_category'
    )
  )


# address alternative theories
# population
patronage_category %>% 
  gg_summary(
    censo_pop,
    prop_hired
  ) +
  scale_x_log10(
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(10^.x))
  ) +
  facet_wrap(
    cbo_category ~ .
  ) +
  coord_cartesian(
    ylim = c(0, 0.2)
  )

# programmatic party
patronage %>%
  mutate(
    mayor_party = if_else(
      mayor_party %in% c('pt', 'psdb', 'pmdb'),
      mayor_party,
      'other'
    )
  ) %>% 
  ggplot(
    aes(
      mayor_party,
      prop_hired
    )
  ) +
  stat_summary(
    fun.data = 'mean_cl_boot',
    color = matte_indigo
  )

# contract type
# rais_contract <- DBI::dbGetQuery(
#   con,
#   "
#     SELECT cod_ibge_6, year, AVG(CAST(adm = '01' OR adm = '02' AS INTEGER)) AS hired,
#     AVG(CAST(contract_type = 'permanent high' AS INTEGER)) AS permanent
#     FROM (SELECT cod_ibge_6, year, adm, contract_type FROM rais_mun WHERE year > 2002) db
#     GROUP BY cod_ibge_6, year;
#     "
# )

# rural
patronage_category %>% 
  gg_summary(
    censo_rural,
    prop_hired
  ) +
  facet_wrap(
    cbo_category ~ .
  ) +
  coord_cartesian(
    ylim = c(0, 0.3)
  )

# level of economic development
patronage_category %>%
  gg_summary(
    censo_median_wage,
    prop_hired
  ) +
  facet_wrap(
    cbo_category ~ .
  ) +
  coord_cartesian(
    ylim = c(0, 0.3)
  )

# illiteracy
patronage_category %>% 
  gg_summary(
    1 - censo_lit_rate,
    prop_hired
  ) +
  facet_wrap(
    cbo_category ~ .
  ) +
  labs(
    x = 'illiteracy rate',
    y = 'proportion of hires'
  ) 

# electoral ---------------------------------------------------------------
# coalitions
election %>% 
  gg_histogram(
    aes(coalition_share*100),
    bins = 20
  ) +
  labs(
    x = "Share of seats (percentage)",
    y = 'Count'
  )

ggsave(
  filename = p_file_here('figs', "hist_coalition.pdf")
)

# hierarchical model ------------------------------------------------------
mods <- list()

turnover_saeb <- "saeb_teacher_work_school + saeb_principal_experience"
formulae_hlm <- c(
  as.formula(
    grade_exam ~ turnover_index*grade + I(turnover_index^2)*grade + (1|year) + (1|state)
  ),
  as.formula(
    grade_exam ~ turnover_index*grade + (1|year) + (1|state) +
      saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
      education_teacher + saeb_wage_teacher + year_as_teacher + 
      parent_attend_college_student, parents_school_meeting_student + fridge_student + failed_school_year_student +
      toilet + meal + I(budget_education/censo_pop) +
      censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate
  ),
  as.formula(
    grade_exam ~ saeb_teacher_work_school + saeb_principal_experience + (1|year) + (1|state)
  ), 
  as.formula(
    grade_exam ~ saeb_teacher_work_school + saeb_principal_experience  + (1|year) + (1|state) +
    saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + year_as_teacher + 
    parent_attend_college_student, parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal +  I(budget_education/censo_pop) + censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate
  )
)

fit_lmer <- map(
  formulae_hlm,
  ~lmer(
    formula = .x,
    data = saeb_hierarchical
  )
)

sink(p_file_here('results', 'hlm_result.tex'))
mstar(
  fit_lmer,
  keep = c('turnover_index', 'saeb_principal_experience', 'saeb_teacher_work_school'),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 2))),
  covariate.labels = c('Turnover index', 'Turnover index $\\times$ Grade 9', 
                       'Teacher experience (2 years)', 'Teacher experience (10 years)',
                       'School principal experience (2 years)', 'School principal experience (10 years)'),
  dep.var.caption = 'Student learning',
  dep.var.labels = 'SAEB average test scores'
)
sink()

formulae_spaece <- c(
  as.formula(
    spaece_mean ~ turnover_index*grade + as.factor(year)
  ),
  as.formula(
    spaece_mean ~ turnover_index*grade + as.factor(year) + toilet + meal +
    censo_rural + censo_log_pop + censo_lit_rate
  )
)

fit_spaece <- map(
  formulae_spaece,
  ~lm(
    formula = .,
    data = censo_turnover_score %>% 
      mutate_if(is.double, rescale) %>% 
      rename(grade = grade_level) %>% 
      filter(grade != 2)
  )
)

sink(p_file_here('results', 'spaece_result.tex'))
mstar(
  fit_spaece,
  keep = c('turnover_index'),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 1))),
  covariate.labels = c('Turnover index', 'Turnover index $\\times$ Grade 9'),
  dep.var.caption = 'Student learning',
  dep.var.labels = 'SPAECE average test scores'
)
sink()

sink(p_file_here('results', 'student_learning.tex'))
mstar(
  c(fit_lmer, fit_spaece),
  keep = c('turnover_index', 'saeb_principal_experience', 'saeb_teacher_work_school'),
  add.lines = list(c("Controls", rep(c("\\_", "\\checkmark"), 3))),
  dep.var.labels.include = F,
  covariate.labels = c(
    'Turnover index', 'Turnover index $\\times$ Grade 9',
    'Teacher experience (2 years)', 'Teacher experience (10 years)',
    'School principal experience (2 years)', 'School principal experience (10 years)'
  ),
  dep.var.caption = 'Student learning',
  column.labels = c('SAEB test score', 'SPAECE test score'),
  column.separate = c(4, 2),
  model.names = FALSE,
  keep.stat = 'n'
)
sink()

# visualization
mods <- c(
  fit_lmer[c(2,4)],
  fit_spaece[2]
)
  
names(mods) <- c('model turnover index', 'model teacher and principal', 'model spaece')

estimate_hlm <- map2_dfr(
  mods,
  names(mods),
  ~tidyfit(
    .x
  ) %>% 
    mutate(
      type = .y
    )
)

estimate_hlm <- map2_dfr(
  mods,
  names(mods),
  ~tidyfit(
    .x
  ) %>% 
    mutate(
      type = .y
    )
)

plot_hlm <- estimate_hlm %>% 
  ggplot(
    aes(
      y = term,
      x = estimate,
      color = type,
      group = type
    )
  ) +
  geom_point(
    position = ggstance::position_dodgev(height=0.3),
    size = 2
  ) +
  geom_errorbar_tidy +
  theme(
    legend.position = 'bottom',
    legend.title = element_blank()
  ) +
  geom_vline(xintercept = 0) +
  scale_y_discrete(
    limits = rev(c(
      "turnover_index",
      "saeb_principal_experienceless than 2 years",
      "saeb_teacher_work_schoolless than 2",
      "censo_rural",
      "censo_log_pop",
      "education_teacherhigher education",
      "saeb_wage_teachermore than 3000"
    )),
    labels = c(
      "turnover_index" = "Teacher turnover index",
      "saeb_principal_experienceless than 2 years" = "Principal: less than 2 years of exp.",
      "saeb_teacher_work_schoolless than 2" = "Teacher: less than 2 years in school",
      "censo_rural" = "Rural population",
      "censo_log_pop" = "Logged population",
      "education_teacherhigher education" = "Teacher: higher education",
      "saeb_wage_teachermore than 3000" = "Teacher: wage more than $750"
    )
  ) +
  coord_cartesian(
    xlim = c(-.1,.1)
  )

ggsave(
  plot_hlm,
  filename = p_file_here('figs', "turnover_fit.pdf")
)
# 
# fit_hlm <- invoke_map(
#   list(
#     lm,
#     lmer
#   ),
#   list(
#     list(
#       formula = update.formula(
#         formula_hlm,
#         . ~ percent_entry + percent_exit + as.factor(uf) + as.factor(year) + .
#       ),
#     data = saeb_hierarchical
#     ),
#     list(
#       formula = update.formula(
#         formula_hlm,
#         . ~ percent_entry + percent_exit + (1|uf) + (1|year) + .
#     ),
#     data = saeb_hierarchical
#     )
#   )
# )

# turnover spaece ---------------------------------------------------------
plot_spaece <- censo_turnover_score %>% 
  filter(
    n >= 5
  ) %>% 
  mutate(
    grade_level = as.factor(grade_level)
  ) %>% 
  ggplot(
    aes(
      turnover_index,
      spaece_mean,
      group = as.factor(grade_level),
      col = as.factor(grade_level)
    )
  ) +
  geom_smooth(
    method = lm, formula = y ~ splines::bs(x, 3),
    se = F
  ) +
  annotate(
    "text",
    label = "2nd grade",
    size = 2,
    col = "grey65",
    0.45, 165
  ) +
  annotate(
    "text",
    label = "5th grade",
    size = 2,
    col = "grey65",
    0.6, 200
  ) +
  annotate(
    "text",
    label = "9th grade",
    size = 2,
    col = "grey65",
    0.7, 225
  ) +
  labs(
    x = "Teacher turnover (index)",
    y = "Test scores (SPAECE)"
  ) +
  theme(
    legend.position = "none"
  )

ggsave(
  plot_spaece,
  filename = p_file_here('figs', "turnover_spaece.pdf")
)

# explaining turnover -----------------------------------------------------
# show that irrespective of level of economic development, the same dynamic is present
# turnover index on coalition
saeb_turnover <- censo_school_turnover %>% 
  join_covariate() %>% 
  left_join(
    censo_school %>% 
      mutate(
        school_id = as.integer(school_id)
      ) %>% 
      select(-state, -location),
    by = c('cod_ibge_6', 'year', 'school_id')
  )

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

# accountability ----------------------------------------------------------
fit_acc <- list()

fit_acc$lm <- lm(
  vote_share ~ delta_mean_grade_exam + edu_desc + gender + occupation + 
    as.factor(party) + vote_lag + delta_mean_grade_exam + 
    censo_rural + censo_log_pop + censo_student_age,
  data = accountability
)

fit_acc$felm <- fit_felm(
  fit_acc,
  "vote_share",
  predictor = "delta_mean_grade_exam",
  control = c(
    "edu_desc", "gender", "occupation", "as.factor(party)", "vote_lag", 
    "delta_mean_grade_exam", "censo_rural", "censo_log_pop", "censo_student_age"
  ),
  data = accountability,
  cluster = c("state + election_year")
)

plot_accountability <- fit_acc %>% 
  map2_df(
    names(.),
    ~tidy(.x, conf.int = T) %>% 
      filter(
        str_detect(
          term,
          "delta|censo_log_pop|censo_rural|gender|vote_lag"
        )
      ) %>% 
      mutate(
        name = .y
      )
  )

plot_accountability <- ggplot(
  data = plot_accountability,
  mapping = aes(
    y = term, 
    x = estimate,
    color = name,
    group = name
  ),
  conf.int = T
) +
  geom_point(
    position = ggstance::position_dodgev(height=0.3)
  ) +
  geom_errorbar_tidy

ggsave(
  plot_accountability,
  filename = p_file_here('figs', "accountability_fit.pdf")
)

# covariate balance -------------------------------------------------------
weight_out <- weightit(
  turnover_index ~ saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + year_as_teacher + 
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal +
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
  data = saeb_hierarchical %>% 
    filter(!is.na(turnover_index)),
  method = 'ps',
  missing = 'ind'
)

love.plot(
  weight_out,
  threshold = .1, 
  abs = TRUE,
  var.order = "unadjusted",
  line = TRUE
)

ggsave(
  filename = p_file_here('figs', 'balance_hlm.pdf')
)

weight_out <- weightit(
  coalition_share ~rais_permanent + rais_size + I(budget_education/censo_pop) + 
    censo_median_wage+ censo_log_pop+ mayor_edu,
  data = rais_edu_mun %>% 
    filter(!is.na(coalition_share)),
  method = 'ps',
  missing = 'ind'
)

love.plot(
  weight_out,
  threshold = .1, 
  abs = TRUE,
  var.order = "unadjusted",
  line = TRUE
)

ggsave(
  filename = p_file_here('figs', 'balance_felm.pdf')
)

# rdd ---------------------------------------------------------------------
formula_rdd <- fm(
  turnover_index,
  quotient_margin,
  cod_ibge_6,
  mayor_opposition_flip,
  state,
  year
)

censo_mun_turnover <- censo_school_turnover %>% 
  filter(
    n >= 10,
    between(grade_level, 5, 9),
    year %in% seq(2005, 2013, 4) # first year of mandate
  ) %>% 
  group_by(
    state,
    cod_ibge_6,
    year
  ) %>% 
  summarise(
    turnover_index = weighted.mean(turnover_index, n)
  ) %>% 
  ungroup()

censo_mun_turnover %<>% 
  add_election %>% 
  left_join(
    quotient,
    by = c("cod_ibge_6", "election_year")
  )

censo_turnover_model <- model.frame(
  formula_rdd,
  censo_mun_turnover
) %>% 
  join_covariate() %>% 
  mutate_at(
    vars(mayor_opposition_flip),
    ~dplyr::recode(., `0` = 'placebo', `1` = 'mayor_opposition_flip')
  )

turnover <- censo_turnover_model %>% 
  group_split(mayor_opposition_flip) %>% 
  map(
    pull,
    turnover_index
  )
  
quotient_margin <- censo_turnover_model %>% 
  group_split(mayor_opposition_flip) %>%
  map(
    pull,
    quotient_margin
  )

covariates <- censo_turnover_model %>% 
  dplyr::group_split(mayor_opposition_flip) %>% 
  map(
    ~select(., state, year)
  )

rdd_plots <- pmap(
  list(
    turnover = turnover,
    quotient_margin = quotient_margin,
    names = names(turnover)
  ),
  function(turnover, quotient_margin, names){
    rdplot(turnover, quotient_margin, title = paste('RDD:', names), y.lim = c(0.4, 0.8))
  }
)

rdd_turnover <- pmap(
  list(
    x = turnover,
    y = quotient_margin,
    z = covariates
  ),
  function(x, y, z){rdrobust(x, y, covs = z)}
)
  
lm_turnover <- censo_turnover_model %>% 
  mutate(
    treat = if_else(quotient_margin > 0, 1, 0)
  ) %>% 
  filter(
    abs(quotient_margin) <= 0.005
  ) %>% 
  group_split(mayor_opposition_flip) %>% 
  map(
    ~lm(
      turnover_index ~ quotient_margin*treat + as.factor(year) + as.factor(state),
      data = .
    )
  )

censo_turnover_model %>% 
  ggplot(
    aes(quotient_margin, turnover_index)
  ) +
  stat_summary_bin(
    fun = 'mean',
    geom = 'point',
    binwidth = 0.0005,
    orientation = 'x',
    alpha = 0.5
  ) +
  geom_vline(
    xintercept = 0,
    linetype = 'dashed'
  ) +
  facet_wrap(
    . ~ mayor_opposition_flip,
    nrow = 2
  )

# bottom line: there is no effect of gaining an additional seat for the mayor on patronage.

# ivreg -------------------------------------------------------------------
fit_iv <- AER::ivreg(
  grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher +
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
    coalition_share + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + 
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
  data = saeb_hierarchical
)

fit_iv_2 <- AER::ivreg(
  grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher +
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
    first_year + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + 
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
  data = saeb_hierarchical %>% 
    mutate(first_year = if_else(year == election_year + 1, 1, 0))
)

# patronage and reelection ------------------------------------------------
patronage_reelection <- patronage_reelection %>% 
  # filter(cbo_category == 'education') %>% 
  mutate_at(
    vars(election_year, state),
    as.factor
  ) %>% 
  mutate(
    mayor_coalition_member = fct_relevel(mayor_coalition_member, "non_coalition_member")
  )

plot_patronage <- patronage_reelection %>%
  gg_summary(
    prop_hired,
    reelected,
    smooth = T
  ) +
  facet_wrap(
    type ~ .
  ) +
  coord_cartesian(ylim = c(0.4, 0.7)) +
  theme_bw() +
  theme_clean

pdf(
  p_file_here('figs', 'patronage.pdf'),
  width = 5,
  height = 3
)
plot_patronage
dev.off()

# estimation
f_reelection <- fm(
  dv = reelected, 
  predictor = prop_hired,
  state, election_year,
  campaign,
  edu, gender, party_fct,
  censo_rural, censo_median_wage, censo_log_pop
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

# marginal_councilor <- margins(
#   fit_reelection_councilor,
#   variables = 'prop_hired',
#   at = list(
#     mayor_coalition_member = c('coalition_member', 'non_coalition_member')
#   )
# )
# 
# write_rds(
#   marginal_councilor,
#   p_file_here('results', 'marginal_effects_councilor.rds'),
#   compress = 'gz'
# )

marginal_councilor <- read_rds(
  p_file_here('results', 'marginal_effects_councilor.rds')
)

ame <- list(
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

plot_ame <- ame %>% 
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

ggsave(
  p_file_here(
    'figs', 'patronage_ame.pdf'
  ),
  plot_ame
)

# vote share
patronage_reelection <- patronage_reelection %>%
  group_by(
    cod_ibge_6, position, election_year
  ) %>% 
  mutate(
    total_vote = sum(vote)
  ) %>% 
  ungroup() %>% 
  mutate(
    vote_share = vote/total_vote
  )

f_vote <- update(
  f_reelection,
  vote_share ~ prop_hired*mayor_coalition_member + .
)

lm_councilor <- lm(
  f_vote,
  data = patronage_reelection %>% 
    filter(position == 'vereador')
)

plot_model(
  lm_councilor,
  type = 'pred',
  terms = c('prop_hired', 'mayor_coalition_member[coalition_member, non_coalition_member]')
)

patronage_reelection %>% 
  filter(elected == 1) %>% 
  group_by(position) %>% 
  summarise(
    vote = sum(vote)
  )

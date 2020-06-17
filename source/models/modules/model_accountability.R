# ==============================================================================
# pre-process data
# ==============================================================================
saeb_exam_mun <- read_data(
    "saeb",
    "saeb_exam_mun.rds"
)

mayor <- read_data(
    "tse",
    "mayor.rds"
)

saeb_election <- saeb_exam_mun %>% 
  group_by(
    cod_ibge_6,
    election_year
  ) %>% 
  summarise(
    mean_grade_exam = mean(mean_grade_exam, na.rm = T)
  ) %>%
  group_by(
    cod_ibge_6
  ) %>% 
  mutate(
    mean_grade_exam_lag_1 = dplyr::lag(mean_grade_exam, 1, order_by = election_year),
    mean_grade_exam_lag_2 = dplyr::lag(mean_grade_exam, 2, order_by = election_year),
    delta_mean_grade_exam = 100*(mean_grade_exam_lag_1 - mean_grade_exam_lag_2)/mean_grade_exam_lag_2
  ) %>% 
  ungroup()

mayor %>% check_if_id_unique(cpf_candidate, election_year)

mayor <- mayor %>% 
arrange(
    desc(election_year)
  ) %>% 
  group_by(
    cpf_candidate
  ) %>% 
  mutate(
    vote_lag = dplyr::lag(vote, order_by = election_year)
  ) %>% 
  ungroup() 

accountability %<>% 
  group_by(
    cod_ibge_6,
    election_year
  ) %>%
  mutate(
    total_vote = sum(vote, na.rm = T),
    n_candidate = n()
  ) %>% 
  ungroup() %>% 
  filter(
    incumbent == 1,
    n_candidate > 1 # drop elections with only 1 candidate
  ) %>% 
  left_join(
    saeb_election,
    by = c("cod_ibge_6", "election_year")
  ) %>% 
  mutate(
    state = str_sub(cod_ibge_6, 1, 2),
    election_year = as.factor(election_year),
    gender = if_else(gender == 2, "male", "female"),
    elected = if_else(outcome == "eleito", 1, 0),
    vote_share = vote/total_vote*100
  ) %>% 
  left_join(censo)

# ==============================================================================
# accountability
# ==============================================================================
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
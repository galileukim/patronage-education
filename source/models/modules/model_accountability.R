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

censo_demographic <- read_data(
  "censo_br",
  "censo_2000.rds"
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

accountability <- mayor %>% 
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
  left_join(censo_demographic)

# ==============================================================================
# accountability
# ==============================================================================
mayor_covariates_accountability <- c(
  "gender",
  "edu"
  )

mun_covariates_accountability <- mun_covariates %>%
  str_subset("education_capita", negate = T)

controls <- c(
    mayor_covariates_accountability,
    mun_covariates_accountability
  )

formulae_accountability <- formulate_models(
  "vote_share",
  "delta_mean_grade_exam",
  fe = NULL,
  controls
) %>%
  map(
    ~add_felm(., fe = "state + election_year", cluster = "cod_ibge_6" )
  )

fit_accountability <- map(
  formulae_accountability,
  ~ lfe::felm(
    .,
    data = accountability
    )
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
  filename = p_file_here('plots', "accountability_fit.pdf")
)
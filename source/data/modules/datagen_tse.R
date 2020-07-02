chamber <- read_data(
  "raw",
  "tse",
  "election_chamber.csv"
)

chamber <- chamber %>%
  filter(
    election_year %in% seq(2000, 2016, 4)
  ) %>%
  group_by(cod_ibge_6) %>%
  mutate(
    mayor_party_lag = dplyr::lag(mayor_party, order_by = election_year),
    mayor_coalition_lag = dplyr::lag(mayor_coalition, order_by = election_year)
  ) %>%
  ungroup() %>%
  mutate(
    mayor_party = if_else(
      mayor_party %in% c('pt', 'psdb', 'pmdb'),
      mayor_party,
      'other'
    ),
    party_turnover = if_else(mayor_party != mayor_party_lag, 1, 0)
  )

chamber %>%
  write_data(
    "tse",
    "election.rds"
  )

quotient <- read_data(
  "raw",
  "tse",
  "election_coalition.csv"
)

quotient %>%
  write_data(
    "tse",
    "electoral_quotient.rds"
  )

election <- read_data(
  "raw",
  "tse",
  "election.csv.gz"
)

mayor <- election %>%
  filter(
    election_year %in% seq(2000, 2016, 4),
    position == "prefeito"
  )

# eliminate municipalities with a second round
mayor %<>%
  group_by(
    cod_ibge_6,
    election_year
  ) %>% 
  filter(
    !any(round == 2)
  ) %>% 
  ungroup()

vereador <- election %>%
  filter(
    election_year %in% seq(2000, 2016, 4),
    position != "prefeito"
  ) %>%
  mutate(
    age = election_year - as.numeric(birthyear)
  )

# write-out
mayor %>%
  write_data(
    "tse",
    "mayor.rds"
  )

vereador %>%
  write_data(
    "tse",
    "vereador.rds"
  )
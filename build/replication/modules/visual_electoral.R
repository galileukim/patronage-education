election <- read_data(
    "tse",
    "election.rds"
)

censo <- read_data(
    "censo_br",
    "censo_2000.rds"
)

rais <- read_data(
    "rais",
    "rais_mun_edu.rds"
)

# 1) visualize underlying distribution of coalition shares
hist_coalition <- election %>%
    gg_histogram(
        aes(coalition_share * 100),
        bins = 20
    ) +
    labs(
        x = "Share of seats (percentage)",
        y = "Count"
    )

save_fig(
    hist_coalition,
    "hist_coalition.pdf"
)

# 2) add additional context on how these are structured (size, composition)
# address the problem of confounding even if partially
rais_mun <- rais %>%
    group_by(
        cod_ibge_6,
        election_year
    ) %>%
    summarise(
        across(
            c(rais_size, rais_higher_edu, rais_permanent, rais_wage),
            ~mean(., na.rm = T)
        ),
        .groups = "drop"
    )

election_covariate <- election %>%
    left_join(
        censo,
        by = "cod_ibge_6"
    ) %>%
    left_join(
        
    )
    select(
        coalition_share,
        chamber_size,
        censo_pop,
        censo_rural,
        censo_median_wage
    ) %>%
    na.omit()

cov_balance <- cobalt::bal.tab(
    coalition_share ~ censo_pop + chamber_size,
    data = election_covariate,
    thresholds = c(m = 0.1)
)

cobalt::love.plot(
    cov_balance,
    threshold = 0.1
)
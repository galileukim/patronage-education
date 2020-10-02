mayor <- read_data(
    "tse",
    "mayor.rds"
)

city_councilor <- read_data(
    "tse",
    "vereador.rds"
)

election <- bind_rows(
    list(mayor = mayor, city_councilor = city_councilor),
    .id = "dataset"
)

# a graph that whos how different are the sizes of constituencies
# do it with percentage of the electorate
# why are there so many 100 percent vote mayors?
election %>%
    group_by(cod_ibge_6, election_year, position) %>%
    mutate(
        total_vote = sum(vote),
        percent_vote = vote/total_vote*100
    ) %>%
    ungroup() %>%
    filter(elected == 1) %>%
    ggplot() + 
    geom_boxplot(
        aes(election_year, percent_vote)
    ) +
    facet_wrap(position ~ .)
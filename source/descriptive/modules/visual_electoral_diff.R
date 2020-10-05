library(scales)

position <- sprintf(
    "%s.rds",
    c("mayor", "vereador")
)

election <- map_dfr(
    position,
    ~ read_data(dir = "tse", file = .)
)

election_vote_share <- election %>%
    group_by(
        cod_ibge_6,
        election_year,
        position
    ) %>%
    mutate(
        total_vote = sum(vote)
    ) %>%
    ungroup() %>%
    filter(elected == 1) %>%
    group_by(
        cod_ibge_6,
        election_year,
        position
    ) %>%
    summarise(
        percentage_vote = vote/total_vote*100
    ) %>%
    ungroup()

election_vote_share %>%
    ggplot(
        aes(
            x = percentage_vote,
            y = stat(width * density),
            fill = position,
            color = position
        )
    ) +
    geom_histogram(
        binwidth = 1,
        # aes(percentage_vote, y = ..density..),
        size = 1
    ) +
    scale_y_continuous(labels = percent, name = "percent") +
    annotate(
        "text",
        x = c(15, 65),
        y = c(0.18, 0.05),
        label = c("city councilors", "mayors")
    ) +
    theme(
        legend.position = "none"
    )
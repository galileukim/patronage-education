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

plot_vote_share <- election_vote_share %>%
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
    scale_y_continuous(labels = percent) +
    annotate(
        "text",
        x = c(17, 65),
        y = c(0.15, 0.05),
        label = c("city councilors", "mayors")
    ) +
    theme(
        legend.position = "none"
    ) +
    labs(
        x = "Vote share",
        y = "Frequency (percent)"
    )

save_fig(
    plot_vote_share,
    file = "plot_vote_share_by_position.pdf"
)
councilor <- read_data(
    "tse",
    "vereador.rds"
)

plot_chamber_size <- councilor %>%
    group_by(election_year) %>%
    summarise(
        total_councilors = sum(elected, na.rm = T)
    ) %>%
    ggplot() + 
    geom_col(
        aes(
            election_year, total_councilors,
            col = matte_indigo,
            fill = matte_indigo
        )
    )

save_fig(
    plot_chamber_size,
    "plot_chambere_size.pdf"
)
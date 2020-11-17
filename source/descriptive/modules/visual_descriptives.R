# produce descriptive statistics on both teachers and school principals
# start with simple visualizations
rais_edu_year <- read_data(
    "rais",
    "rais_year_edu.rds"
)

# ---------------------------------------------------------------------------- #
# aux funs
ggplot_rais_by_year <- function(data, var) {
    ggplot(
        data,
        aes_string("year", var, color = "rais_category")
    ) +
        geom_point(size = 3) +
        geom_line() +
        scale_x_continuous(
            breaks = seq(2005, 2013, 4),
            labels = seq(2005, 2013, 4)
        )
}

plot_descriptives_grobs <- map(
    c("rais_time", "rais_higher_edu", "rais_wage", "rais_permanent"),
    ~ ggplot_rais_by_year(rais_edu_year, .) +
        scale_colour_discrete(palette = "Set2")
)

plot_descriptives <- ggpubr::ggarrange(
    plotlist = plot_descriptives_grobs,
    # ncol = 2,
    common.legend = TRUE
)
# ==============================================================================
# visualize the causal effect of an additional coalition seat on the likelihood
# of additional hires in the municipality
# ==============================================================================
print("load data")
source(here::here("source", "models", "setup.R"))

censo_school_turnover <- read_data(
    "censo_escolar",
    "censo_school_turnover.rds"
)

electoral_quotient <- read_data(
    "tse",
    "electoral_quotient.rds"
)

print("join datasets and prep for rdd")

censo_mun_turnover <- censo_school_turnover %>%
    filter(
        # n >= 10,
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
        electoral_quotient,
        by = c("cod_ibge_6", "election_year")
    )

formula_rdd <- reformulate(
    c(
        "quotient_margin",
        "cod_ibge_6",
        "mayor_opposition_flip",
        "state",
        "year"
    ),
    "turnover_index"
)

censo_turnover_model <- model.frame(
    formula_rdd,
    censo_mun_turnover
) %>%
    join_covariate() %>%
    mutate_at(
        vars(mayor_opposition_flip),
        ~ dplyr::recode(., `0` = "placebo", `1` = "legislative_seat_flip")
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
        ~ select(., state, year)
    )
print("produce rdd plots")
rdd_plots <- pmap(
    list(
        turnover = turnover,
        quotient_margin = quotient_margin,
        names = names(turnover),
        z = covariates
    ),
    function(turnover, quotient_margin, names, z) {
        rdrobust::rdplot(
            turnover,
            quotient_margin,
            y.lim = c(0.4, 0.8),
            title = "",
            covs = z,
            y.label = "teacher turnover",
            x.label = "electoral quotient margin"
        )


        ggsave(
            here("models", "figs", paste0("visual_rdd_", names, ".pdf"))
        )
    }
)

print("write-out estimates from rdrobust")
rdd_turnover <- pmap(
    list(
        x = turnover,
        y = quotient_margin,
        z = covariates
    ),
    function(x, y, z) {
        rdrobust::rdrobust(x, y, covs = z)
    }
)

rdd_turnover %>%
    write_rds(
        here("models", "output", "rdd_output.rds")
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
        ~ lm(
            turnover_index ~ quotient_margin * treat + as.factor(year) + as.factor(state),
            data = .
        )
    )

print("write-out hand coded visualization of discontinuity")
visual_plot_rdd <- censo_turnover_model %>%
    mutate(
        mayor_opposition_flip = recode(
            mayor_opposition_flip,
            "placebo" = "placebo",
            "mayor_opposition_flip" = "legislative seat flips to mayor"
         )
    ) %>%
    ggplot(
        aes(quotient_margin, turnover_index)
    ) +
    stat_summary_bin(
        fun = "mean",
        geom = "point",
        binwidth = 0.0005,
        orientation = "x",
        alpha = 0.5
    ) +
    geom_vline(
        xintercept = 0,
        linetype = "dashed"
    ) +
    facet_wrap(
        . ~ mayor_opposition_flip,
        nrow = 2
    )

save_fig(
    visual_plot_rdd,
    "visual_legislative_seat_flip_rdd.pdf"
)

# bottom line: there is no effect of gaining an additional seat for the mayor on patronage.
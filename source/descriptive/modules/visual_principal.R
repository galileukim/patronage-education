# produce descriptive statistics on principals
# education level, work experience
library(scales)

saeb_principal <- read_data(
    "saeb",
    "saeb_principal.rds"
)

# ---------------------------------------------------------------------------- #
plot_histogram <- function(data, var){
    data %>%
    ggplot(
        aes(
            x = {{var}},
            group = year,
            fill = as.factor(year)
        )
    ) +
    stat_count(
        width = 0.5,
        position = "dodge"
    ) +
    theme(legend.title = element_blank())
}

# ---------------------------------------------------------------------------- #
saeb_principal <- saeb_principal %>%
    filter(year >= 2007)

plot_experience <- saeb_principal %>%
    plot_histogram(
        saeb_principal_experience
    )

plot_appointment <- saeb_principal %>%
    filter(!is.na(saeb_principal_appointment)) %>%
    plot_histogram(
        saeb_principal_appointment
    )

plot_education <- saeb_principal %>%
    plot_histogram(
        saeb_principal_education
    )

plot_age <- saeb_principal %>%
    plot_histogram(
        saeb_principal_age
    )
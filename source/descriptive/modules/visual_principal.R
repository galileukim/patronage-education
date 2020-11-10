# produce descriptive statistics on principals
# education level, work experience
library(scales)

saeb_principal <- read_data(
    "saeb",
    "saeb_principal.rds"
)

saeb_principal <- saeb_principal %>%
    filter(year >= 2007)

saeb_principal %>%
    ggplot(
        aes(
            x = saeb_principal_experience,
            group = year,
            fill = as.factor(year)
        )
    ) +
    stat_count(
        width = 0.5,
        position = "dodge"
    ) +
    theme(legend.title = element_blank())

saeb_principal %>%
    filter(!is.na(saeb_principal_appointment)) %>%
    ggplot(
        aes(
            x = saeb_principal_appointment,
            group = year,
            fill = as.factor(year)
        )
    ) +
    stat_count(
        width = 0.5,
        position = "dodge"
    ) +
    theme(legend.title = element_blank())

# produce descriptive statistics on principals
# education level, work experience
library(scales)

saeb_principal <- read_data(
    "saeb",
    "saeb_principal.rds"
)

# ---------------------------------------------------------------------------- #
plot_histogram <- function(data, var) {
    plot <- data %>%
        ggplot(
            aes(
                x = {{ var }},
                group = year,
                fill = as.factor(year)
            )
        ) +
        stat_count(
            width = 0.5,
            position = "dodge"
        ) +
        labs(x = "", y = "", fill = "Year")
        }

# ---------------------------------------------------------------------------- #
saeb_principal <- saeb_principal %>%
    filter(year >= 2007)

plot_experience <- saeb_principal %>%
    plot_histogram(
        saeb_principal_experience
    ) +
    ggtitle("Years of Experience")

plot_appointment <- saeb_principal %>%
    filter(!is.na(saeb_principal_appointment)) %>%
    plot_histogram(
        saeb_principal_appointment
    ) +
    ggtitle("Appointment Process")

plot_education <- saeb_principal %>%
    plot_histogram(
        saeb_principal_education
    ) +
    ggtitle("Education")

plot_age <- saeb_principal %>%
    plot_histogram(
        saeb_principal_age
    ) +
    ggtitle("Age")

plot_principal_descriptives <- ggpubr::ggarrange(    
        plot_experience,
        plot_appointment,
        plot_education,
        plot_age,
        common.legend = TRUE,
        legend = "bottom"
)

save_fig(
    plot_principal_descriptives,
    "plot_principal_descriptive.pdf"
)
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
    scale_x_discrete(
        labels = c(
            "2 to 10" = "2 to 10\nyears",
            "less than 2 years" = "less than\n 2 years",
            "more than 10" = "more than\n10 years"
        )
    ) +
    ggtitle("Years of experience") +
    theme(
        plot.title = element_text(size = 10, face = "bold"), 
        legend.key.size = unit(1,"line")
        )

plot_appointment <- saeb_principal %>%
    filter(!is.na(saeb_principal_appointment)) %>%
    mutate(
        saeb_principal_appointment = if_else(
            str_detect(saeb_principal_appointment, "selection"),
            "selection process", saeb_principal_appointment
        )
    ) %>%
    plot_histogram(
        saeb_principal_appointment
    ) +
    scale_x_discrete(
        labels = c(
            "political appointment" = "political\nappointment",
            "selection process" = "selection\nprocess"
        )
    ) +
    ggtitle("Appointment process") +
    theme(
        plot.title = element_text(size = 10, face = "bold"), 
        legend.key.size = unit(1,"line")
        )


plot_education <- saeb_principal %>%
    filter(
        saeb_principal_education %in% c("higher education", "high school")
    ) %>%
    plot_histogram(
        saeb_principal_education
    ) +
    ggtitle("Education") +
    theme(
        plot.title = element_text(size = 10, face = "bold"), 
        legend.key.size = unit(1,"line")
        )


plot_age <- saeb_principal %>%
    plot_histogram(
        saeb_principal_age
    ) +
    ggtitle("Age") +
    theme(
        plot.title = element_text(size = 10, face = "bold"), 
        legend.key.size = unit(1,"line")
        )


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
    "plot_principal_descriptive.pdf",
    width = 6,
    height = 4
)
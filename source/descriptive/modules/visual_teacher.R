# produce descriptive statistics on teachers
# number of teachers per school, education level, work experience
library(scales)

censo_teacher <- read_data(
    "censo_escolar",
    "censo_teacher_dep.rds"
)

censo_school <- read_data(
    "censo_escolar",
    "censo_school.rds"
)

censo_teacher_latest <- censo_teacher %>%
    filter(dep == "municipal" & year == max(year))

# number of teachers per school for latest available data
plot_n_teacher_per_school <- censo_teacher_latest %>%
    gg_histogram(
        aes(n_teacher),
        binwidth = 1
    ) +
    coord_cartesian(
        xlim = c(0, 35)
    ) +
    labs(
        x = "Number of teachers",
        y = "Frequency (count)"
    )

save_fig(
    plot_n_teacher_per_school,
    "plot_number_teachers_per_school.pdf"
)

# proportion of higher education teachers
plot_prop_higher_edu_teachers <- censo_teacher_latest %>%
    gg_histogram(
        aes(prop_higher_edu),
        binwidth = 0.05
    )

save_fig(
    plot_n_teacher_per_school,
    "plot_number_teachers_per_school.pdf"
)

# age distribution
censo_teacher_latest %>%
    gg_histogram(
        aes(mean_age),
        bindwidth = 1
    )

# create table of descriptive statistics
censo_teacher_latest %>%
    select(
        n_teacher,
        n_classes,
        prop_higher_edu,
        mean_age,
        prop_female,
        mean_n_class_per_teacher
    ) %>%
    as.data.frame() %>%
    stargazer::stargazer(
        covariate.labels = c(
            "Number of teachers per school",
            "Number of classes per school",
            "Proportion of teachers w/ higher education",
            "Average age",
            "Proportion of women",
            "Average number of classes per teacher"
        ),
        float = FALSE,
        omit.summary.stat = c("p25", "p75", "n"),
        type = "latex",
        out = here("figures/results/descriptive_teacher.tex")
    )
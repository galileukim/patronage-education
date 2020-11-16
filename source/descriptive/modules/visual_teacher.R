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
    select(-cod_ibge_6, -year, -school_id) %>%
    as.data.frame() %>%
    stargazer(
        covariate.labels = c(
            "Number of teachers per school",
            "Number of classes per school",
            "Proportion of teachers w/ higher education",
            "Average age",
            "Average number of classes per teacher"
        ),
        omit.summary.stat = c("p25", "p75"),
        type = "latex",
        out = here("figures/results/descriptive_teacher.tex")
    )
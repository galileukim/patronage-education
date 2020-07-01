# present set of covariance balance visualizations to ensure that the treatment var
# is not strongly correlated with other independent variables
print("loading data")
saeb_hierarchical <- read_data(
  "saeb",
  "saeb_hierarchical.rds"
)

censo_school_turnover <- read_data(
  "censo_escolar",
  "censo_school_turnover.rds"
)

finbra_budget <- read_data(
  "finbra",
  "finbra.rds"
)

censo_school <- read_data(
  "censo_escolar",
  "censo_school.rds"
) %>%
  mutate(
    school_id = as.integer(school_id)
  ) %>%
  select(
    cod_ibge_6,
    school_id,
    year,
    all_of(school_covariates)
  )

rais_edu_mun <- read_data(
  "rais",
  "rais_edu.rds"
)

print("join datasets and prepare covariate data")

covariate_balance_data <- reduce(
  list(saeb_hierarchical, censo_school_turnover, finbra_budget, censo_school),
  left_join
) %>%
  filter(
    !is.na(turnover_index)
  ) %>%
  mutate(
    censo_log_pop = log(censo_pop),
    budget_education_capita = budget_education/censo_pop
  )

print("covariate balance testing")

municipal_controls <- c(
  mun_covariates,
  mayor_covariates,
  chamber_covariates
)

education_controls <- c(
  # rais_covariates,
  school_covariates,
  student_covariates,
  teacher_covariates
)

formula_municipal <- reformulate(municipal_controls, "turnover_index")
formula_education <- reformulate(education_controls, "turnover_index")

covariate_balance_municipal <- cobalt::bal.tab(
  formula_municipal,
  model.frame(formula_municipal, covariate_balance_data)
)

love_plot_municipal <- generate_love_plot(
  covariate_balance_municipal,
    labels = c(
      "censo_median_wage" = "municipal: median wage",
      "censo_rural" = "municipal: rural population",
      "censo_log_pop" = "municipal: total population (logged)",
      "censo_lit_rate" = "municipal: literacy rate",
      "budget_education_capita" = "municipal: education budget per capita",
      "mayor_campaign" = "mayor: expenditures in campaign (mayor)",
      "chamber_size" = "municipal: no. of seats legislative chamber",
      "mayor_reelected" = "mayor: incumbent"
    )
  )

save_fig(love_plot_municipal, "love_plot_municipal.pdf")

covariate_balance_education <- cobalt::bal.tab(
  formula_education,
  data = model.frame(formula_education, covariate_balance_data)
)

love_plot_education <- generate_love_plot(
  covariate_balance_education,
  limits = c(
      str_subset(education_controls, "teacher", negate = T),
      "saeb_wage_teacher_",
      "education_teacher_",
      "gender_teacher_"
    ),
    labels = c(
      "access_water" = "school: access to water",
      "access_electricity" = "school: access to electricity",
      "library" = "school: has library",
      "meal" = "school: offers meals to students",
      "parent_attend_college_student" = "student: parents attended college",
      "failed_school_year_student" = "student: repeated previous year",
      "saeb_wage_teacher_" = "teacher: wages",
      "education_teacher_" = "teacher: education level",
      "gender_teacher_" = "teacher: female"
    )
)

save_fig(love_plot_education, "love_plot_education_test.pdf")
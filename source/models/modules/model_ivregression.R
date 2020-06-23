# ==============================================================================
# estimate the causal effect of teacher turnover on student test scores
# note that this relies on the exclusion restriction, which is fundamentally
# unobservable.
# ==============================================================================
print("import data")

saeb_hierarchical <- read_data(
    "saeb",
    "saeb_hierarchical.rds"
)

print("begin estimation of instrumental variables model")

fe <- c(
  "as.factor(year)",
  "as.factor(state)"
)

controls <- c(
  teacher_covariates,
  principal_covariates,
  student_covariates,
  school_covariates,
  mun_covariates,
  fe
)

formula_iv <- reformulate(
    controls,
    "grade_exam"
)

fit_iv <- AER::ivreg(
  grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher +
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
    coalition_share + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + 
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
  data = saeb_hierarchical
)

fit_iv_2 <- AER::ivreg(
  grade_exam ~ turnover_index + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher +
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate| 
    first_year + saeb_principal_female + saeb_principal_education + saeb_principal_appointment +
    education_teacher + saeb_wage_teacher + 
    parent_attend_college_student + parents_school_meeting_student + fridge_student + failed_school_year_student +
    toilet + meal + 
    censo_median_wage + censo_log_pop + censo_rural + censo_lit_rate,
  data = saeb_hierarchical %>% 
    mutate(first_year = if_else(year == election_year + 1, 1, 0))
)

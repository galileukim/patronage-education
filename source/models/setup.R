# ==============================================================================
# set-up
# ==============================================================================
pacman::p_load(
  tidyverse,
  data.table,
  here,
  furrr
)

knitr::opts_chunk$set(
  eval = T,
  cache = T,
  cache.lazy = F,
  message = F,
  warning = F,
  echo = F,
  fig.height = 3,
  fig.width = 4,
  fig.align = "center"
)

set.seed(1789)

# ==============================================================================
# load auxiliary utils
# ==============================================================================
source(
  here("source", "models", "utils.R")
)

run_module <- partial(
  run_module,
  domain = "models"
)

read_data <- partial(
  read_data,
  type = "clean"
)

save_fig <- partial(
  save_fig,
  domain = "replication"
)

theme_set(
  theme_minimal() +
    theme_clean
)

# ==============================================================================
# covariates
# ==============================================================================
teacher_covariates <- c(
  "saeb_wage_teacher",
  "education_teacher",
  "gender_teacher"
)

principal_covariates <- c(
  "saeb_principal_female",
  "saeb_principal_higher_education",
  "saeb_principal_appointment"
)

student_covariates <- c(
  "parent_attend_college_student", # both proportion of classroom
  "failed_school_year_student"
)

school_covariates <- c(
  "access_water", # all dummies for school
  "access_electricity",
  "library",
  "meal"
)

mun_covariates <- c(
  "censo_median_wage",
  "censo_log_pop",
  "censo_rural",
  "censo_lit_rate",
  "budget_education_capita"
)

mayor_covariates <- c(
  "mayor_reelected",
  "mayor_campaign",
  "mayor_coalition_size"
)

chamber_covariates <- c(
  "chamber_size"
)

rais_covariates <- c(
  "rais_edu",
  "rais_wage",
  "rais_permanent"
  # "rais_time"
)
# ==============================================================================
# create teacher turnover index by school and municipality
# ==============================================================================
init_env <- ls()

censo_teacher_turnover <- read_data(
  "raw",
  "censo_escolar",
  "censo_teacher_turnover.csv.gz"
)

.group_vars <- c("state", "cod_ibge_6", "year", "school_id", "grade_level")
.turnover_vars <- c("turnover_entry", "turnover_exit", "turnover_transfer", "n")
complete_years <- 2007:2016

censo_teacher_turnover <- censo_teacher_turnover %>%
  mutate(
    across(
      all_of(.turnover_vars),
      as.numeric
    )
  )

future::plan(future::multiprocess, workers = 8)

censo_turnover_school <- censo_teacher_turnover %>%
  split(.$state) %>%
  furrr::future_map_dfr(
    ~ create_teacher_turnover_index(
      .,
      .group_vars,
      .turnover_vars,
      complete_years
    )
  ) 

censo_turnover_school_arranged <- censo_turnover_school %>%
  arrange(
    cod_ibge_6, 
    year,
    school_id
  )

censo_turnover_school_arranged %>%
  write_data(
    "censo_escolar",
    "censo_school_turnover.rds"
  )

.mun_group_vars <- str_subset(.group_vars, "school_id|grade_level", negate = T)

censo_turnover_mun <- censo_teacher_turnover %>%
  split(.$state) %>%
  furrr::future_map_dfr(
    ~ create_teacher_turnover_index(
      .,
      .mun_group_vars,
      .turnover_vars,
      complete_years
    )
  ) 

censo_turnover_mun %>%
  write_data(
    "censo_escolar",
    "censo_mun_turnover.rds"
  )
# generate municipal table
saeb_exam_mun <- read_data(
  "raw",
  "saeb",
  "saeb_exam_mun.csv"
) %>%
  add_election_year()

saeb_exam_mun %>%
  write_data(
    "saeb",
    "saeb_exam_mun.rds"
  )

# saeb_sample
saeb_student <- read_data(
  "raw",
  "saeb",
  "saeb_student.csv.gz"
)

saeb_student %>%
  sample_frac(0.25) %>%
  write_data(
    "saeb",
    "saeb_student_sample.rds"
  )

# breakdown by sphere
saeb_student %>%
  group_by(
    cod_ibge_6,
    year,
    cod_school
    dep
  ) %>%
  

# hierarchical model
saeb_hierarchical <- read_data(
  "raw",
  "saeb",
  "saeb_hierarchical.csv.gz"
)

saeb_hierarchical %>%
  write_data(
    "saeb",
    "saeb_hierarchical.rds"
  )
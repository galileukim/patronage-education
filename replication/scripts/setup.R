# set up ------------------------------------------------------------------
pacman::p_load(
  tidyverse,
  data.table,
  rgdal,
  here,
  R.utils,
  GGally,
  lfe,
  lme4,
  plm,
  broom,
  extrafont,
  rprojroot,
  tmap,
  gridExtra,
  cowplot,
  ggdag,
  scales,
  kableExtra,
  RColorBrewer,
  gghighlight,
  parallel,
  foreach,
  gridExtra,
  naniar,
  knitr,
  gghighlight,
  egg,
  WeightIt,
  cobalt,
  boot,
  rdrobust,
  magrittr,
  sjPlot
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
  # dev = "pdf"
)

# connect to database
con <- DBI::dbConnect(
  odbc::odbc(),
  driver = "PostgreSQL Unicode",
  database = "rais",
  UID = "gali",
  PWD = 'gali1789!',
  port = 5432
)

p_file_here <- partial(
  file_here,
  paper = 'job_market'
)

ggsave <- partial(
  ggplot2::ggsave,
  width = 5,
  height = 3
)

read_data <- partial(
  read_data,
  type = "clean"
)

controls <- c(
  "rais_permanent", "rais_size", "I(budget_education/censo_pop)", 
  "censo_median_wage", "censo_log_pop", "mayor_edu", 
  "mayor_coalition_size", "as.factor(mayor_party)"
)

rais_main_categories <- c("education", "administration", "services", "healthcare")


set.seed(1789)
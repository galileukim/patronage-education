# functions ---------------------------------------------------------------
`%<>%` <- magrittr::`%<>%`
ivreg <- AER::ivreg
between <- data.table::between

list.files <- partial(
  base::list.files,
  full.names = T
)

# io ----------------------------------------------------------------------
here_data <- function(type, dir, file) {
  path <- here("data", type, dir, file)
  
  return(path)
}

read_data <- function(type, dir, file) {
  file_path <- here_data(type, dir, file)

  if (str_detect(file, ".rds$")) {
    data <- read_rds(file_path)
  } else {
    data <- fread(file_path)
  }
  
  return(data)
}

write_data <- function(object, dir, file, type = "clean", compress = "gz") {
  file_path <- here_data(type, dir, file)

  if (str_detect(file, ".rds$")) {
    write_rds(object, file_path, compress = compress)
  } else {
    fwrite(object, file_path, compress = "gz")
  }
}

reset_env <- function(init_env){
  final_env <- ls(.GlobalEnv)

  rm(
    envir = .GlobalEnv,
    list = setdiff(final_env, init_env)
  )

  gc()
}

build_repo <- function(module){
  repo <- here("data", "clean", module)

  if(!dir.exists(repo)){
    dir.create(repo)
  }else{
    unlink(repo, recursive = T)
    dir.create(repo)
    }
  }

run_module <- function(module, domain) {
  print(
    paste0("running module ", module, "...")
  )

  build_repo(module)

  source_file <- paste0("datagen_", module, ".R")
  path <- here("source", domain, "modules", source_file)

  init_env <- ls(.GlobalEnv)
  source(path)
  reset_env(init_env)

  print(
    paste("module", module, "complete!")
  )
}

fread <- partial(
  data.table::fread,
  nThread = parallel::detectCores(),
  integer64 = c("character")
)

list_files <- function(path, pattern) {
  files <- map2(
    path,
    pattern,
    list.files
  ) %>%
    flatten_chr()

  return(files)
}

# data manipulation -------------------------------------------------------
calc_turnover <- function(data, group_vars){
  years <- data %>% 
    distinct(year) %>% 
    pull
  
  data %<>%
    group_by_at(
      vars(
        group_vars
      )
    ) %>% 
    summarise_at(
      vars(starts_with("turnover_"), n),
      sum,
      na.rm = T
    ) %>% 
    ungroup()
  
  data %<>% 
    mutate(
      implicit = 0
    ) %>% 
    complete(
      year = years,
      fill = list(implicit = 1)
    ) %>% 
    group_by_at(
      vars(
        group_vars,
        -year
      )
    ) %>% 
    mutate(
      n_lag = dplyr::lag(n, order_by = year),
      percent_exit = turnover_exit/n_lag,
      percent_transfer = turnover_transfer/n_lag,
      percent_extinct = turnover_extinct/n_lag,
      percent_entry = turnover_entry/n,
      turnover_index = (turnover_exit + turnover_transfer + turnover_entry)/(n + n_lag)
    ) %>% 
    ungroup() %>% 
    filter(
      implicit != 1
    ) %>% 
    select(
      -implicit
    )
  
  return(data)
}

add_election_year <- function(data) {
  data <- data %>%
    mutate(
      election_year = case_when(
        between(year, 1985, 1988) ~ 1984,
        between(year, 1989, 1992) ~ 1988,
        between(year, 1993, 1996) ~ 1992,
        between(year, 1997, 2000) ~ 1996,
        between(year, 2001, 2004) ~ 2000,
        between(year, 2005, 2008) ~ 2004,
        between(year, 2009, 2012) ~ 2008,
        between(year, 2013, 2016) ~ 2012
      )
    )

  return(data)
}
censo_files <- list.files(
  here("data", "raw", "censo_br"),
)

for (i in seq_along(censo_files)) {
  select_cols <- c(
      "cod_ibge_6",
      "median_wage",
      "rural",
      "lit_rate",
      "sanitation",
      "garbage",
      "light",
      "pop",
      "student_age",
      "log_pop" 
  )

  censo <- censo_files[i] %>% # filename
    fread %>% # read data
    mutate( # select cols
      log_pop = log(pop)
    ) %>%
    select(
      any_of(select_cols)
    ) %>%
    rename_with(
      ~ paste0("censo_", .),
      c(-cod_ibge_6)
    )

  # write-out
  censo %>% 
    write_data(
      "censo_br", 
      paste0("censo_", c(2000, 2010)[i], ".rds")
    )
}

# create some test fixtures
# structure of the data coming in
# call this function with that fixture
# output needs to be transformed in certain ways
# specifically write the logic
# are the transformations occurring properly
# in order to write tests you need to change the way you write the code
# here is the file path, here is the array of the columns I care about
# run the function on a dummy data with five cols, selecting three of those cols.
# assert that it has the correct amount of rows and cols.
# the desired dimensions of the data is consistent.
# create simpler tests more manageable datasets, handwrite them and test out your calcs are doing
# what you intend them to do. (check your math)
# checking that the answer is the expected answer.
# identify and locate where the bug is.
# iteration cycles, the faster you can iterate.
# make a dummy variable that is exactly the same structure of the original data.
# be confident about where it failed.
# so the log will print out the standard output (er, warn, or standard output).
# embed logs in the actual script. what is the important data.
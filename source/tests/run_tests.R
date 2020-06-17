# ==============================================================================
# run unit tests to check whether functions are running properly
# ==============================================================================
source(
    here::here("source", "data", "setup.R")
)

source(
    here::here("source", "data", "utils.R")
)

library(testthat)
path_test <- function(x) {here("source", "tests", x)}

test_file(path_test("test_data_io.R"))
test_file(path_test("test_data_turnover.R"))
walk(
   c(
       here::here("source", "data", "utils.R"),
    here::here("source", "data", "setup.R")
   ),
    source
)

context("testing data utils")

# ==============================================================================
# data io
# ==============================================================================
print("testing data io")

data <- tibble(
    a = letters[1:10],
    b = 1:10,
    c = 11:20
)

select_cols <- c("a", "b")

data_csv <- get_data("raw", "test_data", "test_data.rds", cols = select_cols)
data_rds <- get_data("raw", "test_data", "test_data.csv", cols = select_cols)

test_that("check if get_data returns correct dimensions", {
    expect_equal(
        dim(data_csv),
        dim(data_rds)
    )
    expect_equal(
        data_csv,
        select(data, select_cols)
    )
    expect_equal(
        ncol(data_csv),
        length(select_cols)
    )
    expect_equal(
        nrow(data_csv),
        nrow(data)
    )
})

# ==============================================================================
# teacher turnover calculation
# ==============================================================================
nrows <- 2

turnover_data <- tibble(
    school = "a",
    year = c(2005:2006, 2008),
    n = c(2, 5, 6),
    n_lag = lag(n),
    turnover_exit = c(2, 2, 3),
    turnover_transfer = 0,
    turnover_entry = c(0, 2, 4)
)

.group_vars <- c("school", "year")

turnover_sum <- turnover_data %>%
    group_sum(
        .group_vars,
        c(starts_with("turnover"), n)
    )
    
completed_data <- turnover_sum %>%
    complete_year_data(
        "school",
        2005:2008
    )

turnover_index_data  <- calc_turnover(
    completed_data,
    c("school", "year")
)

test_that("check if calc_turnover returns correct object", {
    expect_equal(
        dim(turnover_data_calculated), 
        c(length(.group_vars), length(.vars) + length(.group_vars))
    )
    
    expect_identical(
        summed_data,
        turnover_data %>%
            select(.group_vars, starts_with("turnover"))
    )

    expect_equal(
        sort(completed_data$year),
        2005:2008
    )
    
    expect_equal(
        turnover_index_data %>% 
        filter(year == 2006) %>% 
        select(turnover_index),
        turnover_data %>% 
        filter(year == 2006) %>% 
        summarise(turnover_index = (turnover_exit + turnover_transfer + turnover_entry)/(n + n_lag))
    )
})

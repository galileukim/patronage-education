
# ==============================================================================
# teacher turnover calculation
# ==============================================================================
context("testing turnover utils")

nrows <- 2
complete_years <- 2005:2008
.group_vars <- c("school", "year")
.vars <- c("turnover_exit", "turnover_transfer", "turnover_entry", "n")


turnover_data <- tibble(
    school = "a",
    year = c(2005:2006, 2008),
    n = c(2, 5, 6),
    n_lag = lag(n),
    turnover_exit = c(2, 2, 3),
    turnover_transfer = 0,
    turnover_entry = c(0, 2, 4)
)

turnover_sum <- turnover_data %>%
    group_sum(
        .group_vars,
        .vars
    )

completed_data <- turnover_sum %>%
    complete_year_data(
        "school",
        complete_years
    )

turnover_index_data <- calc_turnover(
    completed_data,
    c("school", "year")
)

test_that("check if calc_turnover returns correct object", {
    n_groupings <- nrow(distinct(turnover_data, across(.group_vars)))
    n_cols <- length(.vars) + length(.group_vars)

    max_turnover <- max(turnover_index_data$turnover_index, na.rm = T)
    manual_max_turnover <- with(
        turnover_index_data,
        max(
            turnover_index = (turnover_exit + turnover_transfer + turnover_entry) / (n + n_lag),
            na.rm = T
        )
    )

    expect_equal(
        dim(turnover_sum),
        c(n_groupings, n_cols)
    )

    expect_equal(
        sort(completed_data$year),
        complete_years
    )

    expect_equal(
        max_turnover,
        manual_max_turnover
    )

    expect_equal(
        ceiling(max_turnover),
        1
    )
})
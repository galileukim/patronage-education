
# ==============================================================================
# data io
# ==============================================================================
context("testing data io utils")

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

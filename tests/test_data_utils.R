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
nrows <- 10

turnover_data <- tibble(
    school = rep(c("a", "b"), each = nrows/2),
    year = rep(c(2006:2007), each = nrows/2),
    n = 5
    # turnover_index = 
)

.group_vars <- c("school")
.vars <- c("n")

turnover_data_calculated <- calc_turnover(
    turnover_data,
    .group_vars,
    .vars
)

test_that("check if calc_turnover returns correct dims", {
    expect_equal(
        dim(turnover_data_calculated), 
        c(length(.group_vars), length(.vars) + length(.group_vars))
    )
    
})


     df <- tibble(
       group = c(1:2, 1),
       item_id = c(1:2, 2),
       item_name = c("a", "b", "b"),
       value1 = 1:3,
       value2 = 4:6
     )

     df %>% complete(group, nesting(item_id, item_name))

     df %>% group_by(item_name) %>% complete(group = 1:2)
     df %>% complete_grouped_data("item_name", group, 1:2)
     
     # You can also choose to fill in missing values
     df %>% complete(group, nesting(item_id, item_name), fill = list(value1 = 0))

calc_turnover(
    turnover_data,
    "year",
    "n"
)


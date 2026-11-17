test_that("variable selection directly returns ordinary draws", {
  x <- example_frame()
  file <- load_draws(write_draws(x))
  on.exit(close_draws(file))
  for (args in list(
    list(variable = "mu"),
    list(variable = "theta"),
    list(variable = c("theta[2]", "mu", "theta[1]")),
    list(variable = "theta\\[[12]\\]", regex = TRUE),
    list(variable = "theta", exclude = TRUE),
    list(variable = "mu", scalar = TRUE),
    list(variable = "absent", regex = TRUE),
    list(variable = character())
  )) {
    query <- do.call(subset_draws, c(list(x = file), args))
    expect_s3_class(query, "draws_df")
    expect_equal(query,
                 do.call(posterior::subset_draws, c(list(x = x), args)))
  }
  expect_equal(subset_draws(file), x)
  expect_equal(
    subset_draws(file, variable = character(), chain = 2),
    posterior::subset_draws(x, variable = character(), chain = 2)
  )
  expect_error(subset_draws(file, variable = "absent"), "not found|missing")
  expect_error(subset_draws(file, variable = "theta", scalar = TRUE))
})

test_that("all row selection options match posterior", {
  x <- example_frame()
  file <- load_draws(write_draws(x))
  on.exit(close_draws(file))
  for (args in list(
    list(chain = 2),
    list(chain = c(3, 1), iteration = c(10, 3, 1)),
    list(iteration = 5:10),
    list(chain = 2, iteration = 5:10, exclude = TRUE),
    list(chain = c(2, 2, 1), unique = FALSE),
    list(iteration = c(5, 3, 5), unique = FALSE),
    list(chain = c(3, 1, 3), iteration = c(5, 3, 5), unique = FALSE),
    list(draw = c(250, 1, 201)),
    list(draw = c(3, 3, 1), unique = FALSE),
    list(draw = 1:100, exclude = TRUE),
    list(chain = integer()),
    list(draw = integer())
  )) {
    actual <- suppressMessages(do.call(subset_draws, c(list(x = file, variable = "mu"), args)))
    expected <- suppressMessages(do.call(
      posterior::subset_draws, c(list(x = x, variable = "mu"), args)
    ))
    expect_equal(actual, expected, info = paste(names(args), collapse = ","))
  }
})

test_that("consecutive subsets use repaired indices", {
  x <- example_frame()
  file <- load_draws(write_draws(x))
  on.exit(close_draws(file))
  query <- file |>
    subset_draws(variable = "theta", chain = c(2, 4), iteration = 10:20) |>
    subset_draws(variable = "theta[1]", chain = 2, iteration = 1:3)
  expected <- x |>
    posterior::subset_draws(variable = "theta", chain = c(2, 4), iteration = 10:20) |>
    posterior::subset_draws(variable = "theta[1]", chain = 2, iteration = 1:3)
  expect_equal(query, expected)
  query <- file |>
    subset_draws(chain = c(2, 2, 1), unique = FALSE) |>
    subset_draws(draw = c(3, 103, 203)) |>
    subset_draws(iteration = 2)
  expected <- suppressMessages(x |>
    posterior::subset_draws(chain = c(2, 2, 1), unique = FALSE) |>
    posterior::subset_draws(draw = c(3, 103, 203)) |>
    posterior::subset_draws(iteration = 2))
  expect_equal(suppressMessages(query), expected)
})

test_that("subsetting executes immediately and the source stays reusable", {
  file <- write_draws()
  x <- load_draws(file)
  on.exit(close_draws(x))
  a <- subset_draws(x, variable = "mu", chain = 1)
  b <- subset_draws(x, variable = "tau", chain = 2)
  expect_s3_class(a, "draws_df")
  expect_identical(posterior::variables(a), "mu")
  expect_identical(posterior::variables(b), "tau")
  expect_identical(x$variables, posterior::variables(example_frame()))
  expect_true(DBI::dbIsValid(x$con))
  expect_output(print(x), "Lazy posterior draws")
  expect_error(subset_draws(x, chain = 999))
  unlink(file)
  expect_error(subset_draws(x, variable = "mu"))
  expect_equal(a, posterior::subset_draws(example_frame(), variable = "mu", chain = 1))
})

test_that("DuckDB connections work without changing ownership or user tables", {
  x <- example_frame()
  con <- connect_draws()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  table <- "draws with ' quotes"
  duckdb::duckdb_register(con, table, as.data.frame(x))
  query <- subset_draws(as_lazy_draws(con, table), variable = "mu", chain = 2)
  expect_equal(query, posterior::subset_draws(x, variable = "mu", chain = 2))
  expect_true(DBI::dbIsValid(con))
  expect_identical(DBI::dbListTables(con), table)
  expect_equal(subset_draws(as_lazy_draws(con, table)), x)
  other <- connect_draws()
  duckdb::duckdb_register(other, "draws", as.data.frame(x))
  source <- as_lazy_draws(other)
  query <- subset_draws(source, variable = "mu")
  DBI::dbDisconnect(other, shutdown = TRUE)
  expect_error(subset_draws(source), "closed")
  expect_s3_class(query, "draws_df")
})

test_that("invalid schemas, indices, and conflicting selectors fail clearly", {
  x <- example_frame()
  expect_error(subset_draws(load_draws(write_draws(x)), draw = 1, chain = 1), "Cannot combine")
  expect_error(subset_draws(load_draws(write_draws(x)), draw = 1, iteration = 1), "Cannot combine")
  x$.draw[2] <- x$.draw[1]
  expect_error(read_draws(write_draws(x)), "unique draws")
  x <- as.data.frame(example_frame())
  x$.chain <- NULL
  expect_error(load_draws(write_draws(x)), "must contain")
  expect_error(load_draws("nonexistent.parquet"))
  expect_error(subset_draws(load_draws(write_draws()), unique = NA), "TRUE or FALSE")
})

test_that("unselected parameter values never need conversion to R", {
  con <- connect_draws()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW draws AS SELECT 1.5 AS mu, INTERVAL '1 month' AS unused, ",
    '1 AS ".chain", 1 AS ".iteration", 1 AS ".draw"'
  ))
  # INTERVAL has no ordinary posterior numeric representation. Selecting mu
  # must never fetch or try to convert it.
  x <- subset_draws(as_lazy_draws(con), variable = "mu", chain = 1)
  expect_identical(posterior::variables(x), "mu")
  expect_equal(x$mu, 1.5)
})

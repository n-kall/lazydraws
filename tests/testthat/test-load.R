test_that("load_draws stays lazy and subset_draws returns in-memory draws", {
  x <- example_frame()
  drw <- load_draws(write_draws(x))
  on.exit(close_draws(drw))
  expect_s3_class(drw, "lazydraws", exact = TRUE)
  expect_false(inherits(drw, "draws"))
  expect_true(DBI::dbIsValid(drw$con))
  expect_identical(DBI::dbGetQuery(drw$con, paste0(
    "SELECT table_type FROM information_schema.tables WHERE table_name = 'draws'"
  ))$table_type, "VIEW")
  expect_identical(subset_draws, posterior::subset_draws)
  expect_false("collect_draws" %in% getNamespaceExports("lazydraws"))
  expect_equal(subset_draws(drw), x)
  result <- posterior::subset_draws(drw, variable = "mu", chain = 2)
  expect_s3_class(result, "draws_df", exact = FALSE)
  expect_false(inherits(result, "lazydraws"))
  expect_equal(result, posterior::subset_draws(x, variable = "mu", chain = 2))
  expect_true(DBI::dbIsValid(drw$con))
  expect_equal(nrow(posterior::summarise_draws(result, "mean")), 1)
})

test_that("file paths with quotes are handled safely", {
  file <- paste0(tempfile(), "' draws.parquet")
  file.copy(write_draws(), file)
  drw <- load_draws(file)
  on.exit(close_draws(drw))
  expect_equal(subset_draws(drw), example_frame())
  expect_equal(subset_draws(drw, variable = "mu"),
               posterior::subset_draws(example_frame(), variable = "mu"))
})

test_that("closing the source leaves returned draws usable", {
  drw <- load_draws(write_draws())
  result <- subset_draws(drw, variable = "mu")
  alias <- drw
  expect_identical(close_draws(alias), alias)
  expect_false(DBI::dbIsValid(drw$con))
  expect_error(subset_draws(drw), "closed")
  expect_output(print(drw), "closed")
  expect_equal(result, posterior::subset_draws(example_frame(), variable = "mu"))
  expect_identical(close_draws(drw), drw)
  expect_error(close_draws(NULL), "lazydraws object")
})

test_that("automatic cleanup is independent of returned in-memory draws", {
  drw <- load_draws(write_draws())
  result <- subset_draws(drw, variable = "mu")
  alias <- drw
  con <- drw$con
  rm(drw)
  gc()
  expect_true(DBI::dbIsValid(con))
  rm(alias)
  gc()
  expect_false(DBI::dbIsValid(con))
  expect_s3_class(result, "draws_df")
})

test_that("load failures close the connection and invalid arguments fail", {
  x <- as.data.frame(example_frame())
  x$.chain <- NULL
  file <- write_draws(x)
  con <- connect_draws()
  local_mocked_bindings(connect_draws = function() con)
  expect_error(load_draws(file), "must contain")
  expect_false(DBI::dbIsValid(con))
  expect_error(load_draws("nonexistent.parquet"))
  expect_error(load_draws(NA_character_), "nonempty string")
})

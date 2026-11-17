test_that("cached counts match posterior and remain available after closing", {
  x <- example_frame()
  file <- write_draws(x)
  drw <- load_draws(file)
  close_draws(drw)
  unlink(file)
  expect_equal(posterior::nchains(drw), posterior::nchains(x))
  expect_equal(posterior::niterations(drw), posterior::niterations(x))
  expect_equal(posterior::ndraws(drw), posterior::ndraws(x))
  expect_identical(posterior::nvariables(drw), posterior::nvariables(x))
  expect_identical(posterior::nvariables(drw, with_indices = FALSE),
                   posterior::nvariables(x, with_indices = FALSE))
  expect_identical(posterior::nvariables(drw, reserved = TRUE),
                   posterior::nvariables(x, reserved = TRUE))
})

test_that("ID methods match posterior and require an open source", {
  x <- example_frame()
  drw <- load_draws(write_draws(x))
  on.exit(close_draws(drw))
  expect_identical(posterior::chain_ids(drw), posterior::chain_ids(x))
  expect_identical(posterior::iteration_ids(drw), posterior::iteration_ids(x))
  expect_identical(posterior::draw_ids(drw), posterior::draw_ids(x))
  close_draws(drw)
  expect_error(posterior::chain_ids(drw), "closed")
  expect_error(posterior::iteration_ids(drw), "closed")
  expect_error(posterior::draw_ids(drw), "closed")
})

test_that("IDs come from index columns rather than cached counts", {
  frame <- data.frame(value = 1:4, .chain = c(2L, 2L, 4L, 4L),
                      .iteration = c(10L, 30L, 10L, 30L),
                      .draw = c(101L, 103L, 201L, 203L))
  drw <- load_draws(write_draws(frame))
  on.exit(close_draws(drw))
  expect_equal(posterior::nchains(drw), 2)
  expect_equal(posterior::niterations(drw), 2)
  expect_equal(posterior::ndraws(drw), 4)
  expect_identical(posterior::chain_ids(drw), c(2L, 4L))
  expect_identical(posterior::iteration_ids(drw), c(10L, 30L))
  expect_identical(posterior::draw_ids(drw), c(101L, 103L, 201L, 203L))
})

test_that("metadata inspection does not fetch parameter columns", {
  con <- connect_draws()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, paste0(
    "CREATE VIEW draws AS SELECT INTERVAL '1 month' AS unused, ",
    '2 AS ".chain", 10 AS ".iteration", 101 AS ".draw"'
  ))
  x <- new_lazy_draws(con)
  expect_equal(posterior::nchains(x), 1)
  expect_equal(posterior::niterations(x), 1)
  expect_equal(posterior::ndraws(x), 1)
  expect_equal(posterior::nvariables(x), 1)
  expect_identical(posterior::chain_ids(x), 2L)
  expect_identical(posterior::iteration_ids(x), 10L)
  expect_identical(posterior::draw_ids(x), 101L)
})

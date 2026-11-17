test_that("variable names follow posterior without querying the source", {
  x <- example_frame()
  file <- write_draws(x)
  drw <- load_draws(file)
  close_draws(drw)
  unlink(file)
  expect_identical(posterior::variables(drw), posterior::variables(x))
  expect_identical(posterior::variables(drw, with_indices = FALSE),
                   posterior::variables(x, with_indices = FALSE))
  expect_identical(posterior::variables(drw, reserved = TRUE),
                   posterior::variables(x, reserved = TRUE))
})

test_that("printing uses cached counts and truncates variable names", {
  x <- example_frame()
  file <- write_draws(x)
  drw <- load_draws(file)
  close_draws(drw)
  unlink(file)
  output <- capture.output(result <- print(drw, max_variables = 2))
  expect_identical(result, drw)
  expect_match(output[1], "closed")
  expect_identical(output[2], paste0(
    posterior::niterations(x), " iterations, ", posterior::nchains(x),
    " chains, ", posterior::ndraws(x), " draws, ", posterior::nvariables(x),
    " variables"
  ))
  expect_identical(output[4], "Variables: mu, tau")
  expect_match(tail(output, 1), "8 more variables", fixed = TRUE)
  expect_output(print(drw, max_variables = 10), "theta[8]", fixed = TRUE)
  expect_output(print(drw, max_variables = 0), "(not shown)", fixed = TRUE)
  expect_error(print(drw, max_variables = -1), "nonnegative integer")
  expect_error(print(drw, max_variables = NA_real_), "nonnegative integer")
})

test_that("printing respects the posterior variable limit option", {
  drw <- load_draws(write_draws())
  on.exit(close_draws(drw))
  previous <- options(posterior.max_variables = 1)
  on.exit(options(previous), add = TRUE)
  expect_output(print(drw), "Variables: mu", fixed = TRUE)
  expect_output(print(drw), "9 more variables", fixed = TRUE)
})

test_that("unsupported posterior operations explain how to load draws first", {
  # No source or connection: dispatch must error without trying to fetch data.
  source <- structure(list(), class = "lazydraws")
  operations <- c(
    "mutate_variables", "rename_variables", "variables<-", "thin_draws",
    "bind_draws", "merge_chains", "split_chains", "order_draws", "repair_draws",
    "resample_draws", "weight_draws", "summarise_draws", "extract_variable",
    "as_draws", "as_draws_array", "as_draws_df", "as_draws_list",
    "as_draws_matrix", "as_draws_rvars"
  )
  for (operation in operations) {
    fun <- getExportedValue("posterior", operation)
    error <- tryCatch(do.call(fun, list(source)), error = identity)
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), paste0("`", operation, "()`"), fixed = TRUE)
    expect_match(conditionMessage(error), "posterior::subset_draws()", fixed = TRUE)
    expect_match(conditionMessage(error), "resulting `draws_df`", fixed = TRUE)
  }
  expect_error(posterior::summarize_draws(source), "subset_draws", fixed = TRUE)
})

test_that("unsupported operations do not evaluate arguments or query a source", {
  drw <- load_draws(write_draws())
  close_draws(drw)
  expect_error(
    posterior::mutate_variables(drw, bad = stop("expression evaluated")),
    "posterior::subset_draws()", fixed = TRUE
  )
  expect_error(
    posterior::thin_draws(drw, thin = stop("argument evaluated")),
    "posterior::subset_draws()", fixed = TRUE
  )
})

test_that("posterior operations work normally after subsetting", {
  drw <- load_draws(write_draws())
  on.exit(close_draws(drw))
  x <- posterior::subset_draws(drw, variable = "mu")
  mutated <- posterior::mutate_variables(x, twice_mu = 2 * mu)
  expect_equal(mutated$twice_mu, 2 * x$mu)
  expect_equal(posterior::niterations(posterior::thin_draws(x, thin = 2)),
               posterior::niterations(x) / 2)
  expect_equal(nrow(posterior::summarise_draws(x, "mean")), 1)
  expect_s3_class(posterior::as_draws_array(x), "draws_array")
  expect_equal(posterior::nchains(drw), posterior::nchains(x))
  expect_identical(posterior::variables(drw), posterior::variables(example_frame()))
})

skip_if_not_installed("cmdstanr")

test_that("export matches CSV draws and diagnostics without caching draws", {
  fit <- fixture_fit()
  expected <- cmdstanr::read_cmdstan_csv(fixture_files(), format = "df")
  fit$.__enclos_env__$private$draws_ <- NULL
  fit$.__enclos_env__$private$sampler_diagnostics_ <- NULL
  for (warmup in c(FALSE, TRUE)) {
    file <- tempfile(fileext = ".parquet")
    expect_identical(cmdstanr_to_parquet_draws(fit, file, inc_warmup = warmup), file)
    actual <- read_draws(file)
    draws <- expected$post_warmup_draws
    diagnostics <- expected$post_warmup_sampler_diagnostics
    if (warmup) {
      draws <- posterior::bind_draws(expected$warmup_draws, draws, along = "iteration")
      diagnostics <- posterior::bind_draws(
        expected$warmup_sampler_diagnostics, diagnostics, along = "iteration"
      )
      draws <- posterior::order_draws(draws)
      diagnostics <- posterior::order_draws(diagnostics)
    }
    expect_equal(as.data.frame(actual)[, names(draws)], as.data.frame(draws))
    expect_equal(as.data.frame(actual)[, names(diagnostics)], as.data.frame(diagnostics))
    expect_null(fit$.__enclos_env__$private$draws_)
    expect_null(fit$.__enclos_env__$private$sampler_diagnostics_)
  }
})

test_that("indexed variables and selective export work", {
  files <- vapply(fixture_files(), function(f) {
    file <- tempfile(fileext = ".csv")
    writeLines(sub(",mu,sigma$", ",theta.1,theta.2", readLines(f)), file)
    file
  }, character(1))
  fit <- fixture_fit(files)
  file <- tempfile(fileext = ".parquet")
  cmdstanr_to_parquet_draws(fit, file, variables = "theta", sampler_diagnostics = FALSE)
  expect_equal(read_draws(file), fit$draws("theta", format = "df"))
  cmdstanr_to_parquet_draws(fit, file, variables = "theta[2]",
                           sampler_diagnostics = FALSE, overwrite = TRUE)
  expect_identical(posterior::variables(read_draws(file)), "theta[2]")
  expect_error(cmdstanr_to_parquet_draws(fit, tempfile(), variables = "absent"))
  expect_error(cmdstanr_to_parquet_draws(fit, tempfile(), variables = "",
                                        sampler_diagnostics = FALSE), "at least one")
})

test_that("thinning and unsaved warmup are handled", {
  lines <- readLines(fixture_files()[1])
  rows <- which(grepl("^[0-9+-]", lines))
  keep <- c(seq(1, 100, by = 3), 100 + seq(1, 100, by = 3))
  thinned <- sub("thin = 1", "thin = 3", lines[-rows[-keep]], fixed = TRUE)
  csv <- tempfile(fileext = ".csv")
  writeLines(thinned, csv)
  fit <- fixture_fit(csv)
  file <- tempfile(fileext = ".parquet")
  cmdstanr_to_parquet_draws(fit, file, sampler_diagnostics = FALSE)
  expect_equal(read_draws(file), fit$draws(format = "df"))
  expect_equal(posterior::niterations(read_draws(file)), 34)
  fit <- fixture_fit(test_path("fixtures", "model1-2-no-warmup.csv"))
  cmdstanr_to_parquet_draws(fit, file, sampler_diagnostics = FALSE, overwrite = TRUE)
  expect_equal(read_draws(file), fit$draws(format = "df"))
  expect_error(cmdstanr_to_parquet_draws(fit, tempfile(), inc_warmup = TRUE), "not saved")
})

test_that("failed conversion preserves destination and rejects broken chains", {
  csv <- tempfile(fileext = ".csv")
  file.copy(fixture_files()[1], csv)
  fit <- fixture_fit(csv)
  file <- tempfile(fileext = ".parquet")
  writeLines("existing output", file)
  expect_error(cmdstanr_to_parquet_draws(fit, file), "already exists")
  lines <- readLines(fixture_files()[1])
  row <- tail(which(grepl("^[0-9+-]", lines)), 1)
  writeLines(lines[-row], csv)
  expect_error(cmdstanr_to_parquet_draws(fit, file, overwrite = TRUE), "completed chains")
  lines[row] <- "1,2,3"
  writeLines(lines, csv)
  expect_error(cmdstanr_to_parquet_draws(fit, file, overwrite = TRUE))
  expect_identical(readLines(file), "existing output")
  expect_error(cmdstanr_to_parquet_draws(fit, csv, overwrite = TRUE), "input CSV")
  expect_error(cmdstanr_to_parquet_draws(NULL, tempfile()), "MCMC fit")
  expect_error(cmdstanr_to_parquet_draws(fit, tempfile(), threads = 0), "positive integer")
})

test_that("export preserves floating point and non-finite values", {
  csv <- tempfile(fileext = ".csv")
  file.copy(fixture_files()[1], csv)
  fit <- fixture_fit(csv)
  lines <- readLines(fixture_files()[1])
  rows <- which(grepl("^[0-9+-]", lines))
  for (i in seq_along(rows)) {
    fields <- strsplit(lines[rows[i]], ",", fixed = TRUE)[[1]]
    fields[8:9] <- if (i == length(rows)) c("0.125", "nan") else c("1", "2")
    if (i == length(rows) - 1) fields[8:9] <- c("inf", "-inf")
    lines[rows[i]] <- paste(fields, collapse = ",")
  }
  writeLines(lines, csv)
  file <- tempfile(fileext = ".parquet")
  cmdstanr_to_parquet_draws(fit, file, sampler_diagnostics = FALSE)
  x <- read_draws(file)
  expect_identical(tail(x$mu, 2), c(Inf, 0.125))
  expect_identical(tail(x$sigma, 2), c(-Inf, NaN))
})

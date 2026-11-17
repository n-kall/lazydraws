example_frame <- function() {
  posterior::as_draws_df(posterior::example_draws())
}

write_draws <- function(x = example_frame()) {
  con <- connect_draws()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  file <- tempfile(fileext = ".parquet")
  duckdb::duckdb_register(con, "input", as.data.frame(x))
  DBI::dbExecute(con, paste0(
    "COPY input TO ", quote_strings(con, file), " (FORMAT PARQUET)"
  ))
  file
}

fixture_files <- function() {
  test_path("fixtures", paste0("model1-", 1:2, "-warmup.csv"))
}

fixture_fit <- function(files = fixture_files()) {
  cmdstanr::as_cmdstan_fit(files, check_diagnostics = FALSE)
}

read_draws <- function(file) {
  x <- load_draws(file)
  on.exit(close_draws(x))
  posterior::subset_draws(x)
}

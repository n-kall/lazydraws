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

read_draws <- function(file) {
  x <- load_draws(file)
  on.exit(close_draws(x))
  posterior::subset_draws(x)
}

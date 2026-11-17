#' Load lazy posterior draws from Parquet
#'
#' @param file Path to a Parquet file containing draws and the `.chain`,
#'   `.iteration`, and `.draw` columns.
#'
#' @details Creates an in-memory DuckDB connection and a view over the Parquet
#'   file. The schema and index columns are inspected to cache variable names
#'   and counts of draws, chains, and iterations; parameter values are not loaded
#'   or copied into a database table. Printing and [posterior::variables()] use
#'   these cached details without querying the file. Printing shows up to
#'   `getOption("posterior.max_variables", 8)` names; override this with
#'   `print(x, max_variables = ...)`. Cached details describe the file at load
#'   time. The Parquet file must remain available while the connection is used.
#'
#'   [posterior::nchains()], [posterior::niterations()], [posterior::ndraws()],
#'   and [posterior::nvariables()] use cached metadata, including after closing
#'   the source. [posterior::chain_ids()], [posterior::iteration_ids()], and
#'   [posterior::draw_ids()] return the actual distinct IDs in ascending order,
#'   reading only the relevant index column. ID methods require an open source.
#'
#'   Returns an S3 object of class `lazydraws`. Use [posterior::subset_draws()]
#'   to execute a selection and return an ordinary [posterior::draws_df]. Only
#'   selected parameter values enter R. With no selectors, all draws are read.
#'
#'   Row selection first reads the index columns into R, applies posterior's
#'   indexing rules, then fetches selected values through a DuckDB join. It uses
#'   memory for the original draw indices plus the selected parameter values.
#'
#'   The source remains open and reusable after subsetting. Its connection is
#'   cleaned up automatically when the last `lazydraws` object referring to it
#'   is garbage-collected, or when the R session exits. Use [close_draws()] to
#'   release the connection immediately without affecting already returned
#'   draws. Objects are session-local; reopen the Parquet file rather than
#'   serializing a connection.
#'
#' @return An S3 object of class `lazydraws`. It does not inherit from `draws`;
#'   subset it before using functions that require in-memory draws.
#' @export
#' @examples
#' \dontrun{
#' drw <- load_draws("draws.parquet")
#' drw |>
#'   posterior::subset_draws(variable = "theta", chain = 1:2) |>
#'   posterior::summarise_draws()
#' }
load_draws <- function(file) {
  check_string(file, "file")
  file <- normalizePath(path.expand(file), mustWork = TRUE)
  con <- connect_draws()
  success <- FALSE
  on.exit(if (!success) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE VIEW draws AS SELECT * FROM read_parquet(",
    quote_strings(con, file), ")"
  ))
  x <- new_lazy_draws(con)
  owner <- new.env(parent = emptyenv())
  owner$con <- con
  reg.finalizer(owner, finalize_connection, onexit = TRUE)
  x$owner <- owner
  x$file <- file
  success <- TRUE
  x
}

#' Close the connection of lazy draws
#'
#' @param x A `lazydraws` object returned by [load_draws()].
#'
#' @details Calling this function again on a closed object is harmless.
#'   This is optional: the connection is cleaned up automatically when the last
#'   `lazydraws` object referring to it is garbage-collected, or when R exits.
#'   Subsetting keeps the source open for repeated inspection. Draws already
#'   returned by subsetting remain usable after closing the source.
#'
#' @return The object, invisibly.
#' @export
close_draws <- function(x) {
  if (!inherits(x, "lazydraws")) {
    stop("x must be a lazydraws object.", call. = FALSE)
  }
  if (DBI::dbIsValid(x$con)) {
    DBI::dbDisconnect(x$con, shutdown = TRUE)
  }
  invisible(x)
}

finalize_connection <- function(owner) {
  if (DBI::dbIsValid(owner$con)) {
    try(DBI::dbDisconnect(owner$con, shutdown = TRUE), silent = TRUE)
  }
}

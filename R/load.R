#' Load lazy posterior draws from Parquet
#'
#' @param file Path to a Parquet file containing draws and the `.chain`,
#'   `.iteration`, and `.draw` columns.
#' @param table Name of the DuckDB view to create. Defaults to `"draws"`.
#'
#' @details Creates an in-memory DuckDB connection and a view over the Parquet
#'   file. Only the schema is inspected; parameter values are not loaded or
#'   copied into a database table. The Parquet file must remain available while
#'   the connection is used.
#'
#'   Returns an S3 object of class `lazydraws`. Use [posterior::subset_draws()]
#'   to execute a selection and return an ordinary [posterior::draws_df]. Only
#'   selected parameter values enter R. With no selectors, all draws are read.
#'   The view name is stored in the object, including for a custom `table`.
#'
#'   Row selection first reads the index columns into R, applies posterior's
#'   indexing rules, then fetches selected values through a DuckDB join. It uses
#'   memory for the original draw indices plus the selected parameter values.
#'
#'   The source remains open and reusable after subsetting. [close_draws()]
#'   closes its connection without affecting already returned draws. Automatic
#'   cleanup runs when the last object holding the connection is garbage
#'   collected, or when the R session exits. Objects are session-local; reopen
#'   the Parquet file rather than serializing a connection.
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
#' close_draws(drw)
#' }
load_draws <- function(file, table = "draws") {
  check_string(file, "file")
  check_string(table, "table")
  file <- normalizePath(path.expand(file), mustWork = TRUE)
  con <- connect_draws()
  success <- FALSE
  on.exit(if (!success) DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "CREATE VIEW ", quote_columns(con, table), " AS SELECT * FROM read_parquet(",
    quote_strings(con, file), ")"
  ))
  x <- as_lazy_draws(con, table = table)
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
#'   Subsetting does not close the source connection automatically, allowing
#'   repeated inspection. Draws already returned by subsetting remain usable.
#'
#' @return The object, invisibly.
#' @export
close_draws <- function(x) {
  if (!inherits(x, "lazydraws")) {
    stop("x must be a lazydraws object.", call. = FALSE)
  }
  if (!is.null(x$con) && DBI::dbIsValid(x$con)) {
    DBI::dbDisconnect(x$con, shutdown = TRUE)
  }
  invisible(x)
}

finalize_connection <- function(owner) {
  if (DBI::dbIsValid(owner$con)) {
    try(DBI::dbDisconnect(owner$con, shutdown = TRUE), silent = TRUE)
  }
}

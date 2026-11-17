#' @importFrom posterior nchains
#' @export
nchains.lazydraws <- function(x) {
  x$nchains
}

#' @importFrom posterior niterations
#' @export
niterations.lazydraws <- function(x) {
  x$niterations
}

#' @importFrom posterior ndraws
#' @export
ndraws.lazydraws <- function(x) {
  x$ndraws
}

#' @importFrom posterior nvariables
#' @export
nvariables.lazydraws <- function(x, ...) {
  length(posterior::variables(x, ...))
}

#' @importFrom posterior chain_ids
#' @export
chain_ids.lazydraws <- function(x) {
  read_index_ids(x, ".chain")
}

#' @importFrom posterior iteration_ids
#' @export
iteration_ids.lazydraws <- function(x) {
  read_index_ids(x, ".iteration")
}

#' @importFrom posterior draw_ids
#' @export
draw_ids.lazydraws <- function(x) {
  read_index_ids(x, ".draw")
}

read_index_ids <- function(x, column) {
  with_connection(x, function(con) {
    column <- quote_columns(con, column)
    ids <- DBI::dbGetQuery(con, paste0(
      "SELECT DISTINCT ", column, " FROM ", source_sql(x, con),
      " ORDER BY ", column
    ))[[1]]
    # Match posterior's integer ID vectors without manufacturing a sequence.
    as.integer(ids)
  })
}

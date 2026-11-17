indices <- c(".chain", ".iteration", ".draw")

check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop(name, " must be TRUE or FALSE.", call. = FALSE)
  }
}

check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(name, " must be a nonempty string.", call. = FALSE)
  }
}

connect_draws <- function() {
  DBI::dbConnect(duckdb::duckdb(shared_home = FALSE),
                config = list(threads = "2", memory_limit = "1GB"))
}

quote_columns <- function(con, x) {
  as.character(DBI::dbQuoteIdentifier(con, x))
}

quote_strings <- function(con, x) {
  as.character(DBI::dbQuoteString(con, x))
}

# Delegate variable expansion, regex matching, and exclusion to posterior using
# a single synthetic draw. Only the column names are needed, never real values.
select_variables <- function(available, variable = NULL, regex = FALSE,
                             exclude = FALSE, scalar = FALSE) {
  if (is.null(variable)) {
    return(available)
  }
  schema <- draws_schema(available)
  posterior::variables(posterior::subset_draws(
    schema, variable = variable, regex = regex, exclude = exclude, scalar = scalar
  ))
}

draws_schema <- function(available) {
  frame <- structure(rep(list(0), length(available)), names = available,
                     class = "data.frame", row.names = 1L)
  posterior::as_draws_df(frame)
}

check_connection <- function(con) {
  if (!DBI::dbIsValid(con)) {
    stop("The DuckDB connection is closed.", call. = FALSE)
  }
  con
}

new_lazy_draws <- function(con) {
  con <- check_connection(con)
  fields <- names(DBI::dbGetQuery(con, "SELECT * FROM draws LIMIT 0"))
  if (!all(indices %in% fields)) {
    stop("Draws must contain .chain, .iteration, and .draw columns.", call. = FALSE)
  }
  counts <- DBI::dbGetQuery(con, paste0(
    'SELECT count(*) AS ndraws, count(DISTINCT ".chain") AS nchains, ',
    'count(DISTINCT ".iteration") AS niterations FROM draws'
  ))
  structure(list(
    con = con, variables = setdiff(fields, indices),
    ndraws = as.numeric(counts$ndraws),
    nchains = as.numeric(counts$nchains),
    niterations = as.numeric(counts$niterations)
  ), class = "lazydraws")
}

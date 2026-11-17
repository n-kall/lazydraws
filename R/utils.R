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

source_sql <- function(x, con) {
  if (is.null(x$con)) {
    paste0("read_parquet(", quote_strings(con, x$source), ")")
  } else {
    as.character(DBI::dbQuoteIdentifier(con, x$source))
  }
}

with_connection <- function(x, fun) {
  con <- x$con
  if (is.null(con)) {
    con <- connect_draws()
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  } else if (!DBI::dbIsValid(con)) {
    stop("The DuckDB connection is closed.", call. = FALSE)
  }
  fun(con)
}

as_lazy_draws <- function(x, table = "draws") {
  if (inherits(x, "lazydraws")) {
    if (!is.null(x$con) && !DBI::dbIsValid(x$con)) {
      stop("The DuckDB connection is closed.", call. = FALSE)
    }
    return(x)
  }
  if (inherits(x, "duckdb_connection")) {
    check_string(table, "table")
    out <- list(con = x, source = table)
  } else {
    check_string(x, "x (Parquet path or DuckDB connection)")
    out <- list(con = NULL, source = normalizePath(path.expand(x), mustWork = TRUE))
  }
  fields <- with_connection(out, function(con) {
    names(DBI::dbGetQuery(con, paste0(
      "SELECT * FROM ", source_sql(out, con), " LIMIT 0"
    )))
  })
  if (!all(indices %in% fields)) {
    stop("Draws must contain .chain, .iteration, and .draw columns.", call. = FALSE)
  }
  out$variables <- setdiff(fields, indices)
  counts <- with_connection(out, function(con) {
    DBI::dbGetQuery(con, paste0(
      'SELECT count(*) AS ndraws, count(DISTINCT ".chain") AS nchains, ',
      'count(DISTINCT ".iteration") AS niterations FROM ', source_sql(out, con)
    ))
  })
  out$ndraws <- as.numeric(counts$ndraws)
  out$nchains <- as.numeric(counts$nchains)
  out$niterations <- as.numeric(counts$niterations)
  class(out) <- "lazydraws"
  out
}

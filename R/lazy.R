#' @importFrom posterior subset_draws
#' @export
subset_draws.lazydraws <- function(x, variable = NULL, iteration = NULL, chain = NULL,
                              draw = NULL, regex = FALSE, unique = TRUE,
                              exclude = FALSE, scalar = FALSE, ...) {
  check_flag(regex, "regex")
  check_flag(unique, "unique")
  check_flag(exclude, "exclude")
  check_flag(scalar, "scalar")
  if (!is.null(draw) && (!is.null(chain) || !is.null(iteration))) {
    stop("Cannot combine draw with chain or iteration.", call. = FALSE)
  }
  x <- as_lazy_draws(x)
  x$variables <- select_variables(x$variables, variable, regex, exclude, scalar)
  selection <- NULL
  if (!is.null(chain) || !is.null(iteration) || !is.null(draw)) {
    selection <- list(
      chain = chain, iteration = iteration, draw = draw,
      unique = unique, exclude = exclude
    )
  }
  fetch_draws(x, selection)
}

fetch_draws <- function(x, selection = NULL) {
  with_connection(x, function(con) {
    source <- source_sql(x, con)
    if (is.null(selection)) {
      columns <- paste(quote_columns(con, c(x$variables, indices)), collapse = ", ")
      frame <- DBI::dbGetQuery(con, paste0(
        "SELECT ", columns, " FROM ", source,
        ' ORDER BY ".chain", ".iteration"'
      ))
      validate_indices(frame)
      return(posterior::as_draws_df(frame))
    }
    frame <- DBI::dbGetQuery(con, paste0(
      'SELECT ".chain", ".iteration", ".draw" FROM ', source,
      ' ORDER BY ".chain", ".iteration"'
    ))
    validate_indices(frame)
    # Keep original keys as a regular variable, so posterior repairs only the
    # visible indices while retaining the keys needed to fetch source values.
    frame$source_draw <- frame$.draw
    index <- posterior::as_draws_df(frame)
    index <- do.call(posterior::subset_draws, c(list(x = index), selection))
    map <- as.data.frame(index)
    map$output_order <- seq_len(nrow(map))
    name <- basename(tempfile("lazydraws_indices_"))
    duckdb::duckdb_register(con, name, map)
    on.exit(duckdb::duckdb_unregister(con, name), add = TRUE)
    values <- if (length(x$variables)) {
      paste0("d.", quote_columns(con, x$variables))
    } else {
      character()
    }
    columns <- paste(c(values, paste0("i.", quote_columns(con, indices))),
                     collapse = ", ")
    frame <- DBI::dbGetQuery(con, paste0(
      "SELECT ", columns, " FROM ", source, " AS d JOIN ",
      quote_columns(con, name), ' AS i ON d.".draw" = i.source_draw',
      " ORDER BY i.output_order"
    ))
    posterior::as_draws_df(frame)
  })
}

validate_indices <- function(x) {
  if (anyNA(x[indices]) || anyDuplicated(x$.draw) ||
      anyDuplicated(x[c(".chain", ".iteration")]) ||
      any(vapply(x[indices], function(v) {
        !is.numeric(v) || any(!is.finite(v) | v < 1 | v != floor(v))
      }, logical(1)))) {
    stop("Draw indices must be positive integers with unique draws and ",
         "unique chain/iteration pairs.", call. = FALSE)
  }
}

#' @export
print.lazydraws <- function(x, ...) {
  closed <- !is.null(x$con) && !DBI::dbIsValid(x$con)
  source <- if (!is.null(x$file)) x$file else x$source
  cat("Lazy posterior draws", if (closed) " (closed)" else "", "\n",
      "Source: ", source, "\n", length(x$variables), " variables\n", sep = "")
  invisible(x)
}

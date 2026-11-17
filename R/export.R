#' Export a CmdStanR fit to Parquet
#'
#' @param fit A CmdStanR MCMC fit whose output CSV files still exist.
#' @param file Destination Parquet path. The parent directory must exist.
#' @param variables Optional character vector of variables to export. Indexed
#'   variables may be selected by their base name. `NULL` selects all variables.
#' @param sampler_diagnostics Include sampler diagnostic columns?
#' @param inc_warmup Include saved warmup? Defaults to `FALSE`.
#' @param overwrite Replace an existing destination after successful export?
#' @param memory_limit DuckDB memory limit, for example `"1GB"`. This is not a
#'   hard limit on total process memory. Temporary spill files use R's temporary
#'   directory, which should be on disk on memory-constrained systems.
#' @param threads Positive integer number of DuckDB worker threads.
#'
#' @details Uses only the fit's public `$output_files()` and `$metadata()`
#'   methods. It does not call `$draws()` or populate the draws cache. DuckDB
#'   handles CSV comments and numeric parsing, then writes a single Zstandard
#'   compressed Parquet file without collecting parameter values into R.
#'
#'   Only completed MCMC runs with matching chain lengths and uncompressed CSV
#'   files are supported. Row counts and missing selected numeric fields are
#'   validated before writing. Chain indices follow CSV file order, and iteration
#'   indices count saved draws after thinning. When warmup is included, indices
#'   start at 1 for warmup and continue into sampling; no warmup flag is added.
#'
#'   The file stores parameter values, optional diagnostics, and `.chain`,
#'   `.iteration`, `.draw`. It does not serialize the fit or store configuration,
#'   adaptation information, timing, initial values, or profiles. Failed exports
#'   leave an existing destination untouched.
#'
#' @return The output path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' cmdstanr_to_parquet_draws(fit, "draws.parquet")
#' drw <- load_draws("draws.parquet")
#' drw |>
#'   posterior::subset_draws(variable = "theta", chain = 1:2) |>
#'   posterior::summarise_draws()
#' close_draws(drw)
#' }
cmdstanr_to_parquet_draws <- function(fit, file, variables = NULL,
                                     sampler_diagnostics = TRUE,
                                     inc_warmup = FALSE, overwrite = FALSE,
                                     memory_limit = "1GB", threads = 2L) {
  if (!inherits(fit, "CmdStanMCMC")) {
    stop("fit must be a CmdStanR MCMC fit.", call. = FALSE)
  }
  check_string(file, "file")
  check_string(memory_limit, "memory_limit")
  check_flag(sampler_diagnostics, "sampler_diagnostics")
  check_flag(inc_warmup, "inc_warmup")
  check_flag(overwrite, "overwrite")
  if (!is.numeric(threads) || length(threads) != 1L || is.na(threads) ||
      threads < 1 || threads != floor(threads)) {
    stop("threads must be a positive integer.", call. = FALSE)
  }
  files <- path.expand(fit$output_files())
  if (!length(files) || any(!file.exists(files)) ||
      any(!grepl("\\.csv$", files, ignore.case = TRUE))) {
    stop("All output files must exist and be uncompressed CSV files.", call. = FALSE)
  }
  files <- normalizePath(files, mustWork = TRUE)
  file <- path.expand(file)
  if (!dir.exists(dirname(file))) {
    stop("The destination directory does not exist.", call. = FALSE)
  }
  if (dir.exists(file) || (file.exists(file) && !overwrite)) {
    stop("Output already exists: ", file, call. = FALSE)
  }
  if (normalizePath(file, mustWork = FALSE) %in% files) {
    stop("Output must not overwrite an input CSV file.", call. = FALSE)
  }
  meta <- fit$metadata()
  if (!identical(meta$method, "sample")) {
    stop("Only MCMC sampling output is supported.", call. = FALSE)
  }
  if (inc_warmup && !isTRUE(meta$save_warmup == 1)) {
    stop("Warmup draws were requested but were not saved.", call. = FALSE)
  }
  warmup <- if (isTRUE(meta$save_warmup == 1)) {
    ceiling(meta$iter_warmup / meta$thin)
  } else {
    0
  }
  sampling <- ceiling(meta$iter_sampling / meta$thin)
  if (length(sampling) != 1L || !is.finite(sampling) || sampling < 1) {
    stop("Export requires post-warmup draws.", call. = FALSE)
  }
  n_keep <- sampling + if (inc_warmup) warmup else 0
  if (n_keep * length(files) > .Machine$integer.max) {
    stop("Too many draws for integer draw indices.", call. = FALSE)
  }
  selected <- if (identical(variables, "")) character() else {
    select_variables(meta$variables, variables)
  }
  if (sampler_diagnostics) {
    selected <- unique(c(selected, meta$sampler_diagnostics))
  }
  if (!length(selected)) {
    stop("Select at least one variable or sampler diagnostic.", call. = FALSE)
  }
  # CmdStan's raw header uses dots for indices; metadata uses posterior names.
  raw <- gsub("]", "", gsub(",", ".", gsub("[", ".", selected, fixed = TRUE),
                            fixed = TRUE), fixed = TRUE)
  con <- connect_draws()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste("SET memory_limit =", quote_strings(con, memory_limit)))
  DBI::dbExecute(con, paste("SET threads =", threads))
  DBI::dbExecute(con, paste("SET temp_directory =", quote_strings(con, tempdir())))
  DBI::dbExecute(con, "SET preserve_insertion_order = true")

  header <- NULL
  queries <- vapply(seq_along(files), function(i) {
    columns <- csv_header(files[i])
    if (is.null(header)) {
      header <<- columns
    } else if (!identical(header, columns)) {
      stop("CSV files have different column headers.", call. = FALSE)
    }
    if (!all(raw %in% columns)) {
      stop("CSV columns do not match the fit's metadata.", call. = FALSE)
    }
    types <- paste(paste0(quote_strings(con, columns), ": 'DOUBLE'"),
                   collapse = ", ")
    # Very wide Stan outputs can exceed DuckDB's default maximum CSV line size.
    line_size <- max(2000000, sum(nchar(columns, type = "bytes")) +
                       length(columns), 32 * length(columns))
    reader <- paste0(
      "read_csv(", quote_strings(con, files[i]),
      ", header = true, delim = ',', comment = '#', auto_detect = false, ",
      "columns = {", types, "}, parallel = false, max_line_size = ",
      format(line_size, scientific = FALSE), ", buffer_size = ",
      format(4 * line_size, scientific = FALSE), ")"
    )
    counts <- paste0("count(", quote_columns(con, raw), ")")
    counts <- DBI::dbGetQuery(con, paste0(
      "SELECT count(*) AS n_rows, ", paste(counts, collapse = ", "),
      " FROM ", reader
    ))
    n_rows <- as.numeric(counts[[1]][1])
    if (n_rows != warmup + sampling) {
      stop("CSV does not contain the expected number of saved draws; ",
           "only completed chains are supported: ", files[i], call. = FALSE)
    }
    if (any(vapply(counts[-1], as.numeric, numeric(1)) != n_rows)) {
      stop("CSV contains missing selected numeric fields: ", files[i], call. = FALSE)
    }
    values <- paste(paste(quote_columns(con, raw), "AS",
                         quote_columns(con, selected)), collapse = ", ")
    skip <- if (inc_warmup) 0 else warmup
    paste0(
      "SELECT ", values, ", CAST(", i, ' AS INTEGER) AS ".chain", ',
      'CAST(row_number() OVER () AS INTEGER) AS ".iteration", ',
      "CAST(row_number() OVER () + ", (i - 1) * n_keep,
      ' AS INTEGER) AS ".draw" FROM (SELECT * FROM ', reader,
      " OFFSET ", skip, ") AS sampling_rows"
    )
  }, character(1))
  temporary <- tempfile(".lazydraws-", tmpdir = dirname(file),
                        fileext = ".parquet")
  on.exit(unlink(temporary), add = TRUE)
  DBI::dbExecute(con, paste0(
    "COPY (", paste(queries, collapse = " UNION ALL "), ") TO ",
    quote_strings(con, temporary),
    " (FORMAT PARQUET, COMPRESSION ZSTD, ROW_GROUP_SIZE 2048)"
  ))
  if (!file.rename(temporary, file)) {
    stop("Could not move the completed Parquet file to: ", file, call. = FALSE)
  }
  invisible(file)
}

csv_header <- function(file) {
  con <- file(file, open = "rt")
  on.exit(close(con), add = TRUE)
  repeat {
    line <- readLines(con, n = 1L, warn = FALSE)
    if (!length(line)) {
      stop("CSV has no header: ", file, call. = FALSE)
    }
    if (nzchar(trimws(line)) && !startsWith(trimws(line), "#")) {
      return(strsplit(line, ",", fixed = TRUE)[[1]])
    }
  }
}

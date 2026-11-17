unsupported_draws_operation <- function(operation) {
  stop(
    "`", operation, "()` does not support `lazydraws` objects.\n",
    "Use `posterior::subset_draws()` first to load the draws you need, ",
    "then apply `", operation, "()` to the resulting `draws_df`.",
    call. = FALSE
  )
}

#' @importFrom posterior mutate_variables
#' @export
mutate_variables.lazydraws <- function(.x, ...) {
  unsupported_draws_operation("mutate_variables")
}

#' @importFrom posterior rename_variables
#' @export
rename_variables.lazydraws <- function(.x, ...) {
  unsupported_draws_operation("rename_variables")
}

#' @importFrom posterior variables<-
#' @export
`variables<-.lazydraws` <- function(x, ..., value) {
  unsupported_draws_operation("variables<-")
}

#' @importFrom posterior thin_draws
#' @export
thin_draws.lazydraws <- function(x, thin = NULL, ...) {
  unsupported_draws_operation("thin_draws")
}

#' @importFrom posterior bind_draws
#' @export
bind_draws.lazydraws <- function(x, ...) {
  unsupported_draws_operation("bind_draws")
}

#' @importFrom posterior merge_chains
#' @export
merge_chains.lazydraws <- function(x, ...) {
  unsupported_draws_operation("merge_chains")
}

#' @importFrom posterior split_chains
#' @export
split_chains.lazydraws <- function(x, ...) {
  unsupported_draws_operation("split_chains")
}

#' @importFrom posterior order_draws
#' @export
order_draws.lazydraws <- function(x, ...) {
  unsupported_draws_operation("order_draws")
}

#' @importFrom posterior repair_draws
#' @export
repair_draws.lazydraws <- function(x, order = TRUE, ...) {
  unsupported_draws_operation("repair_draws")
}

#' @importFrom posterior resample_draws
#' @export
resample_draws.lazydraws <- function(x, ...) {
  unsupported_draws_operation("resample_draws")
}

#' @importFrom posterior weight_draws
#' @export
weight_draws.lazydraws <- function(x, weights, ...) {
  unsupported_draws_operation("weight_draws")
}

#' @importFrom posterior summarise_draws
#' @export
summarise_draws.lazydraws <- function(.x, ...) {
  unsupported_draws_operation("summarise_draws")
}

#' @importFrom posterior extract_variable
#' @export
extract_variable.lazydraws <- function(x, variable, ...) {
  unsupported_draws_operation("extract_variable")
}

#' @importFrom posterior as_draws
#' @export
as_draws.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws")
}

#' @importFrom posterior as_draws_array
#' @export
as_draws_array.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws_array")
}

#' @importFrom posterior as_draws_df
#' @export
as_draws_df.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws_df")
}

#' @importFrom posterior as_draws_list
#' @export
as_draws_list.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws_list")
}

#' @importFrom posterior as_draws_matrix
#' @export
as_draws_matrix.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws_matrix")
}

#' @importFrom posterior as_draws_rvars
#' @export
as_draws_rvars.lazydraws <- function(x, ...) {
  unsupported_draws_operation("as_draws_rvars")
}

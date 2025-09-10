#' Build a single intersectional stratum factor
#' @param data A data.frame
#' @param vars Character vector of column names to combine (e.g. c("ethnicity","gender","class"))
#' @param name Name of the new factor column. Default ".stratum".
#' @param drop Drop unused levels. Default TRUE.
#' @return The input data with an added factor column `name`.
#' @export
intersection_strata <- function(data, vars, name = ".stratum", drop = TRUE) {
  stopifnot(is.data.frame(data), length(vars) >= 1L, all(vars %in% names(data)))
  f <- interaction(data[vars], drop = drop, sep = "_", lex.order = TRUE)
  data[[name]] <- base::droplevels(factor(f))  # update
  data
}

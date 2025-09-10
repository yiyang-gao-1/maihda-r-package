#' Quick caterpillar plot of stratum effects
#' @param strata_df Output of predict_strata().
#' @param response If TRUE and `strata_df` has prob columns, plot probabilities; otherwise plot eta.
#' @export
plot_strata <- function(strata_df, response = FALSE) {
  stopifnot(is.data.frame(strata_df), ".stratum" %in% names(strata_df))
  if (isTRUE(response) && all(c("prob_lwr","prob","prob_upr") %in% names(strata_df))) {
    df <- dplyr::arrange(strata_df, prob)
    ggplot2::ggplot(df, ggplot2::aes(x = reorder(.stratum, prob), y = prob)) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = prob_lwr, ymax = prob_upr)) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Stratum", y = "Predicted probability") +
      ggplot2::theme_minimal()
  } else {
    df <- dplyr::arrange(strata_df, eta)
    ggplot2::ggplot(df, ggplot2::aes(x = reorder(.stratum, eta), y = eta)) +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = lwr, ymax = upr)) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Stratum", y = "Effect (link scale)") +
      ggplot2::theme_minimal()
  }
}

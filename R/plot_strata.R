#' Quick caterpillar plot of stratum effects
#' @param strata_df Output of predict_strata().
#' @param response If TRUE and `strata_df` has prob columns, plot probabilities; otherwise plot eta.
#' @importFrom stats reorder
#' @importFrom rlang .data
#' @export
plot_strata <- function(strata_df, response = FALSE) {
  stopifnot(is.data.frame(strata_df), ".stratum" %in% names(strata_df))
  
  if (isTRUE(response) && all(c("prob_lwr","prob","prob_upr") %in% names(strata_df))) {
    df <- dplyr::arrange(strata_df, rlang::.data$prob)
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = stats::reorder(rlang::.data$.stratum, rlang::.data$prob),
                   y = rlang::.data$prob)
    ) +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = rlang::.data$prob_lwr, ymax = rlang::.data$prob_upr)
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Stratum", y = "Predicted probability") +
      ggplot2::theme_minimal()
  } else {
    df <- dplyr::arrange(strata_df, rlang::.data$eta)
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = stats::reorder(rlang::.data$.stratum, rlang::.data$eta),
                   y = rlang::.data$eta)
    ) +
      ggplot2::geom_pointrange(
        ggplot2::aes(ymin = rlang::.data$lwr, ymax = rlang::.data$upr)
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Stratum", y = "Effect (link scale)") +
      ggplot2::theme_minimal()
  }
}

#' Simulate a simple MAIHDA dataset (logistic)
#' @param n Number of individuals.
#' @param levels A named list of factor levels for each identity variable.
#' @param intercept Fixed intercept on logit scale.
#' @param sd_u SD of between-stratum random intercepts.
#' @param seed Optional seed.
#' @return A data.frame with identity vars, stratum, and binary outcome y.
#' @export
simulate_maihda_data <- function(n = 2000,
                                 levels = list(
                                   ethnicity = c("A","B","C"),
                                   gender    = c("F","M"),
                                   class     = c("Low","High")
                                 ),
                                 intercept = -1,
                                 sd_u = 0.6,
                                 seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  # Draw identities
  df <- as.data.frame(lapply(levels, function(vals) sample(vals, n, TRUE)))
  names(df) <- names(levels)
  
  # Build strata + random intercepts
  df <- intersection_strata(df, names(levels), name = ".stratum")
  J  <- nlevels(df$.stratum)
  u  <- stats::rnorm(J, 0, sd_u)
  
  eta <- intercept + u[as.integer(df$.stratum)]
  p   <- 1/(1 + exp(-eta))
  df$y <- stats::rbinom(n, 1, p)
  df
}

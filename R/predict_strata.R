#' Stratum-level random effects with approximate CIs
#' @param object A "maihda" object.
#' @param level Confidence level for intervals (default 0.95).
#' @param type "link" or "response" (response uses logistic transform for binomial).
#' @param include_fixed_intercept Whether to add the fixed intercept to random effects (default TRUE).
#' @return A tibble with stratum, estimate, se, lwr, upr and (if binomial+response) prob columns.
#' @export
predict_strata <- function(object, level = 0.95,
                           type = c("link","response"),
                           include_fixed_intercept = TRUE) {
  stopifnot(inherits(object, "maihda"))
  type <- match.arg(type)
  
  re <- lme4::ranef(object$fit, condVar = TRUE)[[object$strata]]
  est <- re[,"(Intercept)"]
  pv  <- attr(re, "postVar")  # 1 x 1 x J array for random intercepts
  se  <- sqrt(sapply(seq_len(nrow(re)), function(i) pv[,,i]))
  z   <- stats::qnorm(0.5 + level/2)
  
  intercept <- tryCatch(lme4::fixef(object$fit)[["(Intercept)"]], error = function(e) 0)
  eta <- if (isTRUE(include_fixed_intercept)) as.numeric(intercept + est) else as.numeric(est)
  lwr <- eta - z*se
  upr <- eta + z*se
  
  fam <- tryCatch(insight::model_info(object$fit)$family, error = function(e) NA_character_)
  out <- dplyr::tibble(
    .stratum = rownames(re),
    eta = eta,
    se  = se,
    lwr = lwr,
    upr = upr
  )
  
  if (identical(type, "response") && identical(fam, "binomial")) {
    out <- dplyr::mutate(out,
                         prob = stats::plogis(eta),
                         prob_lwr = stats::plogis(lwr),
                         prob_upr = stats::plogis(upr)
    )
  }
  out
}

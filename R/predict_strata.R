#' Stratum-level predictions and (optional) additive vs. excess decomposition
#' @param object A "maihda" object (typically the Additive model B).
#' @param level Confidence level for intervals (default 0.95).
#' @param type "link" or "response" (response uses logistic transform for binomial).
#' @param include_fixed_intercept Whether to add the fixed intercept to random effects (default TRUE).
#' @param decompose If TRUE, also compute additive (fixed-only) and excess (random) parts per stratum.
#' @param additive_model Optional "maihda" object used to compute the additive (fixed-only) part;
#'   if NULL, use `object` itself.
#' @return A tibble with stratum-level estimates. When `decompose=TRUE`, includes
#'   eta_add, eta_excess, eta_total, and their response-scale counterparts (prob_add, prob_total).
#'   Intervals are provided for the random-excess and total (via RE uncertainty).
#' @export
predict_strata <- function(object, level = 0.95,
                           type = c("link","response"),
                           include_fixed_intercept = TRUE,
                           decompose = FALSE,
                           additive_model = NULL) {
  stopifnot(inherits(object, "maihda"))
  type <- match.arg(type)
  
  # random effects (lme4-style)
  re <- lme4::ranef(object$fit, condVar = TRUE)[[object$strata]]
  est <- re[,"(Intercept)"]
  pv  <- attr(re, "postVar")  # 1 x 1 x J array
  se  <- sqrt(sapply(seq_len(nrow(re)), function(i) pv[,,i]))
  z   <- stats::qnorm(0.5 + level/2)
  
  intercept <- tryCatch(lme4::fixef(object$fit)[["(Intercept)"]], error = function(e) 0)
  eta_excess <- as.numeric(est)
  excess_lwr <- eta_excess - z*se
  excess_upr <- eta_excess + z*se
  
  fam <- tryCatch(insight::model_info(object$fit)$family, error = function(e) NA_character_)
  
  # base output (no decompose)
  eta <- if (isTRUE(include_fixed_intercept)) as.numeric(intercept + eta_excess) else as.numeric(eta_excess)
  lwr <- if (isTRUE(include_fixed_intercept)) as.numeric(intercept + excess_lwr) else excess_lwr
  upr <- if (isTRUE(include_fixed_intercept)) as.numeric(intercept + excess_upr) else excess_upr
  
  out <- dplyr::tibble(
    .stratum = rownames(re),
    eta = eta,
    lwr = lwr,
    upr = upr
  )
  
  # Add response scale if logistic
  if (identical(type, "response") && identical(fam, "binomial")) {
    out <- dplyr::mutate(out,
                         prob = stats::plogis(.data$eta),
                         prob_lwr = stats::plogis(.data$lwr),
                         prob_upr = stats::plogis(.data$upr)
    )
  }
  
  if (!decompose) return(out)
  
  # ----- decomposition: additive (fixed-only) + excess (random) = total -----
  add_src <- if (is.null(additive_model)) object else additive_model
  stopifnot(inherits(add_src, "maihda"))
  
  # representative rows per stratum from the additive model's data
  df_add <- add_src$data
  key <- add_src$strata
  reps <- df_add[!duplicated(df_add[[key]]), , drop = FALSE]
  # fixed-only predictions on link scale
  eta_add <- as.numeric(stats::predict(add_src$fit, newdata = reps, re.form = NA, type = "link"))
  
  # Align by stratum level order
  add_map <- setNames(eta_add, reps[[key]])
  eta_add <- as.numeric(add_map[out$.stratum])
  
  # total = additive + excess (random). Intervals for total from RE part only.
  eta_total <- eta_add + eta_excess
  total_lwr <- eta_add + excess_lwr
  total_upr <- eta_add + excess_upr
  
  decomp <- dplyr::tibble(
    .stratum   = out$.stratum,
    eta_add    = eta_add,
    eta_excess = eta_excess,
    eta_total  = eta_total,
    excess_lwr = excess_lwr,
    excess_upr = excess_upr,
    total_lwr  = total_lwr,
    total_upr  = total_upr
  )
  
  if (identical(type, "response") && identical(fam, "binomial")) {
    decomp <- dplyr::mutate(decomp,
                            prob_add   = stats::plogis(.data$eta_add),
                            prob_total = stats::plogis(.data$eta_total),
                            prob_lwr_total = stats::plogis(.data$total_lwr),
                            prob_upr_total = stats::plogis(.data$total_upr)
    )
  }
  
  dplyr::left_join(out, decomp, by = ".stratum")
}

#' Summarise MAIHDA model metrics
#' @param object A "maihda" object.
#' @param null Optional "maihda" null model for PCV.
#' @return A one-row tibble with key metrics.
#' @export
maihda_metrics <- function(object, null = NULL) {
  stopifnot(inherits(object, "maihda"))
  vc <- as.data.frame(lme4::VarCorr(object$fit))
  var_u <- vc$vcov[vc$grp == object$strata][1]
  fam <- tryCatch(insight::model_info(object$fit)$family, error = function(e) NA_character_)
  var_e <- if (identical(fam, "binomial")) NA_real_ else
    tryCatch(stats::sigma(object$fit)^2, error = function(e) NA_real_)
  tib <- dplyr::tibble(
    model      = NA_character_,
    engine     = object$engine,
    family     = fam,
    N          = nrow(object$data),
    J          = nlevels(object$data[[object$strata]]),
    var_u      = var_u,
    var_e      = var_e,
    VPC        = vpc(object),
    PCV        = if (!is.null(null)) pcv(null, object) else NA_real_,
    MOR        = if (identical(fam, "binomial")) mor(object) else NA_real_,
    AUC        = if (identical(fam, "binomial")) suppressWarnings(tryCatch(auc_maihda(object), error = function(e) NA_real_)) else NA_real_
  )
  tib
}

#' Fit the standard MAIHDA sequence (Null → Additive → +Covariates)
#'
#' @param response Character scalar: name of outcome column (e.g., "y").
#' @param data Data frame.
#' @param strata Character vector of variables to build intersectional strata.
#' @param family Model family, e.g. stats::binomial() or stats::gaussian().
#' @param main_effects Character vector of fixed main-effects (e.g., c("ethnicity","gender","class")).
#' @param covariates Optional character vector of additional covariates/exposures.
#' @param engine "lme4","glmmTMB","brms".
#' @param ... Passed to underlying engine.
#' @return A list with elements: models (list), metrics (tibble).
#' @export
maihda_sequence <- function(response, data, strata,
                            family = stats::gaussian(),
                            main_effects = NULL,
                            covariates = NULL,
                            engine = c("lme4","glmmTMB","brms"), ...) {
  engine <- match.arg(engine)
  stopifnot(is.character(response), length(response) == 1L, response %in% names(data))
  
  # Build fixed-part formulas
  fixed_A <- "1"
  fixed_B <- if (length(main_effects)) paste(main_effects, collapse = " + ") else NULL
  fixed_C <- if (length(covariates))  paste(c(collapse = " + ", covariates), collapse = " + ") else NULL
  
  fA <- stats::as.formula(paste(response, "~", fixed_A))
  mA <- maihda_fit(fA, data, strata = strata, family = family, engine = engine, ...)
  
  models <- list(A = mA)
  tab    <- maihda_metrics(mA); tab$model <- "A: Null"
  
  if (!is.null(fixed_B)) {
    fB <- stats::as.formula(paste(response, "~", fixed_B))
    mB <- maihda_fit(fB, data, strata = strata, family = family, engine = engine, ...)
    mb <- maihda_metrics(mB, null = mA); mb$model <- "B: Additive main effects"
    models$B <- mB; tab <- dplyr::bind_rows(tab, mb)
    if (!is.null(fixed_C)) {
      fC <- stats::as.formula(paste(response, "~", paste(c(fixed_B, fixed_C), collapse = " + ")))
      mC <- maihda_fit(fC, data, strata = strata, family = family, engine = engine, ...)
      mc <- maihda_metrics(mC, null = mA); mc$model <- "C: Additive + covariates"
      models$C <- mC; tab <- dplyr::bind_rows(tab, mc)
    }
  }
  
  structure(list(models = models, metrics = tab), class = "maihda_sequence")
}

#' @export
print.maihda_sequence <- function(x, ...) {
  cat("# MAIHDA sequence\n\n")
  print(x$metrics, n = nrow(x$metrics))
  invisible(x)
}

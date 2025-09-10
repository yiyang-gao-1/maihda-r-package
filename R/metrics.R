#' Variance Partition Coefficient (VPC/ICC)
#' @param object A "maihda" object.
#' @export
vpc <- function(object) {
  stopifnot(inherits(object, "maihda"))
  vc <- as.data.frame(lme4::VarCorr(object$fit))   # ← update
  var_u <- vc$vcov[vc$grp == object$strata][1]
  
  fam <- tryCatch(insight::model_info(object$fit)$family, error = function(e) NA_character_)
  if (identical(fam, "binomial")) {
    var_u / (var_u + (pi^2)/3)
  } else {
    var_e <- tryCatch(stats::sigma(object$fit)^2, error = function(e) NA_real_)
    var_u / (var_u + var_e)
  }
}

#' Proportional Change in Variance (between-stratum)
#' @param null_obj MAIHDA null model (usually y ~ 1 + (1|stratum))
#' @param model_obj A richer MAIHDA model (e.g., + main effects)
#' @export
pcv <- function(null_obj, model_obj) {
  stopifnot(inherits(null_obj, "maihda"), inherits(model_obj, "maihda"))
  vc0 <- as.data.frame(lme4::VarCorr(null_obj$fit))   # ← update
  v0  <- vc0$vcov[vc0$grp == null_obj$strata][1]
  vc1 <- as.data.frame(lme4::VarCorr(model_obj$fit))  # ← update
  v1  <- vc1$vcov[vc1$grp == model_obj$strata][1]
  (v0 - v1) / v0
}

#' Median Odds Ratio (MOR) for logistic MAIHDA
#' @param object A "maihda" object.
#' @return Numeric MOR.
#' @export
mor <- function(object) {
  stopifnot(inherits(object, "maihda"))
  vc <- as.data.frame(lme4::VarCorr(object$fit))  # ← update
  var_u <- vc$vcov[vc$grp == object$strata][1]
  exp(sqrt(2 * var_u) * stats::qnorm(0.75))
}


#' AUC for logistic MAIHDA (requires pROC)
#' @param object A "maihda" object fit with binomial family.
#' @return Numeric AUC.
#' @export
auc_maihda <- function(object) {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required for auc_maihda(). Please install it.")
  }
  fam <- tryCatch(insight::model_info(object$fit)$family, error = function(e) NA_character_)
  if (!identical(fam, "binomial")) stop("auc_maihda() only applies to binomial models.")
  pr <- stats::predict(object$fit, type = "response")
  y  <- insight::get_response(object$fit)
  as.numeric(pROC::roc(y, pr, quiet = TRUE)$auc)
}

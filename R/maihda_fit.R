#' Fit a MAIHDA model (random intercept for intersectional strata, optional cross-context)
#'
#' @param formula Model formula without random terms (e.g. y ~ 1 or y ~ x1 + x2).
#' @param data A data.frame.
#' @param strata Character vector that defines the intersectional stratum.
#' @param family stats::gaussian() or stats::binomial(), etc.
#' @param engine "lme4","glmmTMB","brms".
#' @param contexts Optional character vector of additional grouping factors for cross-classified structures,
#'   e.g. c("region") or c("school","area"). Each will get a random intercept `(1|context)`.
#' @param ... Passed to the underlying model function.
#' @export
maihda_fit <- function(formula, data, strata,
                       family = stats::gaussian(),
                       engine = c("lme4","glmmTMB","brms"),
                       contexts = NULL,
                       ...) {
  engine <- match.arg(engine)
  data <- intersection_strata(data, strata, name = ".stratum")
  
  # sanity checks
  if (length(contexts)) {
    stopifnot(all(contexts %in% names(data)))
  }
  
  # build random part string
  rand_terms <- c("(1|.stratum)")
  if (length(contexts)) rand_terms <- c(rand_terms, sprintf("(1|%s)", contexts))
  rand_string <- paste(rand_terms, collapse = " + ")
  
  # full formula
  full_formula <- stats::as.formula(paste(deparse(formula), "+", rand_string))
  
  fit <- switch(engine,
                lme4 = {
                  if (identical(family, stats::gaussian()))
                    lme4::lmer(full_formula, data = data, ...)
                  else
                    lme4::glmer(full_formula, data = data, family = family, ...)
                },
                glmmTMB = glmmTMB::glmmTMB(full_formula, data = data, family = family, ...),
                brms    = brms::brm(full_formula, data = data, family = family, ...)
  )
  
  structure(list(
    fit = fit,
    engine = engine,
    data = data,
    strata = ".stratum",
    formula = full_formula,
    contexts = contexts,
    call = match.call()
  ), class = "maihda")
}

#' Fit a MAIHDA model (random intercept for intersectional strata)
#'
#' @param formula Model formula without the random stratum term (e.g. y ~ 1 or y ~ x1 + x2).
#' @param data A data.frame.
#' @param strata Character vector of variables to build the intersectional strata.
#' @param family A model family, default stats::gaussian().
#' @param engine One of "lme4","glmmTMB","brms".
#' @param ... Passed to the underlying model function.
#' @return An object of class "maihda".
#' @export
maihda_fit <- function(formula, data, strata, family = stats::gaussian(),
                       engine = c("lme4","glmmTMB","brms"), ...) {
  engine <- match.arg(engine)
  data <- intersection_strata(data, strata, name = ".stratum")
  
  rand <- stats::reformulate("(1|.stratum)")
  full_formula <- stats::update(formula, paste(". ~ . +", as.character(rand)[2]))
  
  fit <- switch(engine,
                lme4 = {
                  if (identical(family, stats::gaussian()))
                    lme4::lmer(full_formula, data = data, ...)
                  else
                    lme4::glmer(full_formula, data = data, family = family, ...)
                },
                glmmTMB = {
                  glmmTMB::glmmTMB(full_formula, data = data, family = family, ...)
                },
                brms = {
                  brms::brm(full_formula, data = data, family = family, ...)
                }
  )
  
  structure(list(
    fit = fit,
    engine = engine,
    data = data,
    strata = ".stratum",
    formula = full_formula,
    call = match.call()
  ), class = "maihda")
}

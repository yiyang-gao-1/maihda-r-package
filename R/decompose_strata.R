#' Decompose stratum predictions into additive (fixed) and excess (random)
#'
#' Use an ADDITIVE MAIHDA model (e.g., `y ~ main_effects + (1|.stratum) + (1|region)` if contexts are present) ...
#' - additive_eta: fixed-part prediction by stratum (re.form = NA, link scale)
#' - excess_eta  : random intercept for the stratum (BLUP on link scale)
#' - total_eta   : additive_eta + excess_eta
#' And (optionally) probabilities on response scale.
#'
#' @param object_additive A "maihda" object for the additive model (Model B).
#' @param type "link" or "response". On response scale, excess is reported as
#'   a difference: total_prob - additive_prob (nonlinear).
#' @return tibble with .stratum, additive_eta/excess_eta/total_eta and (if response) prob columns.
#' @export
decompose_strata <- function(object_additive, type = c("link","response")) {
  stopifnot(inherits(object_additive, "maihda"))
  type <- match.arg(type)
  
  # representative row per stratum
  df <- object_additive$data
  reps <- dplyr::slice_head(dplyr::group_by(df, .data$.stratum), n = 1L)
  # fixed-part prediction by stratum (no random effects)
  additive_eta <- as.numeric(stats::predict(object_additive$fit, newdata = reps, re.form = NA, type = "link"))
  
  # random intercepts (excess)
  re <- lme4::ranef(object_additive$fit, condVar = FALSE)[[object_additive$strata]]
  excess_eta <- as.numeric(re[,"(Intercept)"])
  
  total_eta <- additive_eta + excess_eta
  
  out <- dplyr::tibble(
    .stratum = reps$.stratum,
    additive_eta = additive_eta,
    excess_eta   = excess_eta,
    total_eta    = total_eta
  )
  
  fam <- tryCatch(insight::model_info(object_additive$fit)$family, error = function(e) NA_character_)
  if (identical(type, "response") && identical(fam, "binomial")) {
    additive_prob <- stats::plogis(additive_eta)
    total_prob    <- stats::plogis(total_eta)
    excess_prob   <- total_prob - additive_prob  # not strictly additive but intuitive
    out <- dplyr::bind_cols(out, dplyr::tibble(
      additive_prob = additive_prob,
      excess_prob   = excess_prob,
      total_prob    = total_prob
    ))
  }
  out
}

#' Stacked decomposition plot (additive vs. excess) by stratum
#'
#' Draw a segment from the additive prediction to the total prediction for each stratum.
#' Works on either link or response scale, and accepts both naming schemes:
#' - response: either (`total_prob`, `additive_prob`, optionally `excess_prob`) or (`prob_total`, `prob_add`)
#' - link    : either (`total_eta`,  `additive_eta`,  optionally `excess_eta`)  or (`eta_total`,  `eta_add`)
#' Optional intervals: `total_lwr`/`total_upr` (link) or `prob_lwr_total`/`prob_upr_total` (response).
#'
#' @param decomp_df Output of decompose_strata().
#' @param scale "link" or "response".
#' @param order_by Which component to order by ("total","excess","additive").
#' @param top_n Optional. If set, show top_n strata by the order_by magnitude.
#' @importFrom rlang .data
#' @export
plot_strata_decomp <- function(decomp_df, scale = c("link","response"),
                               order_by = c("total","excess","additive"),
                               top_n = NULL) {
  scale <- match.arg(scale)
  order_by <- match.arg(order_by)
  stopifnot(is.data.frame(decomp_df), ".stratum" %in% names(decomp_df))
  
  # ---- Pick columns (support both naming styles)
  if (identical(scale, "response")) {
    if (all(c("total_prob","additive_prob") %in% names(decomp_df))) {
      total <- decomp_df$total_prob
      add   <- decomp_df$additive_prob
      exc   <- if ("excess_prob" %in% names(decomp_df)) decomp_df$excess_prob else (total - add)
      lwr   <- if ("prob_lwr_total" %in% names(decomp_df)) decomp_df$prob_lwr_total else NA_real_
      upr   <- if ("prob_upr_total" %in% names(decomp_df)) decomp_df$prob_upr_total else NA_real_
    } else if (all(c("prob_total","prob_add") %in% names(decomp_df))) {
      total <- decomp_df$prob_total
      add   <- decomp_df$prob_add
      exc   <- if ("excess_prob" %in% names(decomp_df)) decomp_df$excess_prob else (total - add)
      lwr   <- if ("prob_lwr_total" %in% names(decomp_df)) decomp_df$prob_lwr_total else NA_real_
      upr   <- if ("prob_upr_total" %in% names(decomp_df)) decomp_df$prob_upr_total else NA_real_
    } else {
      stop("Response-scale columns not found in decomp_df.")
    }
  } else {
    if (all(c("total_eta","additive_eta") %in% names(decomp_df))) {
      total <- decomp_df$total_eta
      add   <- decomp_df$additive_eta
      exc   <- if ("excess_eta" %in% names(decomp_df)) decomp_df$excess_eta else (total - add)
      lwr   <- if ("total_lwr" %in% names(decomp_df)) decomp_df$total_lwr else NA_real_
      upr   <- if ("total_upr" %in% names(decomp_df)) decomp_df$total_upr else NA_real_
    } else if (all(c("eta_total","eta_add") %in% names(decomp_df))) {
      total <- decomp_df$eta_total
      add   <- decomp_df$eta_add
      exc   <- if ("excess_eta" %in% names(decomp_df)) decomp_df$excess_eta else (total - add)
      lwr   <- if ("total_lwr" %in% names(decomp_df)) decomp_df$total_lwr else NA_real_
      upr   <- if ("total_upr" %in% names(decomp_df)) decomp_df$total_upr else NA_real_
    } else {
      stop("Link-scale columns not found in decomp_df.")
    }
  }
  
  # ---- Standardise names (avoid ..var..)
  df <- decomp_df
  df$add_v    <- add
  df$total_v  <- total
  df$excess_v <- exc
  df$lwr_v    <- lwr
  df$upr_v    <- upr
  
  # ordering + optional top_n
  ord_col <- switch(order_by, total = "total_v", excess = "excess_v", additive = "add_v")
  df <- df[order(abs(df[[ord_col]]), decreasing = TRUE), , drop = FALSE]
  if (!is.null(top_n)) df <- utils::head(df, top_n)
  
  # lock the displayed order on y axis
  df$.stratum <- factor(df$.stratum, levels = df$.stratum)
  
  # ---- Build plot
  p <- ggplot2::ggplot(df, ggplot2::aes(y = .data$.stratum)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$add_v, xend = .data$total_v, yend = .data$.stratum)
    ) +
    ggplot2::geom_point(ggplot2::aes(x = .data$add_v), shape = 1)
  
  has_int <- !all(is.na(df$lwr_v)) && !all(is.na(df$upr_v))
  if (has_int) {
    p <- p + ggplot2::geom_pointrange(
      ggplot2::aes(x = .data$total_v, xmin = .data$lwr_v, xmax = .data$upr_v),
      size = 0.3
    )
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(x = .data$total_v))
  }
  
  p +
    ggplot2::coord_flip() +
    ggplot2::labs(
      x = if (identical(scale,"response")) "Probability (additive → total)" else "Link (additive → total)",
      y = "Stratum"
    ) +
    ggplot2::theme_minimal()
}

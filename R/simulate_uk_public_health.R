#' Simulate a UK-like public health dataset for MAIHDA with region context
#'
#' Outcome is GHQ-12 caseness-like (binary), with intersection strata and a region random effect.
#' @param n Number of individuals.
#' @param seed Optional seed.
#' @return data.frame with ethnicity, sex, income_q, region, and y (0/1). `.stratum` will be added by maihda_fit().
#' @export
simulate_uk_public_health <- function(n = 6000, seed = 1) {
  if (!is.null(seed)) set.seed(seed)
  
  ethnicity_levels <- c("White British","Other White","Indian","Pakistani","Bangladeshi",
                        "Black African","Black Caribbean","Chinese","Mixed","Other")
  sex_levels <- c("Female","Male")
  income_q_levels <- paste0("Q", 1:5) # equivalised income quintile
  region_levels <- c("NE","NW","YH","EM","WM","E","LON","SE","SW","WAL","SCT","NI")
  
  ethnicity <- sample(ethnicity_levels, n, TRUE,
                      prob = c(0.77,0.06,0.03,0.02,0.01,0.02,0.01,0.01,0.04,0.03))
  sex      <- sample(sex_levels, n, TRUE)
  income_q <- sample(income_q_levels, n, TRUE, prob = c(0.18,0.20,0.21,0.21,0.20))
  region   <- sample(region_levels, n, TRUE, prob = c(0.04,0.11,0.08,0.07,0.09,0.09,0.13,0.15,0.09,0.05,0.08,0.02))
  
  df <- data.frame(ethnicity, sex, income_q, region, stringsAsFactors = TRUE)
  
  # random effects（logit）：stratum & region
  tmp <- intersection_strata(df, c("ethnicity","sex","income_q"), name = ".stratum")
  
  J  <- nlevels(tmp$.stratum)
  K  <- nlevels(tmp$region)
  u_stratum <- stats::rnorm(J,  0, 0.50)
  u_region  <- stats::rnorm(K,  0, 0.25)
  
  fe <- -1.2 +
    0.35 * (tmp$income_q %in% c("Q1")) + 0.15 * (tmp$income_q %in% c("Q2")) -
    0.10 * (tmp$income_q %in% c("Q5")) +
    0.10 * (tmp$sex == "Female") +
    0.20 * (tmp$ethnicity %in% c("Pakistani","Bangladeshi")) +
    0.10 * (tmp$ethnicity %in% c("Black African","Black Caribbean")) -
    0.10 * (tmp$ethnicity %in% c("Chinese"))
  
  eta <- fe + u_stratum[as.integer(tmp$.stratum)] + u_region[as.integer(tmp$region)]
  p   <- 1/(1 + exp(-eta))
  df$y <- stats::rbinom(n, 1, p)
  
  df
}

test_that("maihda_fit runs and returns class 'maihda'", {
  set.seed(1)
  n <- 2000
  ethnicity <- sample(c("A","B","C"), n, replace = TRUE)
  gender    <- sample(c("F","M"),     n, replace = TRUE)
  class     <- sample(c("Low","High"),n, replace = TRUE)
  
  df <- data.frame(ethnicity, gender, class)
  # Random intercept by stratum
  str <- interaction(df[c("ethnicity","gender","class")], drop = TRUE)
  J <- nlevels(str)
  u <- rnorm(J, 0, 0.6)
  eta <- -1 + u[as.integer(str)]
  p   <- 1/(1 + exp(-eta))
  df$y <- rbinom(n, 1, p)
  
  mA <- maihda_fit(y ~ 1, df, strata = c("ethnicity","gender","class"),
                   family = binomial(), engine = "lme4")
  expect_s3_class(mA, "maihda")
})

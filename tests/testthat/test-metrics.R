test_that("VPC/PCV/MOR behave sensibly", {
  set.seed(2)
  n <- 2500
  ethnicity <- sample(LETTERS[1:4], n, replace = TRUE)
  gender    <- sample(c("F","M"),   n, replace = TRUE)
  class     <- sample(c("Low","High"), n, replace = TRUE)
  
  df <- data.frame(ethnicity, gender, class)
  str <- interaction(df[c("ethnicity","gender","class")], drop = TRUE)
  J <- nlevels(str)
  u <- rnorm(J, 0, 0.7)
  eta <- -0.7 + u[as.integer(str)]
  p   <- 1/(1 + exp(-eta))
  df$y <- rbinom(n, 1, p)
  
  mA <- maihda_fit(y ~ 1, df, strata = c("ethnicity","gender","class"),
                   family = binomial(), engine = "lme4")
  mB <- maihda_fit(y ~ ethnicity + gender + class, df,
                   strata = c("ethnicity","gender","class"),
                   family = binomial(), engine = "lme4")
  
  vA <- vpc(mA); vB <- vpc(mB); pc <- pcv(mA, mB); mr <- mor(mB)
  
  expect_true(is.finite(vA) && vA >= 0 && vA <= 1)
  expect_true(is.finite(vB) && vB >= 0 && vB <= 1)
  expect_true(is.finite(pc) && pc >= 0 && pc <= 1)
  expect_true(is.finite(mr) && mr >= 1)
})

test_that("cross-context sequence runs", {
  df <- simulate_uk_public_health(n = 4000, seed = 42)
  res <- maihda_sequence(
    response = "y",
    data = df,
    strata = c("ethnicity","sex","income_q"),
    family = binomial(),
    main_effects = c("ethnicity","sex","income_q"),
    contexts = "region",
    engine = "lme4"
  )
  expect_true(inherits(res, "maihda_sequence"))
  expect_true(!is.null(res$models$A))
  expect_true(!is.null(res$models$B))
})


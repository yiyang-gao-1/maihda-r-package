test_that("maihda_sequence runs and PCV is non-negative", {
  df <- simulate_maihda_data(n = 3000, seed = 123)
  seq <- maihda_sequence(
    response = "y",
    data = df,
    strata = c("ethnicity","gender","class"),
    family = binomial(),
    main_effects = c("ethnicity","gender","class"),
    engine = "lme4"
  )
  expect_true(inherits(seq, "maihda_sequence"))
  expect_gte(nrow(seq$metrics), 2)
  if (nrow(seq$metrics) >= 2) {
    pcv_vals <- stats::na.omit(seq$metrics$PCV)
    expect_true(all(pcv_vals >= 0))
  }
  # AUC sanity check
  aucs <- stats::na.omit(seq$metrics$AUC)
  expect_true(all(aucs >= 0.5 & aucs <= 1))
})

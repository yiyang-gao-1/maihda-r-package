# Install (GitHub)

install.packages("devtools")  # or remotes
devtools::install_github("yiyang-gao-1/maihda-r-package", subdir = "maihdaR", dependencies = TRUE)

library(maihdaR)
packageVersion("maihdaR")

# 60-second smoke test

set.seed(1)

# UK-like public-health data with region context
df <- simulate_uk_public_health(3000)

# MAIHDA sequence with cross-context region
seq <- maihda_sequence(
  response = "y",
  data = df,
  strata = c("ethnicity","sex","income_q"),
  family = binomial(),
  main_effects = c("ethnicity","sex","income_q"),
  contexts = "region",
  engine = "lme4",
  control = lme4::glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)),
  nAGQ = 0
)

print(seq)  # VPC / PCV / MOR / AUC table

# Decomposition plot — top 30 strata
dec <- decompose_strata(seq$models$B, type = "response")
plot_strata_decomp(dec, scale = "response", order_by = "total", top_n = 30)

# Vignettes

browseVignettes("maihdaR")
vignette("uk-public-health-using-ukhls-or-sim", package = "maihdaR")
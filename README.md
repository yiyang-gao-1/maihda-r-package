## Overview

The `maihda` package provides specialized tools for conducting **Multilevel Analysis of Individual Heterogeneity and Discriminatory Accuracy (MAIHDA)** in R. MAIHDA is an innovative intersectional approach that uses multilevel models to examine health disparities across multiple intersecting social identities while avoiding the "curse of dimensionality" that affects traditional interaction-based methods.

### What is MAIHDA?

MAIHDA is a quantitative intersectional methodology that:

- 📊 Examines health outcomes across multiple intersecting identities (e.g., race × gender × class × age)
- 🎯 Quantifies both individual heterogeneity and discriminatory accuracy of social categorizations
- 📈 Uses multilevel models to handle high-dimensional intersectional strata
- 🔍 Provides measures of how much health variation exists between vs. within social strata

This approach is particularly valuable for health equity research, allowing researchers to move beyond single-axis analyses (examining only race OR gender OR class) to understand how multiple identities interact to shape health outcomes.

## Key Features

- **🔧 Stratum Creation**: Automated generation of intersectional strata from multiple identity variables
- **📊 Model Fitting**: Streamlined workflows for fitting MAIHDA models with appropriate random effect structures
- **📏 Diagnostic Metrics**: 
  - Intraclass Correlation Coefficient (ICC) for between-stratum variation
  - Proportional Change in Variance (PCV) for understanding fixed effect contributions
  - Area Under the Curve (AUC) for discriminatory accuracy
  - Variance Partition Coefficient (VPC) calculations
- **📈 Visualization Tools**:
  - Caterpillar plots for stratum-specific effects
  - Interaction plots for intersectional disparities
  - Diagnostic plots for model assessment
- **📋 Reporting Functions**: Generate publication-ready tables and summaries

## Installation

You can install the development version of `maihda` from [GitHub](https://github.com/):

```r
# install.packages("devtools")
devtools::install_github("yiyang-gao-1/maihda-r-package")
```

## Basic Usage

```r
library(maihda)

# Load example data (simulated health survey data)
data("health_intersections")

# Create intersectional strata
health_intersections$strata <- create_strata(
  data = health_intersections,
  identity_vars = c("race", "gender", "education", "age_group")
)

# Fit a MAIHDA model
model <- fit_maihda(
  data = health_intersections,
  outcome = "self_rated_health",
  fixed_effects = c("race", "gender", "education", "age_group"),
  strata_var = "strata",
  family = gaussian()  # or binomial() for binary outcomes
)

# Calculate key diagnostics
icc_value <- calculate_icc(model)
print(paste("ICC:", round(icc_value, 3)))

# Visualize stratum effects
plot_strata_effects(model)
```

## Extended Example

```r
# More complex analysis with binary outcome
health_intersections$poor_health <- ifelse(
  health_intersections$self_rated_health < 3, 1, 0
)

# Fit model with binary outcome
model_binary <- fit_maihda(
  data = health_intersections,
  outcome = "poor_health",
  fixed_effects = c("race", "gender", "education", "age_group", "income"),
  strata_var = "strata",
  family = binomial()
)

# Calculate comprehensive diagnostics
diagnostics <- maihda_diagnostics(model_binary)
print(diagnostics)

# Generate report
maihda_report(model_binary, output_format = "html")
```

## Theoretical Background

MAIHDA builds on intersectionality theory (Crenshaw, 1989) and multilevel modeling to address key questions:

1. **Between-stratum variation**: How much do health outcomes vary between different intersectional groups?
2. **Discriminatory accuracy**: How well do social identities predict individual health outcomes?
3. **Interaction effects**: Are intersectional effects more than the sum of their parts?

Key references:
- Evans, C. R., Leckie, G., Subramanian, S. V., Bell, A., & Merlo, J. (2024). A tutorial for conducting intersectional multilevel analysis of individual heterogeneity and discriminatory accuracy (MAIHDA). SSM-population health, 26, 101664.
- Merlo, J. (2018). Multilevel analysis of individual heterogeneity and discriminatory accuracy (MAIHDA) within an intersectional framework. Social science & medicine, 203, 74-80.

## Why Use This Package?

While multilevel models can be fit using existing packages like `lme4`, the `maihda` package provides:

- ✅ **Specialized functions** designed specifically for intersectional analysis
- ✅ **Automated calculations** of MAIHDA-specific metrics not readily available elsewhere
- ✅ **Consistent workflows** that follow best practices from the MAIHDA literature
- ✅ **Built-in visualizations** tailored for presenting intersectional findings
- ✅ **Helper functions** for common data preparation tasks in intersectional analysis
- ✅ **Interpretive guides** for understanding MAIHDA output in context

## Development Roadmap

### Current Development (v0.1.0)
- [x] Basic stratum creation
- [x] Model fitting wrapper
- [ ] ICC calculation
- [ ] Basic visualization

### Planned Features (v0.2.0)
- [ ] PCV and AUC calculations
- [ ] Comprehensive diagnostic functions
- [ ] Advanced plotting capabilities
- [ ] Vignettes with real-world examples

### Future Enhancements (v0.3.0+)
- [ ] Power calculation tools
- [ ] Model comparison functions
- [ ] Integration with tidyverse workflows
- [ ] Shiny app for interactive analysis
- [ ] Support for longitudinal MAIHDA

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details on:
- Reporting bugs
- Suggesting enhancements
- Submitting pull requests
- Code of conduct

## Getting Help

- 📖 [Package documentation](https://yiyang-gao-1.github.io/maihda-r-package/)
- 🐛 [Report issues](https://github.com/yiyang-gao-1/maihda-r-package/issues)
- 💬 [Discussion forum](https://github.com/yiyang-gao-1/maihda-r-package/discussions)
- 📧 Contact: y.gao@sheffield.ac.uk

## Citation

If you use `maihda` in your research, please cite:

```r
citation("maihda")
```

```
To cite package 'maihda' in publications use:

  Y Gao (2025). maihda: Multilevel Analysis of Individual
  Heterogeneity and Discriminatory Accuracy. R package version 0.1.0.
  https://github.com/yiyang-gao-1/maihda-r-package

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {maihda: Multilevel Analysis of Individual Heterogeneity and Discriminatory Accuracy},
    author = {Yiyang Gao},
    year = {2025},
    note = {R package version 0.1.0},
    url = {https://github.com/yiyang-gao-1/maihda-r-package},
  }
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- The MAIHDA methodology was developed by Clare R Evans, Juan Merlo and colleagues
- This package builds upon the excellent `lme4` package for multilevel modeling
- Inspired by the growing community of intersectional health researchers

---

**Package Status**: This package is under active development. Features and interfaces may change. We encourage you to try it out and provide feedback!

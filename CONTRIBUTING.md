## Contributing to maihda

First off, thank you for considering contributing to the `maihdaR` package! 🎉 

This package aims to make MAIHDA methodology accessible to health equity researchers, and contributions from the community are essential to achieving this goal. Whether you're fixing a bug, adding a feature, improving documentation, or sharing examples, every contribution matters.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [How Can I Contribute?](#how-can-i-contribute)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Enhancements](#suggesting-enhancements)
- [Contributing Code](#contributing-code)
- [Improving Documentation](#improving-documentation)
- [Sharing Examples](#sharing-examples)
- [Development Setup](#development-setup)
- [Development Workflow](#development-workflow)
- [Style Guidelines](#style-guidelines)
- [Testing Guidelines](#testing-guidelines)
- [Pull Request Process](#pull-request-process)
- [Recognition](#recognition)



## Code of Conduct

This project adheres to a Code of Conduct. By participating, you are expected to:

- **Be respectful and inclusive**: Welcome people of all backgrounds and identities
- **Be patient**: Remember that everyone was new to R and package development once
- **Be constructive**: Focus on what is best for the community and the package
- **Be mindful**: Consider how your words and actions affect others
- **Accept responsibility**: Listen carefully when you make mistakes and work to correct them

Unacceptable behaviour includes harassment, discriminatory language, and personal attacks. Please report any concerns to [y.gao@sheffield.ac.uk].

## How Can I Contribute?

### Reporting Bugs 🐛

Bugs are tracked as [GitHubissues](https://github.com/yiyang-gao-1/maihda-r-package/issues). Before creating a bug report, please check existing issues to avoid duplicates.

When reporting a bug, include:

1. **Clear, descriptive title**
2. **Minimal reproducible example (reprex)**:

```r
# Load package
library(maihdaR)

# Minimal code that reproduces the issue
data <- data.frame(
  outcome = rnorm(100),
  group1 = sample(c("A", "B"), 100, replace = TRUE),
  group2 = sample(c("X", "Y"), 100, replace = TRUE)
)

# Code that triggers the bug
create_strata(data, c("group1", "group2"))  # Error occurs here
```

3. **What you expected to happen**
4. **What actually happened**
5. **System information**:

```r
sessionInfo()  # Include output
packageVersion("maihda")
```

### Suggesting Enhancements 💡

Enhancement suggestions are also tracked as [GitHub issues](https://github.com/yiyang-gao-1/maihda-r-package/issues). 

For feature requests, please include:

1. **Use case**: Explain the problem you're trying to solve
2. **Proposed solution**: How would this feature work?
3. **Alternative approaches**: Have you considered other solutions?
4. **MAIHDA relevance**: How does this fit with MAIHDA methodology?

Example:

```markdown
## Feature Request: Support for longitudinal MAIHDA

**Use case**: I'm analyzing panel data with repeated measures and need to account for within-person correlation over time.

**Proposed solution**: Add a `fit_longitudinal_maihda()` function that includes temporal random effects.

**Alternatives considered**: Currently using lme4 directly but lose MAIHDA-specific diagnostics.

**MAIHDA relevance**: Extends intersectional analysis to longitudinal contexts as discussed in [citation].
```

### Contributing Code 🖥️

We love code contributions! Here are areas where help is especially welcome:

- **New diagnostic metrics** (e.g., additional discrimination measures)
- **Visualization functions** (e.g., new plot types for intersectional patterns)
- **Data preparation helpers** (e.g., handling missing data in strata)
- **Performance improvements** (e.g., faster computation for large datasets)
- **Bug fixes** (check the [issue tracker](https://github.com/yiyang-gao-1/maihda-r-package/issues))

### Improving Documentation 📚

Documentation improvements are incredibly valuable:

- **Fix typos or clarify explanations**
- **Add examples to function documentation**
- **Write or improve vignettes**
- **Translate documentation** (if you are multilingual)
- **Create tutorials or blog posts** (we will link to them!)

### Sharing Examples 📊

Real-world examples help others learn:

- **Share analysis code** using publicly available data
- **Create case studies** demonstrating MAIHDA applications
- **Contribute example datasets** (properly anonymized)
- **Write methodology comparisons** (MAIHDA vs. traditional approaches)

## Development Setup

1. **Fork and clone the repository**:

```bash
git clone https://github.com/yiyang-gao-1/maihda-r-package.git
cd maihda
```

2. **Open in RStudio**:
   - Open the `maihda.Rproj` file
   - Or use `usethis::proj_activate("path/to/maihda")`

3. **Install development dependencies**:

```r
# Install devtools if needed
install.packages("devtools")

# Install package dependencies
devtools::install_deps(dependencies = TRUE)

# Install development tools
install.packages(c("testthat", "roxygen2", "lintr", "covr"))
```

4. **Load the package for development**:
```r
devtools::load_all()
```

## Development Workflow

### 1. Create a Feature Branch

```bash
git checkout -b feature/your-feature-name
# or
git checkout -b fix/issue-number
```

### 2. Make Your Changes

```r
# Edit files in R/
usethis::use_r("new_function")  # Creates R/new_function.R

# Add tests
usethis::use_test("new_function")  # Creates tests/testthat/test-new_function.R

# Update documentation
devtools::document()  # Updates NAMESPACE and .Rd files
```

### 3. Run Checks

```r
# Run tests
devtools::test()

# Check package
devtools::check()  # Should pass with 0 errors, 0 warnings, 0 notes

# Check code style
lintr::lint_package()

# Check test coverage
covr::package_coverage()
```

### 4. Commit Your Changes

```bash
git add .
git commit -m "Add function for calculating PCV

- Implements proportional change in variance calculation
- Adds tests for edge cases
- Updates documentation with examples"
```

## Style Guidelines

### R Code Style

We follow the [tidyverse style guide](https://style.tidyverse.org/). 

Key points:

```r
# GOOD
calculate_icc <- function(model, ci = FALSE, n_boot = 1000) {
  # Check inputs
  if (!inherits(model, c("lmerMod", "glmerMod"))) {
    stop("Model must be a lmer or glmer object", call. = FALSE)
  }
  
  # Extract variance components
  var_components <- lme4::VarCorr(model)
  var_between <- as.numeric(var_components)
  var_within <- attr(var_components, "sc")^2
  
  # Calculate ICC
  icc <- var_between / (var_between + var_within)
  
  return(icc)
}

# BAD
calcICC=function(model,CI=F,nBoot=1e3){
  if(class(model)[1]!="lmerMod") stop("Wrong model")
  vc=VarCorr(model)
  icc=as.numeric(vc)/(as.numeric(vc)+attr(vc,"sc")^2)
  return(icc)
}
```

### Documentation Style

Use roxygen2 with complete documentation:

```r
#' Calculate Intraclass Correlation Coefficient (ICC)
#'
#' Calculates the ICC for a multilevel model, representing the proportion
#' of total variance attributable to between-stratum differences. This is
#' a key diagnostic in MAIHDA for understanding discriminatory accuracy.
#'
#' @param model A fitted model object of class \code{lmerMod} or \code{glmerMod}
#'   from the \code{lme4} package.
#' @param ci Logical. Should confidence intervals be calculated? Default is FALSE.
#' @param n_boot Integer. Number of bootstrap samples for CI calculation.
#'   Default is 1000. Only used if \code{ci = TRUE}.
#'
#' @return If \code{ci = FALSE}, a numeric value between 0 and 1 representing
#'   the ICC. If \code{ci = TRUE}, a named vector with elements 'estimate',
#'   'lower', and 'upper'.
#'
#' @details
#' The ICC in MAIHDA context represents discriminatory accuracy - the ability
#' of intersectional strata to predict individual outcomes. Values interpretation:
#' \itemize{
#'   \item ICC < 0.05: Very low discriminatory accuracy
#'   \item ICC 0.05-0.10: Low discriminatory accuracy  
#'   \item ICC 0.10-0.20: Moderate discriminatory accuracy
#'   \item ICC > 0.20: High discriminatory accuracy
#' }
#'
#' @examples
#' \dontrun{
#' # Fit a MAIHDA model
#' model <- fit_maihda(data, "outcome", c("var1", "var2"), "strata")
#' 
#' # Calculate ICC
#' icc_value <- calculate_icc(model)
#' 
#' # With confidence intervals
#' icc_ci <- calculate_icc(model, ci = TRUE, n_boot = 2000)
#' }
#'
#' @references
#' Merlo, J. (2018). Multilevel analysis of individual heterogeneity and 
#' discriminatory accuracy (MAIHDA) within an intersectional framework. 
#' \emph{Social Science & Medicine}, 203, 74-80.
#'
#' @seealso \code{\link{fit_maihda}}, \code{\link{calculate_pcv}}
#'
#' @export
```

### Commit Message Style

Follow conventional commits:

```
type(scope): description

[optional body]

[optional footer]
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `style`: Code style changes (no functional changes)
- `refactor`: Code refactoring
- `test`: Adding or updating tests
- `chore`: Maintenance tasks

Examples:
```
feat(diagnostics): add AUC calculation function

fix(strata): handle NA values in identity variables

docs(readme): add installation troubleshooting section

test(icc): add edge cases for single-level models
```

## Testing Guidelines

### Writing Tests

Every new function needs tests:

```r
# In tests/testthat/test-calculate_icc.R

test_that("calculate_icc returns correct value for simple model", {
  # Setup
  test_data <- data.frame(
    outcome = rnorm(100),
    strata = rep(1:10, each = 10)
  )
  model <- lme4::lmer(outcome ~ 1 + (1|strata), data = test_data)
  
  # Test
  icc <- calculate_icc(model)
  
  # Assertions
  expect_type(icc, "double")
  expect_true(icc >= 0 && icc <= 1)
  expect_length(icc, 1)
})

test_that("calculate_icc handles edge cases", {
  # Test with no variance between groups
  test_data <- data.frame(
    outcome = rnorm(100, mean = 5, sd = 1),
    strata = rep(1:10, each = 10)
  )
  
  # Make all group means identical
  for (i in 1:10) {
    test_data$outcome[test_data$strata == i] <- 
      test_data$outcome[test_data$strata == i] - 
      mean(test_data$outcome[test_data$strata == i]) + 5
  }
  
  model <- lme4::lmer(outcome ~ 1 + (1|strata), data = test_data)
  icc <- calculate_icc(model)
  
  expect_true(icc < 0.01)  # Should be near zero
})

test_that("calculate_icc errors appropriately", {
  expect_error(
    calculate_icc("not a model"),
    "Model must be a lmer or glmer object"
  )
  
  expect_error(
    calculate_icc(lm(mpg ~ cyl, data = mtcars)),
    "Model must be a lmer or glmer object"
  )
})
```

### Test Coverage

Aim for >80% test coverage:

```r
# Check coverage
covr::package_coverage()

# Generate coverage report
covr::report()
```

## Pull Request Process

1. **Ensure all checks pass**:
```r
devtools::check()  # Must pass with no errors/warnings
```

2. **Update documentation**:
   - Update README.md if needed
   - Add your feature to NEWS.md
   - Update vignettes if relevant

3. **Create the pull request**:
   - Give it a clear title
   - Reference any related issues (#123)
   - Describe what changes you made and why
   - Include examples of how to use new features

4. **Pull request template**:

```
## Description
Brief description of what this PR does.

## Related Issue
Fixes #(issue number)

## Type of Change
- [ ] Bug fix
- [ ] New feature
- [ ] Breaking change
- [ ] Documentation update

## Checklist
- [ ] My code follows the style guidelines
- [ ] I have performed a self-review
- [ ] I have commented my code where necessary
- [ ] I have updated the documentation
- [ ] My changes generate no new warnings
- [ ] I have added tests that prove my fix/feature works
- [ ] New and existing unit tests pass locally
- [ ] I have updated NEWS.md


# Examples to show how to use your new feature
example_code_here()
```


5. **Respond to review feedback**:
   - Be patient and respectful
   - Make requested changes
   - Ask for clarification if needed
   - Push new commits to the same branch

## Recognition

### Contributors

All contributors will be recognized in:
- The package DESCRIPTION file
- The README contributors section
- The package website (when created)

### Types of Recognition

- **Code contributors**: Listed as contributors (`ctb`) in DESCRIPTION
- **Bug reporters**: Acknowledged in release notes
- **Documentation writers**: Credited in relevant documents
- **Example providers**: Named in example attributions
- **Major contributors**: May be invited as package authors (`aut`)

## Questions?

If you have questions about contributing:

1. Check the [package documentation](https://github.com/yiyang-gao-1/maihda-r-package)
2. Open a [discussion](https://github.com/yiyang-gao-1/maihda-r-package/discussions)
3. Contact the maintainer at [y.gao@sheffield.ac.uk]

## Thank You! 🙏

Your contributions make this package better for everyone studying health intersectionality. Whether you're fixing a typo or adding a major feature, we appreciate your time and effort!

---

*This contributing guide is adapted from best practices in the R community and follows guidelines from [rOpenSci](https://devguide.ropensci.org/) and the [tidyverse](https://www.tidyverse.org/contribute/).*

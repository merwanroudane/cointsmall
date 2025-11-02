# cointsmall: Cointegration Testing with Structural Breaks in Very Small Samples

R package implementing cointegration tests with endogenous structural breaks for very small sample sizes (T < 50) following Trinh (2022).

## Overview

This package implements the methodology from:

> Trinh, J. (2022). Testing for cointegration with structural changes in very small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.

The package extends the Gregory-Hansen (1996) test to allow up to two structural breaks with size-corrected critical values computed via surface response methodology. It is designed for macroeconometric studies of emerging economies where data history is limited.

## Features

- **Size-corrected critical values** for sample sizes as small as T=15
- **Multiple model specifications**: No breaks, breaks in intercept only, breaks in intercept and slope
- **Up to 2 structural breaks** with endogenous break date selection
- **Composite testing procedure** for automatic model selection
- **Verified against paper**: All critical values match Table 1 from Trinh (2022)

## Installation

```r
# Install from GitHub (recommended)
# install.packages("devtools")
devtools::install_github("merwanroudane/cointsmall")

# Or install from local source
install.packages("path/to/cointsmall", repos = NULL, type = "source")
```

## Quick Start

```r
library(cointsmall)

# Generate cointegrated data
set.seed(123)
T <- 50
X <- matrix(rnorm(T * 2), ncol = 2)
Y <- 2 + 1.5 * X[,1] + 0.8 * X[,2] + rnorm(T, sd = 0.5)

# Test for cointegration using composite procedure
result <- composite_cointegration_test(Y, X, max_breaks = 2)
print(result)
summary(result)
```

## Main Functions

### `test_cointegration_breaks()`
Test for cointegration with a specific number of breaks:
```r
# No breaks
result <- test_cointegration_breaks(Y, X, n_breaks = 0, model = "o")

# One break in intercept and slope
result <- test_cointegration_breaks(Y, X, n_breaks = 1, model = "cs")

# Two breaks in intercept only
result <- test_cointegration_breaks(Y, X, n_breaks = 2, model = "c")
```

### `composite_cointegration_test()`
Automatic model selection across multiple specifications:
```r
result <- composite_cointegration_test(Y, X, max_breaks = 2)
```

### `get_critical_value()`
Get size-corrected critical values:
```r
# T=30, m=1 regressor, 1 break in intercept and slope
cv <- get_critical_value(T = 30, m = 1, b = 1, model = "cs")
```

### `verify_critical_values()`
Verify implementation against paper:
```r
verify_critical_values()  # Should return TRUE
```

## Model Specifications

- **Model O**: No structural breaks (standard cointegration test)
- **Model C**: Breaks in intercept only
- **Model CS**: Breaks in intercept and slope coefficients

## Limitations

- Only 5% significance level is implemented (as in the paper)
- Maximum 3 regressors (m ≤ 3)
- Maximum 2 breaks (b ≤ 2)
- P-values are not computed (not provided in original paper)

## Citation

When using this package, please cite:

```
Trinh, J. (2022). Testing for cointegration with structural changes in very 
small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.
```

And optionally cite this package:

```
Roudane, M. (2025). cointsmall: R package for cointegration testing with 
structural breaks in very small samples. R package version 0.1.0.
```

## Author

- **Methodology**: Jérôme Trinh (jerome.trinh@ensae.fr)
- **R Package**: Dr. Merwan Roudane (merwanroudane920@gmail.com)

## License

GPL-3

## References

- Gregory, A. W., & Hansen, B. E. (1996). Residual-based tests for cointegration in models with regime shifts. *Journal of Econometrics*, 70(1), 99-126.
- MacKinnon, J. G. (1991). Critical values for cointegration tests. In *Long-Run Economic Relationships: Readings in Cointegration*, Chapter 13.
- Trinh, J. (2022). Testing for cointegration with structural changes in very small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.

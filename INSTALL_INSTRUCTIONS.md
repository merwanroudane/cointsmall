# Installation Instructions for cointsmall Package

This document provides step-by-step instructions to build and install the `cointsmall` R package.

## Prerequisites

Make sure you have R installed (version >= 3.5.0). You'll also need the following R packages:

```r
install.packages(c("devtools", "roxygen2", "testthat", "lmtest", "knitr", "rmarkdown"))
```

## Method 1: Automated Build and Install (Recommended)

If you have all the files in `/home/claude/`, run:

```r
source("/home/claude/BUILD_AND_INSTALL.R")
```

This script will:
1. Create the data file
2. Generate documentation
3. Run tests
4. Check the package
5. Build the package
6. Install the package

## Method 2: Manual Installation

### Step 1: Create Data File

```r
setwd("/home/claude/cointsmall")

CRITICAL_VALUE_COEFS <- list(
  o_m1 = c(-3.33, -16.88, 798.01, -30818.40, 460634.58, -2279397.87),
  o_m2 = c(-3.75, -10.25, 80.17, -13337.52, 302551.21, -1848305.35),
  o_m3 = c(-4.10, -12.16, -321.05, 7197.98, -40759.64, 0),
  c_m1_b1 = c(-4.62, -13.05, -1399.49, 76213.27, -1939275.51, 23030362.35, -99593635.82),
  c_m2_b1 = c(-4.97, -28.28, 112.01, -3338.30, 48647.86, 0, 0),
  c_m3_b1 = c(-5.30, -40.62, 1759.22, -94294.31, 2287166.84, -25326822.40, 106995759.84),
  c_m1_b2 = c(-5.21, -279.74, 35643.95, -1963265.34, 49133408.78, -564440298.77, 2411884754.22),
  c_m2_b2 = c(-5.53, -287.06, 36360.62, -2001591.63, 50089808.47, -575604522.50, 2460441619.22),
  c_m3_b2 = c(-5.88, -272.48, 34788.63, -1938696.30, 48860977.69, -564638501.27, 2425723155.38),
  cs_m1_b1 = c(-4.96, -20.19, -64.18, -1901.05, 45903.20, 0, 0),
  cs_m2_b1 = c(-5.55, -29.61, 205.17, -7483.02, 84068.24, 0, 0),
  cs_m3_b1 = c(-6.09, -13.81, -2439.38, 125430.43, -2972990.17, 32209309.76, -126768011.40),
  cs_m1_b2 = c(-5.94, -207.45, 26499.94, -1492615.06, 37796851.87, -437913647.90, 1884665034.50),
  cs_m2_b2 = c(-6.90, -57.52, 5352.61, -403380.76, 12234074.42, -164125034.32, 802737634.37),
  cs_m3_b2 = c(-7.67, 65.20, -18638.41, 1263981, -41398810, 639118600, -3757791000)
)

if (!dir.exists("data")) dir.create("data")
save(CRITICAL_VALUE_COEFS, file = "data/CRITICAL_VALUE_COEFS.rda", compress = "xz")
```

### Step 2: Generate Documentation

```r
library(roxygen2)
roxygenize()
```

### Step 3: Install Package

```r
library(devtools)
install()
```

## Method 3: Install from Source Tarball

If you have the built package tarball:

```r
install.packages("/home/claude/cointsmall_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Verification

After installation, verify the package works correctly:

```r
library(cointsmall)

# Verify critical values match the paper
verify_critical_values()  # Should return TRUE

# Quick test
set.seed(123)
T <- 50
X <- matrix(rnorm(T), ncol = 1)
Y <- 2 + 1.5 * X[,1] + rnorm(T, sd = 0.3)

result <- test_cointegration_breaks(Y, X, n_breaks = 0, model = "o")
print(result)
```

## Troubleshooting

### Issue: "package 'lmtest' is not available"
Solution:
```r
install.packages("lmtest")
```

### Issue: "there is no package called 'devtools'"
Solution:
```r
install.packages("devtools")
```

### Issue: "lazy loading failed"
Solution: Make sure the data file was created correctly in Step 1.

### Issue: Package check fails
This may be due to missing documentation or tests. Run:
```r
library(devtools)
document()  # Generate documentation
test()      # Run tests
check()     # Check package
```

## Contact

For issues or questions:
- Dr. Merwan Roudane: merwanroudane920@gmail.com

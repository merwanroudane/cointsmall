# =============================================================================
# ADF Test with Automatic Lag Selection
# Based on Trinh (2022), Section 2, page 4
# =============================================================================

#' @importFrom lmtest bgtest
NULL

#' ADF Test on Residuals with Automatic Lag Selection
#'
#' Performs Augmented Dickey-Fuller test on regression residuals with
#' minimal lag selection based on serial correlation testing using the
#' Breusch-Godfrey test.
#'
#' @param residuals Numeric vector. Regression residuals to test for unit root.
#' @param max_lags Integer. Maximum number of lags to consider. If NULL, uses
#'   the rule min(12*(T/100)^0.25, (T-1)/3).
#'
#' @return A list with class "adf_test" containing:
#'   \item{statistic}{Numeric. ADF test statistic (t-statistic for gamma)}
#'   \item{lag}{Integer. Selected number of lags}
#'   \item{model}{Object of class "lm". Fitted ADF regression model}
#'
#' @details
#' Following Trinh (2022, page 4): "We only consider the Augmented Dickey-Fuller 
#' (ADF) version of the test for its relative efficiency in very small sample, 
#' in which we select the minimal number of lags for which the residuals of the 
#' ADF model are not serially correlated."
#' 
#' The ADF regression is: Delta epsilon_t = gamma*epsilon_{t-1} + sum phi_i*Delta epsilon_{t-i} + error
#' 
#' The test statistic is: t = gamma_hat / SE(gamma_hat)
#'
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, Section 2, page 4.
#'
#' @examples
#' # Generate some residuals
#' set.seed(123)
#' residuals <- cumsum(rnorm(50, sd = 0.1))
#' 
#' # Test for unit root
#' result <- adf_test_residuals(residuals)
#' result$statistic
#' result$lag
#'
#' @export
adf_test_residuals <- function(residuals, max_lags = NULL) {
  
  n <- length(residuals)
  
  if (n < 10) {
    stop("At least 10 observations required for ADF test")
  }
  
  if (is.null(max_lags)) {
    # Standard rule: min(12*(T/100)^0.25, (T-1)/3)
    max_lags <- min(floor(12 * (n/100)^0.25), floor((n-1)/3))
  }
  
  if (max_lags >= n - 3) {
    max_lags <- max(0, n - 10)
  }
  
  # Try lags from 0 upwards until no serial correlation
  for (lag in 0:max_lags) {
    
    # First difference of residuals
    d_resid <- diff(residuals)
    n_diff <- length(d_resid)
    
    if (lag == 0) {
      # ADF without lags
      y_lag <- residuals[1:(n-1)]
      fit <- lm(d_resid ~ y_lag - 1)
      
    } else {
      # ADF with lags
      lag_matrix <- matrix(NA, nrow = n_diff, ncol = lag)
      for (i in 1:lag) {
        if ((i+1) <= n_diff) {
          lag_matrix[(i+1):n_diff, i] <- d_resid[1:(n_diff-i)]
        }
      }
      
      # Combine data
      data_df <- data.frame(
        dy = d_resid,
        y_lag = residuals[1:(n-1)]
      )
      data_df <- cbind(data_df, lag_matrix)
      
      # Remove NA rows
      data_df <- data_df[complete.cases(data_df), ]
      
      if (nrow(data_df) < lag + 3) {
        next  # Not enough observations
      }
      
      # Fit model
      fit <- lm(dy ~ . - 1, data = data_df)
    }
    
    # Test for serial correlation using Breusch-Godfrey test
    if (lag > 0 && nrow(fit$model) > lag + 2) {
      bg_test <- tryCatch({
        lmtest::bgtest(fit, order = 1)
      }, error = function(e) NULL)
      
      # If no serial correlation detected, use this lag
      if (!is.null(bg_test) && bg_test$p.value > 0.05) {
        # Compute t-statistic
        test_stat <- coef(fit)[1] / sqrt(vcov(fit)[1, 1])
        result <- list(
          statistic = test_stat,
          lag = lag,
          model = fit
        )
        class(result) <- "adf_test"
        return(result)
      }
    }
  }
  
  # If no lag eliminates serial correlation, use maximum lag
  d_resid <- diff(residuals)
  
  if (max_lags == 0) {
    y_lag <- residuals[1:(n-1)]
    final_fit <- lm(d_resid ~ y_lag - 1)
    lag_used <- 0
  } else {
    lag_matrix <- matrix(NA, nrow = length(d_resid), ncol = max_lags)
    for (i in 1:max_lags) {
      if ((i+1) <= length(d_resid)) {
        lag_matrix[(i+1):length(d_resid), i] <- d_resid[1:(length(d_resid)-i)]
      }
    }
    
    data_df <- data.frame(
      dy = d_resid,
      y_lag = residuals[1:(n-1)]
    )
    data_df <- cbind(data_df, lag_matrix)
    data_df <- data_df[complete.cases(data_df), ]
    
    final_fit <- lm(dy ~ . - 1, data = data_df)
    lag_used <- max_lags
  }
  
  # Compute final test statistic
  final_stat <- coef(final_fit)[1] / sqrt(vcov(final_fit)[1, 1])
  
  result <- list(
    statistic = final_stat,
    lag = lag_used,
    model = final_fit
  )
  class(result) <- "adf_test"
  return(result)
}

#' Print Method for adf_test Objects
#'
#' @param x An object of class "adf_test"
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object x
#' @export
print.adf_test <- function(x, ...) {
  cat("\nAugmented Dickey-Fuller Test\n")
  cat("============================\n")
  cat("Test Statistic:", round(x$statistic, 4), "\n")
  cat("Lags:", x$lag, "\n\n")
  invisible(x)
}

# =============================================================================
# Cointegration Test with Structural Breaks
# Based on Trinh (2022), Section 2
# =============================================================================

#' Test for Cointegration with Structural Breaks
#'
#' Implements the Gregory-Hansen (1996) test extended to allow up to two
#' endogenous structural breaks, as developed in Trinh (2022).
#'
#' @param Y Numeric vector. Dependent variable (T x 1).
#' @param X Numeric matrix or vector. Independent variables (T x m).
#' @param n_breaks Integer. Number of structural breaks (0, 1, or 2).
#' @param model Character. Type of structural change: "o" (no breaks),
#'   "c" (breaks in intercept), "cs" (breaks in intercept and slope).
#' @param trim Numeric. Trimming parameter for break date search (default: 0.15).
#'   Restricts break dates to [trim*T, (1-trim)*T].
#' @param alpha Numeric. Significance level for critical value (default: 0.05).
#'
#' @return A list with class "coint_test" containing:
#'   \item{statistic}{Numeric. Minimum ADF test statistic (ADF*)}
#'   \item{critical_value}{Numeric. Size-corrected critical value}
#'   \item{p_value}{Numeric. Approximate p-value (currently NA)}
#'   \item{reject_null}{Logical. TRUE if null of no cointegration is rejected}
#'   \item{break_dates}{Integer vector. Estimated break dates}
#'   \item{model}{Character. Model type used}
#'   \item{n_breaks}{Integer. Number of breaks}
#'   \item{residuals}{Numeric vector. Residuals from best-fit regression}
#'   \item{coefficients}{Numeric vector. Estimated coefficients}
#'   \item{adf_result}{Object of class "adf_test". ADF test results}
#'
#' @details
#' Following Trinh (2022), the test minimizes the ADF statistic over all
#' possible break dates: ADF* = inf_{t_i} ADF(epsilon_hat)
#' 
#' The regression model (Equation 1, page 3) is:
#' Y = [1 X][mu beta]' + sum B_i[1 X][mu_i beta_i]' + epsilon
#' 
#' where B_i are break dummy matrices.
#'
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, Section 2, pages 3-5.
#' 
#' Gregory, A. W., & Hansen, B. E. (1996). Residual-based tests for cointegration 
#' in models with regime shifts. Journal of Econometrics, 70(1), 99-126.
#'
#' @examples
#' # Generate cointegrated data
#' set.seed(123)
#' T <- 50
#' X <- matrix(rnorm(T), ncol = 1)
#' Y <- 2 + 1.5 * X[,1] + rnorm(T, sd = 0.3)
#' 
#' # Test for cointegration without breaks
#' result <- test_cointegration_breaks(Y, X, n_breaks = 0, model = "o")
#' print(result)
#' 
#' # Test with one break in intercept and slope
#' result2 <- test_cointegration_breaks(Y, X, n_breaks = 1, model = "cs")
#' print(result2)
#'
#' @export
test_cointegration_breaks <- function(Y, X, n_breaks, model = "cs", 
                                      trim = 0.15, alpha = 0.05) {
  
  # Ensure X is a matrix
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  
  T <- length(Y)
  m <- ncol(X)
  
  # Input validation
  if (T != nrow(X)) {
    stop("Y and X must have the same number of observations")
  }
  
  if (trim <= 0 || trim >= 0.5) {
    stop("trim must be between 0 and 0.5")
  }
  
  if (n_breaks < 0 || n_breaks > 2) {
    stop("n_breaks must be 0, 1, or 2")
  }
  
  if (!model %in% c("o", "c", "cs")) {
    stop("model must be 'o', 'c', or 'cs'")
  }
  
  # Get critical value
  cv <- get_critical_value(T = T, m = m, b = n_breaks, model = model, alpha = alpha)
  
  if (is.na(cv)) {
    warning("Could not compute critical value for this configuration")
  }
  
  # Case: No breaks
  if (n_breaks == 0 || model == "o") {
    # Simple OLS regression
    fit <- lm(Y ~ X)
    residuals <- residuals(fit)
    adf_result <- adf_test_residuals(residuals)
    
    result <- list(
      statistic = adf_result$statistic,
      critical_value = cv,
      p_value = NA_real_,
      reject_null = adf_result$statistic < cv,
      break_dates = NULL,
      model = model,
      n_breaks = 0,
      residuals = residuals,
      coefficients = coef(fit),
      adf_result = adf_result
    )
    class(result) <- "coint_test"
    return(result)
  }
  
  # Case: With breaks - grid search
  trim_start <- ceiling(T * trim)
  trim_end <- floor(T * (1 - trim))
  possible_dates <- trim_start:trim_end
  
  min_stat <- Inf
  best_breaks <- NULL
  best_residuals <- NULL
  best_coefs <- NULL
  best_adf <- NULL
  
  if (n_breaks == 1) {
    # Search over single break dates
    for (t1 in possible_dates) {
      # Create break dummies
      D1 <- matrix(0, T, m + 1)
      D1[t1:T, ] <- cbind(1, X[t1:T, , drop = FALSE])
      
      # Construct design matrix
      if (model == "c") {
        # Break in intercept only
        Z <- cbind(1, X, D1[, 1, drop = FALSE])
      } else {  # model == "cs"
        # Break in intercept and slope
        Z <- cbind(1, X, D1)
      }
      
      # Estimate regression
      fit <- lm(Y ~ Z - 1)
      resid <- residuals(fit)
      
      # ADF test on residuals
      adf_result <- adf_test_residuals(resid)
      
      if (adf_result$statistic < min_stat) {
        min_stat <- adf_result$statistic
        best_breaks <- t1
        best_residuals <- resid
        best_coefs <- coef(fit)
        best_adf <- adf_result
      }
    }
    
  } else if (n_breaks == 2) {
    # Search over two break dates
    for (t1 in possible_dates) {
      for (t2 in possible_dates) {
        if (t2 <= t1 + ceiling(T * trim)) next  # Ensure minimum distance
        
        # Create break dummies
        D1 <- matrix(0, T, m + 1)
        D1[t1:T, ] <- cbind(1, X[t1:T, , drop = FALSE])
        
        D2 <- matrix(0, T, m + 1)
        D2[t2:T, ] <- cbind(1, X[t2:T, , drop = FALSE])
        
        # Construct design matrix
        if (model == "c") {
          # Breaks in intercept only
          Z <- cbind(1, X, D1[, 1, drop = FALSE], D2[, 1, drop = FALSE])
        } else {  # model == "cs"
          # Breaks in intercept and slope
          Z <- cbind(1, X, D1, D2)
        }
        
        # Estimate regression
        fit <- tryCatch({
          lm(Y ~ Z - 1)
        }, error = function(e) NULL)
        
        if (is.null(fit)) next
        
        resid <- residuals(fit)
        
        # ADF test on residuals
        adf_result <- adf_test_residuals(resid)
        
        if (adf_result$statistic < min_stat) {
          min_stat <- adf_result$statistic
          best_breaks <- c(t1, t2)
          best_residuals <- resid
          best_coefs <- coef(fit)
          best_adf <- adf_result
        }
      }
    }
  }
  
  # Return results
  result <- list(
    statistic = min_stat,
    critical_value = cv,
    p_value = NA_real_,
    reject_null = min_stat < cv,
    break_dates = best_breaks,
    model = model,
    n_breaks = n_breaks,
    residuals = best_residuals,
    coefficients = best_coefs,
    adf_result = best_adf
  )
  class(result) <- "coint_test"
  return(result)
}

#' Print Method for coint_test Objects
#'
#' @param x An object of class "coint_test"
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object x
#' @export
print.coint_test <- function(x, ...) {
  cat("\nCointegration Test with Structural Breaks\n")
  cat("==========================================\n")
  cat("Model:", x$model, "\n")
  cat("Number of breaks:", x$n_breaks, "\n")
  if (!is.null(x$break_dates)) {
    cat("Break dates:", paste(x$break_dates, collapse = ", "), "\n")
  }
  cat("\nTest Statistic (ADF*):", round(x$statistic, 4), "\n")
  cat("Critical Value (5%):", round(x$critical_value, 4), "\n")
  cat("Decision:", ifelse(x$reject_null, 
                          "Reject null (cointegration detected)", 
                          "Fail to reject null (no cointegration)"), "\n\n")
  invisible(x)
}

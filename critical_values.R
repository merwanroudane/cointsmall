# =============================================================================
# Critical Values and Main Functions
# Based on Trinh (2022)
# =============================================================================

#' Critical Value Coefficients
#' 
#' Surface response coefficients for computing critical values.
#' Verified against Table 1 (page 8) of Trinh (2022).
#' 
#' @format A list with 15 elements, each containing coefficient vectors for
#'   different model configurations (model type, number of regressors, and breaks)
#' @source Trinh, J. (2022). Testing for cointegration with structural changes 
#'   in very small sample. THEMA Working Paper n°2022-01, Table 13, page 31.
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.
"CRITICAL_VALUE_COEFS"

#' Get Critical Value for Cointegration Test
#' 
#' Computes size-corrected critical values using surface response methodology
#' following MacKinnon (1991) as implemented in Trinh (2022).
#'
#' @param T Integer. Sample size (number of observations). Should be >= 12.
#' @param m Integer. Number of regressors (1, 2, or 3).
#' @param b Integer. Number of structural breaks (0, 1, or 2).
#' @param model Character. Model type: "o" (no breaks), "c" (breaks in intercept), 
#'   or "cs" (breaks in intercept and slope).
#' @param alpha Numeric. Significance level. Only 0.05 is currently implemented.
#'
#' @return Numeric. Critical value for the specified configuration, or NA if
#'   the configuration is not available.
#' 
#' @details
#' The surface response function is: Crt(T, q, m, b, M) = ψ∞ + Σ ψk/T^k
#' where k ranges from 1 to K (order selected by AIC, maximum 6).
#' See Trinh (2022), Section 4, pages 6-7.
#' 
#' The function returns size-corrected critical values that account for the
#' severe size distortion observed in very small samples (T < 50).
#'
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.
#' 
#' MacKinnon, J. G. (1991). Critical values for cointegration tests. In 
#' Long-Run Economic Relationships: Readings in Cointegration, Chapter 13.
#'
#' @examples
#' # Get critical value for T=30, m=1, one break in intercept and slope
#' get_critical_value(T = 30, m = 1, b = 1, model = "cs")
#' 
#' # Get critical value for T=50, m=2, no breaks
#' get_critical_value(T = 50, m = 2, b = 0, model = "o")
#' 
#' # Get critical value for T=100, m=1, two breaks in intercept
#' get_critical_value(T = 100, m = 1, b = 2, model = "c")
#'
#' @export
get_critical_value <- function(T, m, b = 1, model = "cs", alpha = 0.05) {
  
  # Input validation
  if (alpha != 0.05) {
    warning("Only 5% critical values are implemented in Trinh (2022). Returning NA.")
    return(NA_real_)
  }
  
  if (m > 3 || m < 1) {
    warning("Critical values only available for m = 1, 2, or 3. Returning NA.")
    return(NA_real_)
  }
  
  if (b > 2 || b < 0) {
    warning("Critical values only available for b = 0, 1, or 2. Returning NA.")
    return(NA_real_)
  }
  
  if (T < 12) {
    warning("Sample size T should be at least 12 for reliable critical values.")
  }
  
  # Construct key for coefficient lookup
  if (model == "o" || b == 0) {
    key <- paste0("o_m", m)
  } else {
    key <- paste0(model, "_m", m, "_b", b)
  }
  
  if (!key %in% names(CRITICAL_VALUE_COEFS)) {
    warning(sprintf(
      "No critical values for configuration: m=%d, b=%d, model='%s'. Returning NA.",
      m, b, model
    ))
    return(NA_real_)
  }
  
  coefs <- CRITICAL_VALUE_COEFS[[key]]
  
  # Apply surface response function
  cv <- coefs[1]  # ψ∞ (asymptotic critical value)
  
  for (k in 2:length(coefs)) {
    if (!is.na(coefs[k]) && coefs[k] != 0) {
      cv <- cv + coefs[k] / (T^(k-1))
    }
  }
  
  return(cv)
}

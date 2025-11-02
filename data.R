# =============================================================================
# Package Data Documentation
# =============================================================================

#' Critical Value Coefficients
#' 
#' Surface response coefficients for computing size-corrected critical values
#' using MacKinnon (1991) methodology. These coefficients are used in the
#' surface response function to compute critical values for any sample size.
#' 
#' @format A named list with 15 elements. Each element is a numeric vector
#'   of coefficients for a specific configuration:
#'   \describe{
#'     \item{o_m1, o_m2, o_m3}{No breaks, m=1,2,3 regressors}
#'     \item{c_m1_b1, c_m2_b1, c_m3_b1}{One break in intercept, m=1,2,3}
#'     \item{c_m1_b2, c_m2_b2, c_m3_b2}{Two breaks in intercept, m=1,2,3}
#'     \item{cs_m1_b1, cs_m2_b1, cs_m3_b1}{One break in intercept and slope, m=1,2,3}
#'     \item{cs_m1_b2, cs_m2_b2, cs_m3_b2}{Two breaks in intercept and slope, m=1,2,3}
#'   }
#'   
#' The surface response function is: Crt(T) = psi_inf + sum(psi_k / T^k)
#' where the first element is psi_inf (asymptotic value) and subsequent
#' elements are psi_1, psi_2, ..., psi_K.
#'   
#' @source Trinh, J. (2022). Testing for cointegration with structural changes 
#'   in very small sample. THEMA Working Paper n°2022-01, Table 13, page 31.
#'   
#' @references
#' MacKinnon, J. G. (1991). Critical values for cointegration tests. 
#' In Long-Run Economic Relationships: Readings in Cointegration, Chapter 13.
#' 
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, CY Cergy Paris Université.
#'   
#' @examples
#' # View coefficient names
#' names(CRITICAL_VALUE_COEFS)
#' 
#' # View coefficients for model cs, m=1, b=1
#' CRITICAL_VALUE_COEFS$cs_m1_b1
#' 
#' # Asymptotic critical value (first coefficient)
#' CRITICAL_VALUE_COEFS$cs_m1_b1[1]
"CRITICAL_VALUE_COEFS"

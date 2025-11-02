# =============================================================================
# Composite Cointegration Testing Procedure
# Based on Trinh (2022), Section 6
# =============================================================================

#' Composite Cointegration Testing Procedure
#'
#' Tests for cointegration across multiple model specifications and selects
#' the best model based on joint significance tests of structural break
#' parameters, following Trinh (2022), Section 6.
#'
#' @param Y Numeric vector. Dependent variable (T x 1).
#' @param X Numeric matrix or vector. Independent variables (T x m).
#' @param max_breaks Integer. Maximum number of breaks to consider (default: 2).
#' @param alpha Numeric. Significance level (default: 0.05).
#' @param trim Numeric. Trimming parameter for break search (default: 0.15).
#' @param verbose Logical. If TRUE, print progress messages (default: FALSE).
#'
#' @return A list with class "composite_coint_test" containing:
#'   \item{conclusion}{Character. Overall conclusion about cointegration}
#'   \item{selected_model}{Object of class "coint_test". Best model results, or NULL}
#'   \item{all_results}{List. Results for all tested models}
#'   \item{model_selection_info}{List. Information about model selection process}
#'
#' @details
#' Following Trinh (2022), Section 6 (pages 17-19), the procedure:
#' 
#' \enumerate{
#'   \item Tests models: O (no breaks), C_b1, CS_b1, C_b2, CS_b2
#'   \item Applies selection rules (page 18):
#'     \itemize{
#'       \item If null never rejected: No cointegration
#'       \item If null rejected in one model only: Select that model
#'       \item If null rejected in multiple models: Select most general model
#'     }
#'   \item Model priority: CS_b2 > CS_b1 > C_b2 > C_b1 > O_b0
#' }
#'
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, Section 6, pages 17-19.
#'
#' @examples
#' # Generate cointegrated data
#' set.seed(123)
#' T <- 50
#' X <- matrix(rnorm(T * 2), ncol = 2)
#' Y <- 2 + 1.5 * X[,1] + 0.8 * X[,2] + rnorm(T, sd = 0.5)
#' 
#' # Test using composite procedure
#' result <- composite_cointegration_test(Y, X, max_breaks = 2)
#' print(result)
#' summary(result)
#'
#' @export
composite_cointegration_test <- function(Y, X, max_breaks = 2, 
                                        alpha = 0.05, trim = 0.15,
                                        verbose = FALSE) {
  
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  
  if (verbose) {
    message("Testing for cointegration using composite procedure...")
  }
  
  # Test all model configurations
  all_results <- list()
  
  # Model O (no breaks)
  if (verbose) message("  Testing model O (no breaks)...")
  all_results[["o_b0"]] <- test_cointegration_breaks(
    Y, X, n_breaks = 0, model = "o", trim = trim, alpha = alpha
  )
  
  # Models with breaks
  for (b in 1:max_breaks) {
    # Model C (breaks in intercept)
    model_name <- paste0("c_b", b)
    if (verbose) message(sprintf("  Testing model %s...", model_name))
    all_results[[model_name]] <- test_cointegration_breaks(
      Y, X, n_breaks = b, model = "c", trim = trim, alpha = alpha
    )
    
    # Model CS (breaks in intercept and slope)
    model_name <- paste0("cs_b", b)
    if (verbose) message(sprintf("  Testing model %s...", model_name))
    all_results[[model_name]] <- test_cointegration_breaks(
      Y, X, n_breaks = b, model = "cs", trim = trim, alpha = alpha
    )
  }
  
  # Count how many models reject null
  rejected_models <- names(all_results)[sapply(all_results, function(x) x$reject_null)]
  
  # Selection logic (Section 6, page 18)
  if (length(rejected_models) == 0) {
    # No cointegration detected
    conclusion <- "No cointegration"
    selected_model <- NULL
    
  } else if (length(rejected_models) == 1) {
    # Only one model rejects: select it
    conclusion <- "Cointegration detected"
    selected_model <- all_results[[rejected_models[1]]]
    
  } else {
    # Multiple models reject: select most general model that rejects
    # Priority: cs_b2 > cs_b1 > c_b2 > c_b1 > o
    
    priority <- c("cs_b2", "cs_b1", "c_b2", "c_b1", "o_b0")
    
    selected_model <- NULL
    for (mod_name in priority) {
      if (mod_name %in% rejected_models) {
        conclusion <- "Cointegration detected"
        selected_model <- all_results[[mod_name]]
        break
      }
    }
  }
  
  result <- list(
    conclusion = conclusion,
    selected_model = selected_model,
    all_results = all_results,
    model_selection_info = list(
      rejected_models = rejected_models,
      n_rejected = length(rejected_models),
      alpha = alpha,
      max_breaks = max_breaks
    )
  )
  class(result) <- "composite_coint_test"
  return(result)
}

#' Print Method for composite_coint_test Objects
#'
#' @param x An object of class "composite_coint_test"
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the input object x
#' @export
print.composite_coint_test <- function(x, ...) {
  cat("\nComposite Cointegration Test\n")
  cat("============================\n")
  cat("Models tested:", length(x$all_results), "\n")
  cat("Models rejecting null:", x$model_selection_info$n_rejected, "\n")
  cat("\nConclusion:", x$conclusion, "\n")
  
  if (!is.null(x$selected_model)) {
    cat("\nSelected Model:\n")
    cat("  Type:", x$selected_model$model, "\n")
    cat("  Breaks:", x$selected_model$n_breaks, "\n")
    if (!is.null(x$selected_model$break_dates)) {
      cat("  Break dates:", paste(x$selected_model$break_dates, collapse = ", "), "\n")
    }
    cat("  Test statistic:", round(x$selected_model$statistic, 4), "\n")
    cat("  Critical value:", round(x$selected_model$critical_value, 4), "\n")
  }
  cat("\n")
  invisible(x)
}

#' Summary Method for composite_coint_test Objects
#'
#' @param object An object of class "composite_coint_test"
#' @param ... Additional arguments (not used)
#' @return Invisibly returns a summary data frame with results for all models
#' @export
summary.composite_coint_test <- function(object, ...) {
  cat("\nDetailed Summary of All Models\n")
  cat("==============================\n\n")
  
  # Create summary table
  results_df <- data.frame(
    Model = names(object$all_results),
    Statistic = sapply(object$all_results, function(x) round(x$statistic, 4)),
    Critical_Value = sapply(object$all_results, function(x) round(x$critical_value, 4)),
    Reject_Null = sapply(object$all_results, function(x) x$reject_null),
    stringsAsFactors = FALSE
  )
  
  print(results_df, row.names = FALSE)
  
  cat("\n")
  cat("Rejected models:", paste(object$model_selection_info$rejected_models, collapse = ", "))
  cat("\n")
  cat("Selected model:", 
      if (!is.null(object$selected_model)) {
        paste0(object$selected_model$model, "_b", object$selected_model$n_breaks)
      } else {
        "None"
      })
  cat("\n\n")
  
  invisible(results_df)
}

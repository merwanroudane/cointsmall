# =============================================================================
# Verification Functions
# =============================================================================

#' Verify Critical Values Against Paper
#'
#' Compares computed critical values with Table 1 from Trinh (2022), page 8.
#' This function helps verify that the implementation matches the paper.
#'
#' @param tolerance Numeric. Maximum acceptable difference (default: 0.1).
#' @param verbose Logical. If TRUE, print detailed comparison (default: TRUE).
#'
#' @return If verbose=TRUE, invisibly returns a logical value: TRUE if all 
#'   values match within tolerance, FALSE otherwise. If verbose=FALSE, returns 
#'   a list with elements: all_match (logical), max_diff (numeric), tolerance 
#'   (numeric), and details (list of data frames with comparison results).
#'
#' @details
#' Verifies implementation against Table 1 (page 8) of the paper.
#' The default tolerance of 0.1 is used because values in the paper are 
#' reported to 2 decimal places.
#'
#' @references
#' Trinh, J. (2022). Testing for cointegration with structural changes in very 
#' small sample. THEMA Working Paper n°2022-01, Table 1, page 8.
#'
#' @examples
#' # Verify critical values match the paper (with output)
#' verify_critical_values()
#' 
#' # Verify with custom tolerance, suppress output and get detailed results
#' result <- verify_critical_values(tolerance = 0.05, verbose = FALSE)
#' print(result$all_match)
#' print(result$max_diff)
#'
#' @export
verify_critical_values <- function(tolerance = 0.1, verbose = TRUE) {
  
  # Values from Table 1 of the paper (page 8)
  table1 <- list(
    m1_b1_c = list(
      T = c(15, 20, 30, 50, 100, 500),
      CV = c(-5.85, -5.73, -5.37, -5.08, -4.83, -4.62)
    ),
    m1_b1_cs = list(
      T = c(15, 20, 30, 50, 100, 500),
      CV = c(-6.25, -6.08, -5.72, -5.40, -5.11, -4.96)
    ),
    m1_b2_c = list(
      T = c(15, 20, 30, 50, 100, 500),
      CV = c(-8.16, -7.11, -6.90, -6.04, -5.47, -5.21)
    ),
    m1_b2_cs = list(
      T = c(15, 20, 30, 50, 100, 500),
      CV = c(-8.87, -7.81, -7.47, -6.67, -6.24, -5.94)
    )
  )
  
  configs <- list(
    list(m = 1, b = 1, model = "c", name = "m1_b1_c"),
    list(m = 1, b = 1, model = "cs", name = "m1_b1_cs"),
    list(m = 1, b = 2, model = "c", name = "m1_b2_c"),
    list(m = 1, b = 2, model = "cs", name = "m1_b2_cs")
  )
  
  all_correct <- TRUE
  all_diffs <- numeric()
  detailed_results <- list()
  
  if (verbose) {
    message("\n", strrep("=", 80))
    message("VERIFICATION: Critical Values vs Table 1 (page 8)")
    message(strrep("=", 80))
  }
  
  for (cfg in configs) {
    if (verbose) {
      message(sprintf("\n%s (m=%d, b=%d, model=%s)", 
                     cfg$name, cfg$m, cfg$b, cfg$model))
      message(strrep("-", 70))
      message(sprintf("%8s %15s %15s %12s", "T", "Paper", "Computed", "Diff"))
      message(strrep("-", 70))
    }
    
    paper_data <- table1[[cfg$name]]
    config_results <- data.frame(
      T = paper_data$T,
      Paper = paper_data$CV,
      Computed = numeric(length(paper_data$T)),
      Diff = numeric(length(paper_data$T)),
      Match = logical(length(paper_data$T)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(paper_data$T)) {
      T_val <- paper_data$T[i]
      paper_cv <- paper_data$CV[i]
      computed_cv <- get_critical_value(T_val, cfg$m, cfg$b, cfg$model)
      diff <- abs(paper_cv - computed_cv)
      all_diffs <- c(all_diffs, diff)
      
      config_results$Computed[i] <- computed_cv
      config_results$Diff[i] <- diff
      config_results$Match[i] <- diff <= tolerance
      
      if (verbose) {
        status <- if (diff > tolerance) " [MISMATCH]" else " [OK]"
        message(sprintf("%8d %15.2f %15.2f %12.4f%s", 
                       T_val, paper_cv, computed_cv, diff, status))
      }
      
      if (diff > tolerance) {
        all_correct <- FALSE
      }
    }
    
    detailed_results[[cfg$name]] <- config_results
  }
  
  if (verbose) {
    message("\n", strrep("=", 80))
    if (all_correct) {
      message("SUCCESS: All critical values verified (max diff: ", 
             round(max(all_diffs), 4), ")")
    } else {
      message("WARNING: Some critical values do not match within tolerance")
      message("Max difference: ", round(max(all_diffs), 4))
    }
    message(strrep("=", 80), "\n")
    return(invisible(all_correct))
  } else {
    result <- list(
      all_match = all_correct,
      max_diff = max(all_diffs),
      tolerance = tolerance,
      details = detailed_results
    )
    return(result)
  }
}

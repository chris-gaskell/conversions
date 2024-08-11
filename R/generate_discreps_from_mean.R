#' Calculate score deviances and significance
#'
#' This function calculates the mean deviances of test scores from the mean,
#' evaluates their significance, and computes various related statistics.
#'
#' @param x A numeric vector of test scores.
#' @param R A numeric matrix representing the correlation matrix. If not provided, correlation-based calculations are skipped.
#' @param sem A numeric vector of standard error of measurement (SEM) values.
#' @param sd Standard deviation of the test scores. Default is 15.
#' @param dp Number of decimal places for rounding in the results. Default is 2.
#' @param names Optional character vector of names for the test scores. If not provided, default names will be used.
#' @param conf.level Confidence level for the confidence intervals. Default is 0.90.
#' @param threshold Threshold for abnormality detection. Default is -1.645.
#' @param apply_bonferroni Logical, whether to apply the Bonferroni correction. Default is FALSE.
#' @param alpha Significance level used in the Bonferroni correction. Default is 0.05.
#'
#' @return A list containing various statistics and results.
#'
#' @importFrom stats pnorm sd qnorm
#' @export
#'
#' @examples
#' sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
#' scores <- c(118, 107, 77, 68)
#' R <- matrix(c(1.00, 0.61, 0.64, 0.45,
#'               0.61, 1.00, 0.62, 0.52,
#'               0.64, 0.62, 1.00, 0.51,
#'               0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
#' names <- c("Verbal Comprehension", "Perceptual Reasoning",
#'            "Working Memory", "Processing Speed")
#' generate_discreps_from_mean(scores, R = R, sem = sem, conf.level = 0.95, names = names)
generate_discreps_from_mean <- function(x, R = NULL, sem, sd = 15, dp = 2, names = NULL, conf.level = 0.90, threshold = -1.645, apply_bonferroni = FALSE, alpha = 0.05) {
  k <- length(x)
  if (is.null(names)) {
    names <- as.character(seq(1, k))
  } else {
    names <- stringr::str_to_upper(abbreviate(names, minlength = 2, method = "left.kept"))
  }

  z.sd <- qnorm(1 - ((1 - conf.level) / 2))
  mean.x <- mean(x)

  sem.dev <- numeric(length(sem))
  for (i in 1:length(sem)) {
    sem.dev[i] <- sqrt(((k - 2) / k) * sem[i]^2 + (1 / k^2) * sum(sem^2))
  }

  dev <- x - mean.x
  dev.ci.lb <- dev - sem.dev * z.sd
  dev.ci.ub <- dev + sem.dev * z.sd

  crit.vals <- sem.dev * z.sd
  signif.below <- dev < -crit.vals
  signif.either <- abs(dev) > crit.vals

  result <- list(
    k = k,
    z.sd = z.sd,
    names = names,
    mean.x = mean.x,
    dev = round(dev, dp),
    dev.ci.lb = round(dev.ci.lb, dp),
    dev.ci.ub = round(dev.ci.ub, dp),
    sem.dev = round(sem.dev, dp),
    crit.vals = round(crit.vals, dp),
    signif.below = signif.below,
    signif.either = signif.either
  )

  if (!is.null(R)) {
    G.bar <- mean(R)  # Mean of all elements in the correlation matrix
    h.a.bar <- rowMeans(R)  # Mean of each row of the matrix
    sd.dev <- sd * sqrt(1 + G.bar - 2 * h.a.bar)
    crit.val.sd <- sd.dev * z.sd
    sig.diffs <- abs(dev) > crit.val.sd

    p.vals.1t <- 1 - pnorm(abs(dev) / sem.dev)
    p.vals.2t <- p.vals.1t * 2

    # Apply sequential Bonferroni correction if requested
    if (apply_bonferroni) {
      bonferroni_results_1t <- sequential_bonferroni(p.vals.1t, alpha = alpha)
      p.vals.1t <- bonferroni_results_1t$adj.p.vals
      sig.diffs.1t <- bonferroni_results_1t$sig

      bonferroni_results_2t <- sequential_bonferroni(p.vals.2t, alpha = alpha)
      p.vals.2t <- bonferroni_results_2t$adj.p.vals
      sig.diffs.2t <- bonferroni_results_2t$sig
    }

    abn.1t <- round(100 - pnorm(abs(dev) / sd.dev) * 100, 4)
    abn.2t <- round(100 - pnorm(abs(dev) / sd.dev) * 100, 4) * 2
    abn.k <- sum(abn.2t < 5)

    # Assuming abnormal_rate_j_battery function is defined elsewhere
    abn.rates <- conversions::prevalence_of_j_abnormal_scores(R = R, threshold = threshold)
    abn <- abn.rates$props[abn.k]

    # Add additional items to the result
    result <- c(result, list(
      prev.1t = round(abn.1t, dp),
      prev.2t = round(abn.2t, dp),
      pval.1t = round(p.vals.1t, dp),
      pval.2t = round(p.vals.2t, dp),
      abn.k = abn.k,
      abn = abn,
      abn.rates = abn.rates,
      sig.diffs = sig.diffs
    ))
  }

  class(result) <- 'mean_devs'
  return(result)
}

#' Print Method for Mean Deviances
#'
#' Custom print method for objects of class 'mean_devs'. This function
#' formats and prints the calculated deviances, confidence intervals,
#' p-values, and other related statistics using knitr::kable for better formatting.
#'
#' @param x An object of class 'mean_devs'.
#' @param ... Additional arguments passed to other methods.
#' @export
print.mean_devs <- function(x, ...) {
  # Create a data frame with all the relevant statistics
  result_df <- data.frame(
    Test = x$names,
    Deviation = x$dev,
    CI = paste0(x$dev.ci.lb, " to ", x$dev.ci.ub),
    `p-val 2t` = format.pval(x$pval.2t, digits = x$dp, eps = .0001),
    `p-val 1t` = format.pval(x$pval.1t, digits = x$dp, eps = .0001),
    `Abnormal 2d` = x$prev.2t,
    `Abnormal 1d` = x$prev.1t,
    stringsAsFactors = FALSE
  )

  # Print the header
  header <- paste(
    "ABNORMALITY of Index score deviations from the case's mean Index score:\n",
    "Case's mean Index score: ", x$mean.x, "\n\n", sep = ""
  )
  cat(header)

  # Use knitr::kable to print the data frame as a table
  output_table <- knitr::kable(result_df, format = "simple", align = "c")
  cat(output_table, sep = "\n")

  # Additional information about abnormality
  if (!is.null(x$abn.rates)) {
    abn_k <- glue::glue("NUMBER of case's deviation scores that meet criterion for abnormality = {x$abn.k}")
    abn <- glue::glue("\n\nPERCENTAGE of normal population expected to exhibit this number or more of abnormally low scores = {x$abn}%")
    cat("\n", abn_k, "\n", abn, "\n", sep = "")
  }
}


# # Example Usage
# sem <- c(3.000000, 3.354102, 3.674235, 4.743416)
# R <- matrix(c(1.00, 0.61, 0.64, 0.45,
#               0.61, 1.00, 0.62, 0.52,
#               0.64, 0.62, 1.00, 0.51,
#               0.45, 0.52, 0.51, 1.00), nrow = 4, byrow = TRUE)
# scores <- c(118, 107, 77, 68)
# scores <- c(80, 100, 100, 100)
# names <- c("Verbal Comprehension", "Perceptual Reasoning",
#            "Working Memory", "Processing Speed")
#
# # Generate the results
# results <- generate_discreps_from_mean(x = c(60, 100, 100, 100), R = R, sem = sem,
#                                        conf.level = 0.95, names = names,
#                                        apply_bonferroni = F, dp = 3
#                                        )
#
# # Print the results using the custom print method
# print(results)

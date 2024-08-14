dissociation_bayes_single <- function(
    score,
    ctrl.mean,
    ctrl.sd,
    ctrl.n,
    conf.level = .95,
    direction = "lower",
    dp = 4,
    sims = 10000,
    treshold = 0.1) {

  df <-  ctrl.n-1
  ctrl.var <- ctrl.sd^2

  p.sims <- c()
  for (i in 1:sims) {
    psi <- rchisq(1, df = df, ncp = 0)
    o <- (ctrl.n - 1) * ctrl.var / psi
    z.sim <- rnorm(1, 0, 1)
    u <- ctrl.mean + z.sim * sqrt((o / ctrl.n))
    z <- (score - u) / sqrt(o)

    # Calculate p-value based on the specified direction
    if (direction == "lower") {
      p <- pnorm(z, lower.tail = TRUE)  # Lower tail probability
    } else if (direction == "higher") {
      p <- pnorm(z, lower.tail = FALSE)  # Lower tail probability
    }


    p.sims <- c(p.sims, p)
  }

  z.scores <- c()
  for (i in 1:sims) {
    psi <- rchisq(1, df = df, ncp = 0)
    o <- (ctrl.n - 1) * ctrl.var / psi
    z.sim <- rnorm(1, 0, 1)
    u <- ctrl.mean + z.sim * sqrt((o / ctrl.n))
    z <- (score - u) / sqrt(o)
    z.scores <- c(z.scores, z)
  }

  z <- (score - ctrl.mean) / ctrl.sd
  percentile <- pnorm(z)*100
  z.ci <- bayestestR::hdi(z.scores, ci = conf.level)
  z.ci.lb <- z.ci$CI_low
  z.ci.ub <- z.ci$CI_high

  p <- mean(p.sims)
  p.two.tailed <- 2 * min(p, 1 - p)

  abn <- p * 100
  abn.ci <- bayestestR::hdi(p.sims, ci = conf.level)
  abn.ci.lb <- min(abn.ci$CI_low, abn.ci$CI_high) * 100
  abn.ci.ub <- max(abn.ci$CI_low, abn.ci$CI_high) * 100

  result <- list(
    score = score,
    ctrl.mean = ctrl.mean,
    ctrl.sd = ctrl.sd,
    ctrl.n = ctrl.n,
    conf.level = conf.level,
    sims = sims,
    treshold = treshold,
    direction = direction,
    dp = dp,
    df = df,
    ctrl.var = ctrl.var,
    z = round(z, dp),
    z.ci.lb = round(z.ci.lb, dp),
    z.ci.ub = round(z.ci.ub, dp),
    percentile = round(percentile, dp),
    p = round(p, dp),
    p.two.tailed = round(p.two.tailed, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp)
  )

  class(result) <- 'dissociation_bayes_single'
  return(result)
}

#' @export
print.dissociation_bayes_single <- function(x, ...) {

  input_df <- data.frame(
    Item = c("Sample mean", "Sample SD", "Sample size", "Case's test score"),
    Value = c(x$ctrl.mean, x$ctrl.sd, x$ctrl.n, x$score),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    Item = c(
      paste("p-val (", x$direction, ")", sep = ""),
      "p-val (either direction)",
      "Effect size (Z-CC)",
      "Abnormality"
    ),
    Value = c(
      format(x$p, nsmall = 4),
      format(x$p.two.tailed, nsmall = 4),
      format(x$z, nsmall = 4),
      paste(format(x$abn, nsmall = 4), "%", sep = "")
    ),
    CI = c(
      "",
      "",
      paste(x$z.ci.lb, "to", x$z.ci.ub, sep = " "),
      paste(x$abn.ci.lb, "% to", x$abn.ci.ub, "%", sep = " ")
    ),
    stringsAsFactors = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Inputs", "Value"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Outputs", "Value", glue::glue("{x$conf.level*100}% Confidence Interval")))

  header <- "Bayesian Single Dissociation"
  description <- "Bayesian estimation of effect size and abnormality for a patient's score."
  reference <- "Based on Bayesian methods for abnormality testing."

  result <- paste(header, "\n\n",
                  description, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  reference, "\n",
                  sep = "")

  cat(result)
}

# Example usage:
# print.dissociation_bayes_single(list(ctrl.mean = 100, ctrl.sd = 15, ctrl.n = 30, score = 130, z = 2.5, p = 0.02, p.two.tailed = 0.04, z.ci.lb = 1.5, z.ci.ub = 3.5, abn = 5.5, abn.ci.lb = 3.5, abn.ci.ub = 7.5, direction = "lower"))

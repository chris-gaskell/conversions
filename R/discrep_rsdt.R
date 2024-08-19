#' RSDT
#'
#' description.
#'
#' @details ...
#'
#' @inheritParams dissoc_single
#' @inheritParams dissoc_discrep
#'
#' @return A list of statistical input, parameters, and results. Key outputs
#'   include:
#'   - t value: The t-value calculated for the test.
#'   - p value: The p-value for the test, indicating statistical significance.
#'   - effect-size (z-cc): The z-score (effect-size) corrected for the control group.
#'   - abnormality: The percentage of the population expected to score a more extreme score.
#' @references
#'   - To complete
#'   - To complete
#' @seealso
#'   - [dissoc_discrep()]:
#'   - [dissoc_single()]:
#' @export
#' @examples
#' discrep_rsdt(100, 15, 110, 10, 0.5, 30, 130, 120, test.names = c("Fluency", "Sequencing"))

discrep_rsdt <- function(ctrl.mean.x,
                         ctrl.sd.x,
                         ctrl.mean.y,
                         ctrl.sd.y,
                         ctrl.r.xy,
                         ctrl.n,
                         score.x,
                         score.y,
                         direction = "lower",
                         tail = "one.tailed",
                         test.names = c("test X", "test Y"),
                         conf.level = 0.95,
                         dp = 2
) {
  z.x <- (score.x - ctrl.mean.x) / ctrl.sd.x
  z.y <- (score.y - ctrl.mean.y) / ctrl.sd.y

  df <- ctrl.n - 1

  # Calculate the terms a, b, c for the t-statistic using the rsdt approach
  a <- (1 + ctrl.r.xy) * (1 - ctrl.r.xy^2)
  b <- (1 - ctrl.r.xy) * (
    4*(ctrl.n - 1)^2 + 4*(1 + ctrl.r.xy)*(ctrl.n - 1) + (1 + ctrl.r.xy)*(5 + ctrl.r.xy)
  )
  c <- -2 * (
    ((score.x - ctrl.mean.x)/ctrl.sd.x - (score.y - ctrl.mean.y)/ctrl.sd.y)^2 *
      (ctrl.n*(ctrl.n - 1)^2)/(ctrl.n + 1)
  )

  t <- sqrt((-b + sqrt(b^2 - 4*a*c)) / (2*a))
  t <- round(t, dp) # round the t-statistic

  # Determine p-value based on direction and tail
  if (tail == "one.tailed") {
    if (direction == "lower") {
      p.value <- pt(t, df = df, lower.tail = TRUE)
    } else if (direction == "higher") {
      p.value <- pt(t, df = df, lower.tail = FALSE)
    } else {
      stop("Invalid direction. Use 'lower' or 'higher'.")
    }
  } else if (tail == "two.tailed") {
    p.value <- 2 * pt(-abs(t), df = df)
  } else {
    stop("Invalid tail. Use 'one.tailed' or 'two.tailed'.")
  }

  p.value <- round(p.value, dp) # round the p-value

  # Calculate abnormality based on one-tailed p-value
  if (direction == "lower") {
    abn <- pt(t, df = df, lower.tail = TRUE) * 100
  } else if (direction == "higher") {
    abn <- pt(t, df = df, lower.tail = FALSE) * 100
  }
  zdcc <- (z.x - z.y) / sqrt(2 - 2*ctrl.r.xy)
  ncp <- neuropsytools::abnorm_ci_t(c = zdcc, n = ctrl.n)
  zdcc.ci.lb <- ncp$delta.lb$root / sqrt(ctrl.n)
  zdcc.ci.ub <- ncp$delta.ub$root / sqrt(ctrl.n)

  abn.ci <- c(as.numeric(ncp$`2.5%`), as.numeric(ncp$`97.5%`))
  if (direction == "higher") {abn.ci <- 100 - abn.ci}
  abn.ci.lb <- min(abn.ci)
  abn.ci.ub <- max(abn.ci)

  result <- list(
    test.names = test.names,
    ctrl.mean.x = ctrl.mean.x,
    ctrl.sd.x = ctrl.sd.x,
    ctrl.mean.y = ctrl.mean.y,
    ctrl.sd.y = ctrl.sd.y,
    ctrl.r.xy = ctrl.r.xy,
    ctrl.n = ctrl.n,
    score.x = score.x,
    score.y = score.y,
    conf.level = conf.level,
    direction = direction,
    z.x = z.x,
    z.y = z.y,
    t = round(t, dp),
    p.value = round(p.value, dp),
    zdcc = round(zdcc, dp),
    zdcc.ci.lb = round(zdcc.ci.lb, dp),
    zdcc.ci.ub = round(zdcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp),
    direction = direction,
    dp = dp
  )

  class(result) <- 'discrep_rsdt'
  return(result)
}

#' @export
print.discrep_rsdt <- function(x, ...) {

  input_df <- data.frame(
    test =  x$test.names,
    mean =  c(x$ctrl.mean.x, x$ctrl.mean.y),
    sd =  c(x$ctrl.sd.x, x$ctrl.sd.y),
    n = c(x$ctrl.n, ""),
    r = c(x$ctrl.r.xy, ""),
    value = c(x$score.x, x$score.y),
    stringsAsFactors = FALSE
  )

  output_df <- data.frame(
    item = c(paste("Effect size (z) for ", x$test.names[1]),
             paste("Effect size (z) for ", x$test.names[2]),
             paste("Effect size (z-dcc) between", x$test.names[1], "and", x$test.names[2]),
             "t value",
             "p-value",
             "Abnormality"
    ),
    value = c(format(x$z.x, nsmall = x$dp),
              format(x$z.y, nsmall = x$dp),
              format(x$zdcc, nsmall = x$dp),
              format(x$t, nsmall = x$dp),
              format(x$p.value, nsmall = x$dp),
              paste(format(x$abn, nsmall = x$dp), " %", sep = "")
    ),
    ci = c("",
           "",
           paste(format(x$zdcc.ci.lb, nsmall = x$dp), " to ", format(x$zdcc.ci.ub, nsmall = x$dp), sep = ""),
           "",
           "",
           paste(x$abn.ci.lb, " % to ", x$abn.ci.ub, " %", sep = "")
    ), stringsAsFactors = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Test", "Mean", "SD", "Sample size", "r", "Case score"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Variable", "Value", paste0(x$conf.level * 100, "% Confidence Interval")))
  header <- "Frequentist Dissociation Between Two Test Scores Compared to a Control Sample (RSDT Approach)."
  footnote <- "See documentation for further information on how scores are computed."

  result <- paste(header, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  footnote, "\n",
                  sep = "")

  cat(result)
}

# y <- qt(p = 0.975, df = df)
# numerator <- (score.x - ctrl.mean.x) / ctrl.sd.x - (score.y - ctrl.mean.y) / ctrl.sd.y
# denominator <-
#   sqrt(
#     ((ctrl.n + 1) / ctrl.n) *
#       (
#         (2 - 2*ctrl.r.xy) +
#           (2 * (1 - ctrl.r.xy^2)) / (ctrl.n - 1) +
#           (5 + y^2) * (1 - ctrl.r.xy^2) / (2 * (ctrl.n - 1)^2) +
#           (ctrl.r.xy * (1 + y^2) * (1 - ctrl.r.xy^2)) / (2 * (ctrl.n - 1)^2)
#       )
#   )
# RSDT <- numerator / denominator

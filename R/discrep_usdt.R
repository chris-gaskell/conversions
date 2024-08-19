#' USDT
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
#' discrep_usdt(100, 15, 110, 10, 0.5, 30, 130, 120, test.names = c("Fluency", "Sequencing"))
discrep_usdt <- function(ctrl.mean.x,
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
  X_bar <- ctrl.mean.x
  s2x <- ctrl.sd.x^2
  y_bar <- ctrl.mean.y
  s2y <- ctrl.sd.y^2
  x <- ctrl.r.xy
  n <- ctrl.n
  Xstar <- score.x
  ystar <- score.y
  cov_xy <- ctrl.sd.x * ctrl.sd.y * ctrl.r.xy

  # Calculate the t-statistic using the USDT approach
  t.usdt <-
    ((Xstar - X_bar) - (ystar - y_bar)) /
    sqrt(
      (s2x + s2y - 2*cov_xy) * # variance of diff for controls
        ((n + 1) / n)
    )

  df <- n - 1
  t.usdt <- round(t.usdt, dp) # round the t-statistic

  # Determine p-value based on direction and tail
  if (tail == "one.tailed") {
    if (direction == "lower") {
      p.value <- pt(t.usdt, df = df, lower.tail = TRUE)
    } else if (direction == "higher") {
      p.value <- pt(t.usdt, df = df, lower.tail = FALSE)
    } else {
      stop("Invalid direction. Use 'lower' or 'higher'.")
    }
  } else if (tail == "two.tailed") {
    p.value <- 2 * pt(-abs(t.usdt), df = df)
  } else {
    stop("Invalid tail. Use 'one.tailed' or 'two.tailed'.")
  }

  p.value <- round(p.value, dp) # round the p-value

  # Calculate abnormality based on one-tailed p-value
  if (direction == "lower") {
    abn <- pt(t.usdt, df = df, lower.tail = TRUE) * 100
  } else if (direction == "higher") {
    abn <- pt(t.usdt, df = df, lower.tail = FALSE) * 100
  }
  zdcc <- (Xstar - X_bar - (ystar - y_bar)) / sqrt(s2x + s2y - 2*cov_xy)
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
    t = round(t.usdt, dp),
    p.value = round(p.value, dp),
    z.x = z.x,
    z.y = z.y,
    zdcc = round(zdcc, dp),
    zdcc.ci.lb = round(zdcc.ci.lb, dp),
    zdcc.ci.ub = round(zdcc.ci.ub, dp),
    abn = round(abn, dp),
    abn.ci.lb = round(abn.ci.lb, dp),
    abn.ci.ub = round(abn.ci.ub, dp),
    direction = direction,
    dp = dp
  )

  class(result) <- 'discrep_usdt'
  return(result)
}

#' @export
print.discrep_usdt <- function(x, ...) {

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
  header <- "Frequentist Dissociation Between Two Test Scores Compared to a Control Sample (USDT Approach)."
  footnote <- "See documentation for further information on how scores are computed."

  result <- paste(header, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  footnote, "\n",
                  sep = "")

  cat(result)
}

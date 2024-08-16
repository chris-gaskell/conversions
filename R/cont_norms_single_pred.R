#' Generate Regression Norms for a Single Variable
#'
#' This function generates regression norms and performs significance testing for an individual's score on a predictor variable. It calculates the regression equation, standard errors, and significance levels based on summary statistics from a control sample.
#'
#' @param ctrl.x.mean Mean of the predictor variable in the control sample.
#' @param ctrl.x.sd Standard deviation of the predictor variable in the control sample.
#' @param ctrl.y.mean Mean of the criterion variable in the control sample.
#' @param ctrl.y.sd Standard deviation of the criterion variable in the control sample.
#' @param r Correlation coefficient between the predictor and criterion variables in the control sample.
#' @param n Sample size of the control group.
#' @param x Score of the individual on the predictor variable.
#' @param y Obtained score of the individual on the criterion variable.
#' @param conf.level Confidence level for the significance test. Default is 0.05.
#' @param direction Direction for the significance test. Options are "lower" or "upper". Default is "lower".
#' @param dp Number of decimal places.
#' @param x.name Name for the predictor variable. Default is "X".
#' @param y.name Name for the criterion variable. Default is "Y".
#' @return A list containing calculated values and results.
#'
#' @importFrom glue glue
#' @importFrom stats pt
#' @export
#'
#' @examples
#' cont_norms_single_pred(
#'   ctrl.x.mean = 63.8,
#'   ctrl.x.sd = 8.42,
#'   ctrl.y.mean = 41.3,
#'   ctrl.y.sd = 13.2,
#'   r = -0.58,
#'   n = 160,
#'   x = 26,
#'   y = 41,
#'   x.name = "Predictor",
#'   y.name = "Criterion"
#' )
cont_norms_single_pred <- function(ctrl.x.mean,
                                                 ctrl.x.sd,
                                                 ctrl.y.mean,
                                                 ctrl.y.sd,
                                                 r,
                                                 n,
                                                 x,
                                                 y,
                                                 conf.level = 0.05,
                                                 direction = "lower",
                                                 dp = 4,
                                                 x.name = "X",
                                                 y.name = "Y") {

  # Validate the x.name and y.name arguments
  if (length(x.name) != 1 || length(y.name) != 1) {
    stop("x.name and y.name must be character vectors of length 1.")
  }

  # Stats calculations
  df <- n - 1 - 1
  b <- r * (ctrl.y.sd / ctrl.x.sd)  # Slope
  a <- ctrl.y.mean - (b * ctrl.x.mean)    # Intercept
  s.yx <- ctrl.y.sd * sqrt((1 - r^2) * ((n - 1) / (n - 2)))  # SE estimate
  se.n.1 <- s.yx * sqrt(1 + (1 / n) + (((x - ctrl.x.mean)^2) / ((ctrl.x.sd^2) * (n - 1))))  # SE with one entering
  y.est <- a + (b * x)  # Predicted score
  disc <- y - y.est     # Discrepancy
  t <- disc / se.n.1    # t-value
  p <- pt(t, df = df, lower.tail = TRUE)  # p-value (one-tailed)
  z.op <- disc / (ctrl.y.sd * sqrt(1 - r^2))  # Z-OP

  # Calculate abn percentage
  abn <- (abs(1 - p)) * 100
  if (direction == "lower") {
    abn <- (1 - abs(1 - p)) * 100
  }

  result <- list(
    ctrl.x.mean = ctrl.x.mean,
    ctrl.x.sd = ctrl.x.sd,
    ctrl.y.mean = ctrl.y.mean,
    ctrl.y.sd = ctrl.y.sd,
    r = r,
    n = n,
    x = x,
    y = y,
    conf.level = conf.level,
    direction = direction,
    dp = dp,
    df = df,
    b = round(b, dp),
    a = round(a, dp),
    s.yx = round(s.yx, dp),
    se.n.1 = round(se.n.1, dp),
    y.est = round(y.est, dp),
    disc = round(disc, dp),
    t = round(t, dp),
    p.one.tailed = round(p, dp),
    p.two.tailed = round(p*2, dp),
    z.op = round(z.op, dp),
    abn = round(abn, dp),
    x.name = x.name,
    y.name = y.name
  )

  class(result) <- 'cont_norms_single'
  return(result)
}




#' @export
print.cont_norms_single <- function(x, ...) {


  # Input Data Frame with customizable names
  input_df <- data.frame(
    Variable = c(paste("Predictor (", x$x.name, ")", sep = ""),
                 paste("Criterion (", x$y.name, ")", sep = "")),
    Mean = c(x$ctrl.x.mean, x$ctrl.y.mean),
    SD = c(x$ctrl.x.sd, x$ctrl.y.sd),
    `Case's Score` = c(x$x, x$y),
    n = c(x$n, ""),
    r = c(x$r, ""),
    `t1 Score` = c(x$x, x$y),  # Adjust this if t1 score is used elsewhere
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Output Data Frame
  output_df <- data.frame(
    Item = c("Regression equation (Predicted Y)",
             "Standard error of estimate",
             "Case's PREDICTED score",
             "Discrepancy (Observed - Predicted)",
             "Effect size (Z-OP)",
             "t value",
             "One-tailed p-value",
             "Two-tailed p-value",
             "Estimated % with more extreme discrepancy"
    ),
    Value = c(paste(x$a, "+", x$b, "*", x$x.name),
              format(x$s.yx, nsmall = 4),
              format(x$y.est, nsmall = 4),
              format(x$disc, nsmall = 4),
              format(x$z.op, nsmall = 4),
              format(x$t, nsmall = 4),
              format(x$p.one.tailed, nsmall = 4),
              format(x$p.two.tailed, nsmall = 4),
              paste(format(x$abn, nsmall = 4), "%")
    ),
    stringsAsFactors = FALSE
  )

  # Create the input and output tables
  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Variable", "Mean", "SD", "Case's Score", "n", "r", "t1 Score"))
  output_table <- knitr::kable(output_df, format = "simple", col.names = c("Outputs", "Value"))

  # Define the header and description
  header <- "Neuropsychological Regression Norms Single Case Analysis"
  description <- "Regression norms and significance testing for an individual case based on the control sample."
  reference <- "Based on: Crawford, Garthwaite, & Porter (2010), Crawford & Garthwaite (2002), and Crawford & Howell (1998)."

  # Combine all parts into the final result
  result <- paste(header, "\n\n",
                  description, "\n\n",
                  "INPUTS:", paste(capture.output(input_table), collapse = "\n"), "\n\n",
                  "OUTPUTS:", paste(capture.output(output_table), collapse = "\n"), "\n\n",
                  reference, "\n",
                  sep = "")

  # Print the result
  cat(result)
}


# Example usage
cont_norms_single_pred(
  ctrl.x.mean = 63.8,
  ctrl.x.sd = 8.42,
  ctrl.y.mean = 41.3,
  ctrl.y.sd = 13.2,
  r = -0.58,
  n = 160,
  x = 26,
  y = 41, dp = 4, x.name = "age", y.name = "fluency"
)

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
#' @return A formatted character string containing:
#' \item{b}{Slope of the regression line.}
#' \item{a}{Intercept of the regression line.}
#' \item{s.yx}{Standard error of the estimate for the regression equation.}
#' \item{se.n.1}{Standard error of estimate with one entering.}
#' \item{y.est}{Predicted score based on the regression equation.}
#' \item{disc}{Discrepancy between obtained and predicted scores.}
#' \item{z.op}{Effect size (Z-OP).}
#' \item{t}{t-value for the significance test.}
#' \item{p.1t}{One-tailed p-value for the significance test.}
#' \item{p.two.tailed}{Two-tailed p-value for the significance test.}
#' \item{abn}{Estimated percentage of the population with a discrepancy more extreme than the individual's score.}
#'
#' @importFrom glue glue
#' @importFrom stats pt
#' @export
#'
#' @examples
#' generate_regression_norms_single_var(
#'   ctrl.x.mean = 63.8,
#'   ctrl.x.sd = 8.42,
#'   ctrl.y.mean = 41.3,
#'   ctrl.y.sd = 13.2,
#'   r = -0.58,
#'   n = 160,
#'   x = 26,
#'   y = 41
#' )
generate_regression_norms_single_var <- function(ctrl.x.mean,
                                                 ctrl.x.sd,
                                                 ctrl.y.mean,
                                                 ctrl.y.sd,
                                                 r,
                                                 n,
                                                 x,
                                                 y,
                                                 conf.level = 0.05,
                                                 direction = "lower",
                                                 dp = 4) {

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
    abn = round(abn, dp)
  )

  class(result) <- 'cont_norms_single'
  return(result)
}


#' @export
print.cont_norms_single <- function(x, ...) {

  header <-  paste(
    "Crawford, J.R., & Garthwaite, P.H. (2007). Using regression equations built from summary data in the neuropsychological assessment of the individual case. Neuropsychology, 21, 611-620.\n", sep = "\n"
  )

  input <- glue::glue(
    "INPUTS:\n",
    "Data from control sample used to build regression equation:\n",
    "Mean for predictor variable (X):   {x$ctrl.x.mean}\n",
    "SD for predictor variable (X):     {x$ctrl.x.sd}\n",
    "Mean for criterion variable (Y):   {x$ctrl.y.mean}\n",
    "SD for criterion variable (Y):     {x$ctrl.y.sd}\n",
    "r between predictor and criterion: {x$r}\n",
    "Sample size:                       {x$n}\n",
    "Case's score for predictor (X):    {x$x}\n",
    "Case's score for criterion (Y):    {x$y}\n\n"
  )

  output <- glue::glue(
    "OUTPUTS:\n",
    "Regression equation:              Predicted Y: {x$a} + {x$b}*{x$x}\n",
    "Standard error of estimate:       {x$s.yx}\n",
    "Case's OBTAINED score:            {x$y}\n",
    "Case's PREDICTED score:           {x$y.est}\n",
    "Discrepancy:                      {x$disc}\n",
    "Effect size (Z-OP):               {x$z.op}\n",
    "t value:                          {x$t} (df = {x$df})\n",
    "One-tailed pval:                  {x$p.one.tailed}\n",
    "Two-tailed pval:                  {x$p.two.tailed}\n\n",
    "Estimated percentage of population obtaining a discrepancy more extreme than case = {x$abn}%\n"
  )

  results <- paste(header, input, output,# output.one.tailed, output.two.tailed,
                   sep = "\n"
  )
  cat(results)
}



generate_regression_norms_single_var(
  ctrl.x.mean = 63.8,
  ctrl.x.sd = 8.42,
  ctrl.y.mean = 41.3,
  ctrl.y.sd = 13.2,
  r = -0.58,
  n = 160,
  x = 26,
  y = 41, dp = 4
)

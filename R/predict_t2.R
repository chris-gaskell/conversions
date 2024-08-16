#' Estimate Neuropsychological Test Scores Based on Earlier Administrations
#'
#' This function calculates predicted values for neuropsychological tests based
#' on scores from an earlier administration along with stability data (M, SD,
#' test-retest) from a normative sample. It allows the user to determine if the
#' change in a score between two time points is statistically significant.
#'
#' Note that this function is an intermediary function and that the function
#' XXXX will be more suited to estimating and analysing stability or change.
#'
#' @details The function supports multiple methods for calculating reliable
#'   change, including those proposed by Crawford & Garthwaite (2006), Jacobson
#'   & Truax (1991), Speer (1992), Chelune et al. (1993), Iverson et al. (2001),
#'   McSweeny et al. (1993), Maassen et al. (2006), and Charter (1996). By
#'   default, it uses the Crawford method (Crawford & Garthwaite, 2006).
#'
#'   The available methods differ in their approach to estimating the predicted
#'   value and the associated standard error. While all of the included
#'   approaches account for reliability of the test (i.e,. test-retest), only
#'   some of the approaches account for practice effects and regression to the
#'   mean. Readers who want to consider the merits of each approach are invited
#'   to read the introductory papers provided by Hinton-Bayre (2010) and Duff
#'   (2012).
#'
#' @param method Character string specifying the method used for reliable change
#'   estimate calculation. Options include "crawford", "jacobson", "speer",
#'   "chelune", "mcsweeny", "charter", "CH", "temkin", "iverson", and "maassen".
#'   Default is "crawford".
#' @param t1.score Numeric value of the score at time 1.
#' @param norm.t1.mean Numeric value of the mean from the normative data at time
#'   1.
#' @param norm.t1.sd Numeric value of the standard deviation from the normative
#'   data at time 1.
#' @param norm.t2.mean Numeric value of the mean from the normative data at time
#'   2.
#' @param norm.t2.sd Numeric value of the standard deviation from the normative
#'   data at time 2.
#' @param norm.r Numeric value of the Pearson correlation coefficient between
#'   time 1 and time 2 scores.
#' @param norm.n Numeric value of the sample size of the normative data.
#' @param dp Integer specifying the number of decimal places to round the
#'   converted scores to. Default is 2.
#' @param conf.level Confidence level for stability assessment.
#'
#' @return A list containing the following components:
#' \itemize{
#'   \item \code{method}: The method used for calculation.
#'   \item \code{t1.score}: The input score at time 1.
#'   \item \code{t2.expected}: The expected score at time 2 based on the method used.
#'   \item \code{t2.sem}: The standard error of measurement for the time 2 score.
#'   \item \code{t2.ci.lb}: The lower bound of the confidence interval for the
#'     expected score at time 2.
#'   \item \code{t2.ci.ub}: The upper bound of the confidence interval for the
#'     expected score at time 2.
#' }
#'
#' @examples
#' # Calculate reliable change using the Jacobson method
#' predict_t2(
#'   method = "jacobson",
#'   t1.score = 100,
#'   norm.t1.mean = 103,
#'   norm.t1.sd = 10,
#'   norm.t2.mean = 109,
#'   norm.t2.sd = 8,
#'   norm.r = 0.76,
#'   norm.n = 100,
#'   conf.level = 0.95
#' )
#'
#' @references
#'   - Jacobson, N.S., & Truax, P. (1991). Clinical significance: A statistical approach to defining meaningful change in psychotherapy research. *Journal of Consulting and Clinical Psychology, 59*(1), 12-19.
#'   - Duff, K. (2012). Evidence-Based Indicators of Neuropsychological Change in the Individual Patient: Relevant Concepts and Methods. *Archives of Clinical Neuropsychology, 27*(3), 248–261. doi:10.1093/arclin/acr120. PMCID: PMC3499091.
#'   - Hinton-Bayre, A. D. (2010). Deriving Reliable Change Statistics from Test–Retest Normative Data: Comparison of Models and Mathematical Expressions. *Archives of Clinical Neuropsychology, 25*(3), 244–256. doi:10.1093/arclin/acq008.
#'   - Crawford, J. R., & Garthwaite, P. H. (2006). Comparing patients’ predicted test scores from a regression equation with their obtained scores: A significance test and point estimate of abnormality with accompanying confidence limits. *Neuropsychology, 20*, 259-271.
#' @export
predict_t2 <- function(method = "crawford", t1.score,
                      norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
                      norm.r, norm.n, conf.level = .95, dp = 2) {
  change.methods <- c("jacobson", "speer", "chelune","mcsweeny",
                      "charter", "CH", "temkin", "iverson", "maassen", "crawford")

  if (missing(method)) {stop("'method' argument for calculating predict_t2 required.")}
  if (missing(t1.score) || missing(norm.t1.mean) || missing(norm.t1.sd) || missing(norm.t2.mean) || missing(norm.t2.sd) || missing(norm.r) || missing(norm.n)) {stop("Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")}


  if (!(method %in% change.methods)) {
    stop(
      call. = F,
      glue::glue('"{method}" is not an accepted reliable change method. Please select from the available methods ({stringr::str_flatten(change.methods, collapse = ", ")})')
    )
  }

  t2.expected <-
    dplyr::case_when(
      method == "jacobson" ~ round(t1.score, 2),
      method == "speer" ~ round(norm.t2.mean + norm.r * (t1.score - norm.t1.mean), 2),
      method == "chelune" ~ round(t1.score + (norm.t2.mean - norm.t1.mean), 2),
      method == "mcsweeny" ~ round((norm.r * (norm.t2.sd / norm.t1.sd)) * t1.score + norm.t2.mean - ((norm.r * (norm.t2.sd / norm.t1.sd)) * norm.t1.mean), 2),
      method == "charter" ~ round(norm.t2.mean + norm.r * (t1.score - norm.t1.mean), 2),
      method == "temkin"  ~ round(t1.score + (norm.t2.mean - norm.t1.mean), 2),
      method == "iverson" ~ round(t1.score + (norm.t2.mean - norm.t1.mean), 2)
    )

  if (method == "CH") estimated_beta <- norm.r * (norm.t2.sd / norm.t1.sd)
  if (method %in% c("maassen", "crawford")) estimated_beta <- norm.t2.sd / norm.t1.sd
  if (method %in% c("maassen", "crawford", "CH")) estimated_constant <- norm.t2.mean - estimated_beta * norm.t1.mean
  if (method %in% c("maassen", "crawford", "CH")) t2.expected <- round(estimated_beta * t1.score + estimated_constant, 2)

  t2.sem <-
    dplyr::case_when(
      method == "jacobson" ~ round(sqrt(2 * norm.t1.sd^2 * (1 - norm.r)), 2), # SEM
      method == "speer" ~    round(sqrt(2 * norm.t1.sd^2 * (1 - norm.r)), 2), # SEM
      method == "chelune" ~  round(sqrt(2 * norm.t1.sd^2 * (1 - norm.r)), 2), # SEM
      method == "mcsweeny" ~ round(norm.t2.sd * sqrt(1 - norm.r^2), 2), # SEE
      method == "charter" ~  round(norm.t2.sd * sqrt(1 - norm.r^2), 2), # SEE
      method == "temkin" ~ round(sqrt(norm.t1.sd^2 + norm.t2.sd^2 - 2 * norm.t1.sd * norm.t2.sd * norm.r), 2),
      method == "iverson" ~ round(sqrt((norm.t1.sd^2 + norm.t2.sd^2) * (1 - norm.r)), 2),
      method == "maassen" ~ round(sqrt((norm.t1.sd^2 + norm.t2.sd^2) * (1 - norm.r)), 2)
    )

  if (method == "CH") SE4 <- norm.t2.sd * sqrt(1 - norm.r^2)
  if (method == "CH") t2.sem <- round(SE4 * sqrt(1 + (1 / norm.n) + (((t1.score - norm.t1.mean)^2) / ((norm.t1.sd^2) * (norm.n - 1)))), 2)
  if (method %in% c("maassen", "crawford")) { estimated_beta <- norm.t2.sd / norm.t1.sd; estimated_constant <- norm.t2.mean - estimated_beta * norm.t1.mean; est <- round(estimated_beta * t1.score + estimated_constant, 2) }
  if (method == "crawford") t2.sem <- se_n1(
      sd.1 = norm.t1.sd, sd.2 = norm.t2.sd, r = norm.r, n = norm.n,
      t1.score = t1.score, #t2.score = t2.score,
      norm.t1.mean = norm.t1.mean
  )

  t2.sem <- round(t2.sem, 2)

  critical_value <- qnorm(1 - ((1-conf.level)/2))
  # check the critical value! does the /2 produce the correct value?

  t2.ci.lb <- round(t2.expected-(t2.sem*critical_value), 2)
  t2.ci.ub <- round(t2.expected+(t2.sem*critical_value), 2)

 result <- list(
   method = method,
   t1.score = t1.score,
   norm.t1.mean = norm.t1.mean,
   norm.t1.sd = norm.t1.sd,
   norm.t2.mean = norm.t2.mean,
   norm.t2.sd = norm.t2.sd,
   norm.r = norm.r,
   norm.n = norm.n,
   conf.level = conf.level,
   t2.expected = t2.expected,
   t2.sem = t2.sem,
   t2.ci.lb = t2.ci.lb,
   t2.ci.ub = t2.ci.ub
   )

 class(result) <- 'predict_t2'
 return(result)
}

#' @export
print.predict_t2 <- function(x, ...) {

  header <- paste("Estimate Neuropsychological Test Scores Based on Earlier Administrations.\n\n", sep = "")
  footnote <- "See documentation for further information on how scores are computed."

  input_df <- data.frame(
    Variable = c("Time 1", "Time 2"),
    Mean = c(x$norm.t1.mean, x$norm.t2.mean),
    SD = c(x$norm.t1.sd, x$norm.t2.sd),
    `Case's Score` = c(x$t1.score, ""),
    n = c(x$norm.n, ""),
    r = c(x$norm.r, ""),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  input_table <- knitr::kable(input_df, format = "simple", col.names = c("Variable", "Mean", "SD", "Case's Score", "n", "r"))

  input <- paste(
    "INPUT:",
    paste(capture.output(input_table), collapse = "\n"), "\n\n",
    sep = ""
  )

  params <- paste(
    "PARAMS:", "\n\n",
    "Change method:    ", x$method, "\n",
    "Confidence level: ", x$conf.level, "\n\n",
    sep = ""
  )

  output <- paste(
    "OUTPUT:", "\n\n",
    "Estimate for Time 2: ", x$t2.expected, "\n",
    "Standard error:      ", x$t2.sem, "\n",
    "Confidence interval: ", x$t2.ci.lb, " to ", x$t2.ci.ub, "\n",
    sep = ""
  )

  result <- paste(header, input, params, output, sep = "")
  cat(result)
}

#predict_t2(method = "crawford", 100, 103, 10, 109, 8, 0.76, 100, .95)

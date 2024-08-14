#' Calculate reliable_change using various methods
#'
#' This function calculates reliable_change using different methods such as Crawford,
#' Jacobson, Speer, and others.
#'
#' @param method The method used for reliable_change calculation. Default is "crawford".
#' @param t1.score The score at time 1.
#' @param norm.t1.mean Mean of the normative data at time 1.
#' @param norm.t1.sd Standard deviation of the normative data at time 1.
#' @param norm.t2.mean Mean of the normative data at time 2.
#' @param norm.t2.sd Standard deviation of the normative data at time 2.
#' @param norm.r Pearson correlation coefficient between time 1 and time 2 scores.
#' @param norm.n Sample size of the normative data.
#' @param conf.level Confidence level for the confidence interval calculation. Default is 0.9.
#' @return A list containing the expected score at time 2, standard error of the mean,
#' lower and upper bounds of the confidence interval.
#'
#' @examples
#' reliable_change(method = "jacobson", 100, 103, 10, 109, 8, 0.76, 100, .95)
#'
#' @export
reliable_change <- function(method = "crawford", t1.score,
                      norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n, conf.level = .9) {
  change.methods <- c("jacobson", "speer", "chelune","mcsweeny",
                      "charter", "CH", "temkin", "iverson", "maassen", "crawford")

  if (missing(method)) {stop("'method' argument for calculating reliable_change required.")}
  if (missing(t1.score) || missing(norm.t1.mean) || missing(norm.t1.sd) || missing(norm.t2.mean) || missing(norm.t2.sd) || missing(norm.r) || missing(norm.n)) {stop("Missing arguments, please include: 't1.score', 'norm.t1.mean', 'norm.t1.sd', 'norm.t2.mean', 'norm.t2.sd', 'norm.r', 'norm.n'.")}


  conf.level_ <- function(conf.level) {
    if (conf.level > 1) {
      return(conf.level / 100)
    } else {
      return(conf.level)
    }
  }

  if (!(method %in% change.methods)) {
    stop(
      call. = F,
      glue::glue('"{method}" is not an accepted reliable change method. Please select from the available methods ({stringr::str_flatten(change.methods, collapse = ", ")})')
    )
  }

  # note mcsweeny has been removed as appears the same as crawford (expect mcsweeny expects reg coefficients)

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

 class(result) <- 'reliable_change'
 return(result)
}

#' @export
print.reliable_change <- function(x, ...) {
  width_test <- 7
  width_dev <- 10
  width_ci <- 20
  width_2t <- 14
  width_1t <- 14
  width_2t_prev <- 12
  width_1t_prev <- 12

  header <- paste(
    "Reliable Change: Assessing if there has been a statistically significant change over time between two scores.\n\n",
    sep = ""
  )

  # Input Data Frame with customizable names
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
    "OUTPUT:", "\n",
    "Estimate for Time 2: ", x$t2.expected, "\n",
    "Standard error:      ", x$t2.sem, "\n",
    "Confidence interval: ", x$t2.ci.lb, " to ", x$t2.ci.ub, "\n",
    sep = ""
  )

  result <- paste(header, input, params, output, sep = "")
  cat(result)
}


reliable_change(method = "crawford", 100, 103, 10, 109, 8, 0.76, 100, .95)

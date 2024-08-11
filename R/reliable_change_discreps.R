#' Compare Stability Methods
#'
#' This function compares stability methods given the scores of two tests.
#'
#' @name reliable_change_discreps
#' @param t1.score Scores of the first test.
#' @param t2.score Scores of the second test.
#' @param norm.t1.mean Mean of the normative sample at time 1.
#' @param norm.t1.sd Standard deviation of the normative sample at time 1.
#' @param norm.t2.mean Mean of the normative sample at time 2.
#' @param norm.t2.sd Standard deviation of the normative sample at time 2.
#' @param norm.r Reliability of the normative sample.
#' @param norm.n Sample size of the normative sample.
#' @param conf.level Confidence level for stability assessment.
#' @param methods A character vector specifying which methods to calculate.
#'   Default is "all" methods.
#'
#' @importFrom utils capture.output globalVariables
#' @importFrom glue glue
#' @importFrom knitr kable
#' @importFrom dplyr relocate
#' @export
#'
#' @examples
#' # Example data
#' t1.score <- 88
#' norm.t1.mean <- 113
#' norm.t1.sd <- 7.2
#' norm.t2.mean <- 118.7
#' norm.t2.sd <- 8.3
#' norm.r <- 0.93
#' norm.n <- 821
#' t2.score <- 103
#' ci <- 0.95
#'
#' # Using all methods
#' result_all <- reliable_change_discreps(t1.score, t2.score,
#' norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd, norm.r, norm.n,
#' ci)
#' print(result_all)
#'
#' # Using selected methods
#' result_selected <- reliable_change_discreps(t1.score, t2.score,
#' norm.t1.mean, norm.t1.sd,norm.t2.mean, norm.t2.sd, norm.r,
#' norm.n, ci, methods = c("crawford", "maassen"))
#' print(result_selected)


utils::globalVariables(c("method", "expected", "actual"))

reliable_change_discreps <- function(
    t1.score, t2.score,
    norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
    norm.r, norm.n, conf.level = .9,
    methods = "all") {

  all_methods <- c("jacobson", "speer", "chelune", "mcsweeny", "charter", "CH", "temkin", "iverson", "maassen", "crawford")

  if (any(methods == "all")) {
    methods <- all_methods
  }

  tab <- sapply(methods, function(method) {
    rci.tab <- reliable_change(method, t1.score,
                               norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
                               norm.r, norm.n, conf.level = conf.level)
    c(expected = rci.tab$t2.expected,
      sem = rci.tab$t2.sem,
      ci.lb = rci.tab$t2.ci.lb,
      ci.ub = rci.tab$t2.ci.ub)
  })

  tab <- rbind(tab, actual = t2.score)
  tab <- as.data.frame(t(tab))
  tab$method <- row.names(tab)
  tab$z <- round((tab$actual - tab$expected) / tab$sem, 2)
  rownames(tab) <- NULL
  tab <- tab |> dplyr::relocate(method, expected, actual)

  result <- list(
    tab = tab,
    t1.score = t1.score,
    t2.score = t2.score,
    norm.t1.mean = norm.t1.mean,
    norm.t1.sd = norm.t1.sd,
    norm.t2.mean = norm.t2.mean,
    norm.t2.sd = norm.t2.sd,
    norm.r = norm.r,
    norm.n = norm.n,
    conf.level = conf.level
  )

  class(result) <- 'reliable_change_discreps'
  return(result)
}

#' @export
print.reliable_change_discreps <- function(x, ...) {
  header <- "Comparison of Stability Methods\n\n"

  input <- glue::glue(
    "INPUT: Data from the normative sample", "\n\n",
    "Time 1 mean:  {x$norm.t1.mean}", "\n",
    "Time 1 sd:    {x$norm.t1.sd}", "\n",
    "Time 2 mean:  {x$norm.t2.mean}", "\n",
    "Time 2 sd:    {x$norm.t2.sd}", "\n",
    "Reliability:  {x$norm.r}", "\n",
    "N:            {x$norm.n}", "\n",
    "Time 1 score: {x$t1.score}"
  )

  params <- paste(
    "\n\nPARAMS:", "\n",
    "Confidence level: ", x$conf.level, "\n\n",
    sep = ""
  )

  output_table <- knitr::kable(x$tab, format = "simple")

  output <- paste(
    "OUTPUT:",
    paste(capture.output(output_table), collapse = "\n"), "\n",
    sep = ""
  )

  result <- paste(header, input, params, output, sep = "")
  cat(result)
}

# # Example usage
# t1.score <- 88
# norm.t1.mean <- 113
# norm.t1.sd <- 7.2
# norm.t2.mean <- 118.7
# norm.t2.sd <- 8.3
# norm.r <- 0.93
# norm.n <- 821
# t2.score <- 103
# ci <- 0.95
#
# result <- reliable_change_discreps(t1.score, t2.score,
#                                    norm.t1.mean, norm.t1.sd, norm.t2.mean, norm.t2.sd,
#                                    norm.r, norm.n, ci, methods = c("crawford", "maassen"))
#
# print(result)

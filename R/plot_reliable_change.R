utils::globalVariables(c("change"))

#' Plot Stability
#'
#' @param data The data frame containing the input data.
#' @param metric The metric to be used ("z" by default).
#' @param group The grouping variable in the data.
#' @param test The test variable in the data.
#' @param t1.score The score at time point 1.
#' @param t2.expected The expected score at time point 2.
#' @param t2.score The actual score at time point 2.
#' @param t2.ci.lb The lower bound of the confidence interval for time point 2.
#' @param t2.ci.ub The upper bound of the confidence interval for time point 2.
#' @param axis.label.metric The metric used for axis labeling (default is "z").
#' @param abbreviations Logical, whether to abbreviate test names (default is TRUE).
#'
#' @return A ggplot object representing the stability plot.
#' @export
#' @import ggplot2
#' @importFrom dplyr mutate arrange select case_when vars
#' @importFrom ggplot2 geom_segment geom_point geom_errorbar coord_cartesian theme_bw scale_x_continuous scale_y_discrete facet_grid scale_color_manual
#' @importFrom glue glue
#' @importFrom stats pnorm
#' @importFrom neuropsytools convert_standard
plot_reliable_change <- function(data, metric = "z", group, test, t1.score, t2.expected, t2.score,
                                 t2.ci.lb, t2.ci.ub, axis.label.metric = metric, abbreviations = TRUE) {
  # Check if group or test is missing
  if (missing(group) || missing(test)) {stop("Both the 'test' and 'group' arguments are required.")}
  if (missing(t2.ci.lb) || missing(t2.ci.ub)) {stop("Both the 't2.ci.lb' and 't2.ci.ub' arguments are required.")}
  if (missing(t1.score) || missing(t2.expected)  || missing(t2.score)) {stop("Both the 't1.score' 't2.expected' and 't2.score' arguments are required.")}

  # Detect duplicate rows based on test and group
  dup_rows <- duplicated(dplyr::select(data, {{test}}, {{group}}))
  if (any(dup_rows)) {
    dup_indices <- which(dup_rows)
    warning(glue::glue("Duplicate rows detected based on 'test' and row. Rows {dup_indices} will be removed."))
    data <- data[!dup_rows, ]
  }

  # Convert inputs to z scores
  data <- data |> dplyr::mutate(
    t1.score = neuropsytools::convert_standard({{t1.score}}, metric = metric, metric.new = "z"),
    t2.expected = neuropsytools::convert_standard({{t2.expected}}, metric = metric, metric.new = "z"),
    t2.score = neuropsytools::convert_standard({{t2.score}}, metric = metric, metric.new = "z"),
    t2.ci.lb = neuropsytools::convert_standard({{t2.ci.lb}}, metric = metric, metric.new = "z"),
    t2.ci.ub = neuropsytools::convert_standard({{t2.ci.ub}}, metric = metric, metric.new = "z"),
  )

  # axis labels and title
  label_types <- c("percentile", "t", "index", "scaled", "z")
  label_names <- c("Percentile", "T Score", "Index Score", "Scaled Score", "Z Score")
  labeling_functions <- list(
    percentile = function(x) paste0(format(neuropsytools::convert_standard(x, metric = "z", metric.new = "t")), "%"),
    t = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "t")),
    index = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "index")),
    scaled = function(x) format(neuropsytools::convert_standard(x, metric = "z", metric.new = "scaled")),
    z = function(x) as.character(x)
  )
  labeling_function <- labeling_functions[[match(axis.label.metric, label_types)]]
  y_label_text <- label_names[match(axis.label.metric, label_types)]

  # Abbreviations
  abbreviate_test_names <- if (abbreviations) {
    function(x) {
      ifelse(nchar(x) > 4, toupper(abbreviate(x, minlength = 1, dot = FALSE, method = "both.sides")), stringr::str_to_title(x))
    }
  } else {
    function(x) x
  }

  # Plot
  p <- data |>
    dplyr::mutate(
      group = factor({{group}}, levels = unique({{group}})),
      test = factor({{test}}, levels = unique({{test}})),
      change = dplyr::case_when(
        {{t2.score}} <= {{t2.ci.lb}} ~ "decrease",
        {{t2.score}} >= {{t2.ci.ub}} ~ "increase",
        TRUE ~ "no change"
      )
    ) |>
    dplyr::arrange(group, test) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = {{t1.score}}, xend = {{t2.score}},
        y = test, yend = test, color = change
      )
    ) +
    ggplot2::geom_segment(arrow = ggplot2::arrow(angle = 30, length = ggplot2::unit(0.2, "cm")), size = 1, position = ggplot2::position_nudge(y = -0.15)) +
    ggplot2::geom_point(ggplot2::aes(x = {{t2.expected}}, y = test), color = "#CC99FF", size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = {{t1.score}}, y = test, color = change), size = 2, position = ggplot2::position_nudge(y = -0.15)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmin = {{t2.ci.lb}}, xmax = {{t2.ci.ub}}),
                           color = "#CC99FF",
                           linewidth = 0.6
    ) +
    ggplot2::coord_cartesian(xlim = c(-4,4)) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::scale_x_continuous(n.breaks = 10, expand = c(0, 0), labels = labeling_function) +
    ggplot2::scale_y_discrete(labels = abbreviate_test_names) +
    #neuropsytools::plot_theme() +
    ggplot2::theme(
      strip.text.y.right = ggplot2::element_text(size = 12),
      plot.title.position = "plot",
      legend.position = "bottom"
    ) +
    ggplot2::labs(y = "Neuropsychology Tests", x = y_label_text) +
    ggplot2::facet_grid(
      rows = dplyr::vars(group),
      scales = "free", space = "free", labeller = ggplot2::labeller(group = abbreviate_test_names)
    ) +
    ggplot2::scale_color_manual(
      values = c("increase" = "#00CC33", "decrease" = "#FF3333", "no change" = "darkgray"),
      drop = FALSE
    )

  return(p)
}



 # data <- data.frame(
 #   t1.score = c(-0.7, 2.1, -1.3, -2.2),
 #   t2.score = c(1.2, -1.8, -1.2, 5),
 #   t2.expected = c(1, 2, -1, -2),
 #   test = c("test 1", "test 2", "test 3", "test 4"),
 #   group = c("group 1", "group 2", "group 3", "group 4"),
 #   t2.ci.lb = c(0.5, 1.5, -4.5, -2.5),
 #   t2.ci.ub = c(1.5, 2.5, 0, -1.5)
 # )
 # plot_reliable_change(data = data, metric = "z", group = group,
 # test = test, t1.score = t1.score, t2.expected = t2.expected,
 # t2.score = t2.score, t2.ci.lb = t2.ci.lb, t2.ci.ub = t2.ci.ub, axis.label.metric = "percentile", abbreviations = T)
 #

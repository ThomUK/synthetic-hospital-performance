#' SPC analysis and chart of an indicator
#'
#' `indicator_spc()` runs [NHSRplotthedots::ptd_spc()] on an indicator series with
#' the improvement direction, target and rebase dates of its YAML.
#' `plot_indicator()` draws it with [NHSRplotthedots::ptd_create_ggplot()].
#' Periods with no value (a zero denominator) are left out of the analysis.
#'
#' @param data An indicator series from [build_indicator()] or [combine_components()].
#' @param ... Passed to [NHSRplotthedots::ptd_create_ggplot()], for example
#'   `point_size` or `icons_position`.
#' @return `indicator_spc()` returns a `ptd_spc_df`; `plot_indicator()` a ggplot.
#' @export
indicator_spc <- function(data) {
  m <- attr(data, "indicator")
  if (is.null(m)) stop("data has no indicator attribute; build it with build_indicator() or combine_components()", call. = FALSE)
  if ("group" %in% names(data) && length(unique(data$group)) > 1) {
    stop("data holds several groups; pick one with group_series() first", call. = FALSE)
  }
  d <- as.data.frame(data)
  d <- d[!is.na(d$value), , drop = FALSE]
  target <- if (is.null(m$target)) NHSRplotthedots::ptd_target() else m$target
  rebase <- if (is.null(m$rebase)) NHSRplotthedots::ptd_rebase() else as.Date(unlist(m$rebase))
  NHSRplotthedots::ptd_spc(
    d,
    value_field = value, date_field = period,
    improvement_direction = m$improvement_direction,
    target = target, rebase = rebase
  )
}

#' @rdname indicator_spc
#' @export
plot_indicator <- function(data, ...) {
  m <- attr(data, "indicator")
  spc <- indicator_spc(data)
  args <- list(
    x = spc,
    main_title = m$title,
    x_axis_label = paste0(tools::toTitleCase(m$grain), " starting"),
    y_axis_label = m$unit %||% "",
    x_axis_date_format = "%b %y",
    x_axis_breaks = switch(m$grain, month = "3 months", week = "3 months", day = "1 month"),
    point_size = 2.5
  )
  dots <- list(...)
  args[names(dots)] <- dots
  do.call(NHSRplotthedots::ptd_create_ggplot, args)
}

#' Summarise the latest point of an indicator
#'
#' @inheritParams indicator_spc
#' @return A one-row data frame: `id`, `period`, `value`, `variation` (the
#'   plot-the-dots point type of the latest period, such as
#'   `special_cause_improvement`), `mean` (the process mean the latest point
#'   is judged against), `target` and `assurance` (`pass`, `fail`,
#'   `inconsistent` or `NA` without a target).
#' @export
indicator_summary <- function(data) {
  m <- attr(data, "indicator")
  spc <- as.data.frame(indicator_spc(data))
  last <- spc[which.max(spc$x), ]
  assurance <- NA_character_
  if (!is.null(m$target) && nrow(spc) > 0) {
    assurance <- assurance_type(last$lpl, last$upl, m$target, m$improvement_direction)
  }
  data.frame(
    id = m$id, period = as.Date(last$x), value = last$y, variation = last$point_type,
    mean = last$mean_col, target = m$target %||% NA_real_, assurance = assurance,
    stringsAsFactors = FALSE
  )
}

# The plot-the-dots assurance icon: pass when the whole control range meets
# the target, fail when none of it does, otherwise inconsistent.
assurance_type <- function(lpl, upl, target, direction) {
  if (direction == "neutral" || is.na(lpl) || is.na(upl)) return(NA_character_)
  if (direction == "increase") {
    if (lpl >= target) "pass" else if (upl < target) "fail" else "inconsistent"
  } else {
    if (upl <= target) "pass" else if (lpl > target) "fail" else "inconsistent"
  }
}

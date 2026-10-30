#' The 6-12 view of an indicator
#'
#' The weekly business review chart: the last six complete weeks and the
#' last twelve complete months side by side, each with the same period a
#' year earlier and the target, plus a row of to-date figures. For a flow
#' indicator each figure is computed from the components summed over the
#' range, so a rate for a month is the month's numerator over the month's
#' denominator, never an average of weeks. For a snapshot indicator (see
#' [is_snapshot()]) a range's figure is the stock at its end.
#'
#' Weeks run Monday to Sunday and are compared with the week 52 weeks
#' earlier, so weekdays line up. Months are compared with the same calendar
#' month a year earlier. Month-, quarter- and year-to-date run to `as_of`
#' and are compared with the same days a year earlier; the year and its
#' quarters start in `start_month`. Changes are percentage changes, except
#' for percentage indicators, where they are differences in percentage
#' points.
#'
#' @inheritParams build_indicator
#' @param as_of The last day with data; default the window's last day.
#' @param group For a breakdown, the group code(s) to restrict to; several
#'   codes are summed, which is how a folded "Other" group is reproduced.
#' @param start_month The month the financial year starts in; 4 for April.
#' @return An object of class `fhperf_six_twelve`: a list with `weekly` and
#'   `monthly` data frames (`start`, `end`, `label`, `value`, `prior`,
#'   `target`), `stats` (one row per figure: `stat`, `value`, `comparison`,
#'   `change`), the `indicator`, `as_of`, `snapshot` and `change_unit`
#'   (`"%"` or `"pp"`).
#' @export
six_twelve <- function(con, cat, id, as_of = NULL, window = NULL, cache = new.env(), by = NULL, group = NULL, start_month = 4) {
  ind <- get_indicator(cat, id)
  window <- resolve_window(window)
  if (is.null(window)) stop("six_twelve() needs a window; none was given and the dataset window is not available", call. = FALSE)
  as_of <- as.Date(as_of %||% window[2])
  snapshot <- is_snapshot(cat, id)
  if (!is.null(by) && !by %in% indicator_breakdowns(cat, id)) {
    stop("Indicator '", id, "' cannot be broken down by '", by, "'", call. = FALSE)
  }
  if (!is.null(by) && is.null(group)) stop("group is needed with by", call. = FALSE)
  extra <- c(as_of, shift_year(as_of, -1))
  series <- function(component) {
    comp <- get_component(cat, component)
    events <- cached_events(con, cat, component, window, cache, snapshot_dates = if (comp$snapshot) extra)
    daily <- aggregate_events(events, "day", comp$aggregate, window, by, snapshot = comp$snapshot)
    if (!is.null(by)) {
      daily <- daily[daily$group %in% group, , drop = FALSE]
      daily <- if (nrow(daily) == 0) data.frame(period = as.Date(character()), value = numeric()) else
        stats::aggregate(value ~ period, daily, sum, na.action = stats::na.pass)
    }
    daily
  }
  num <- series(ind$numerator)
  den <- if (is.null(ind$denominator)) NULL else series(ind$denominator)
  six_twelve_data(num, den, ind, as_of, snapshot, start_month)
}

#' @rdname six_twelve
#' @param numerator,denominator Daily component series (`period`, `value`),
#'   as [aggregate_events()] gives with grain `"day"`; for a snapshot,
#'   values on the snapshot dates and `NA` elsewhere. `denominator` is
#'   `NULL` for a count.
#' @param indicator An indicator from [get_indicator()].
#' @param snapshot Is the indicator a snapshot?
#' @export
six_twelve_data <- function(numerator, denominator = NULL, indicator, as_of, snapshot = FALSE, start_month = 4) {
  as_of <- as.Date(as_of)
  scale <- indicator$multiplier / indicator$divisor
  count <- identical(indicator$type, "count")
  figure <- function(start, end) {
    pick <- function(s) {
      if (is.null(s)) return(NA_real_)
      if (snapshot) {
        v <- s$value[s$period == end]
        return(if (length(v) == 1) v else NA_real_)
      }
      sel <- s$period >= start & s$period <= end
      if (!any(sel) || !all(seq(start, end, by = "day") %in% s$period)) return(NA_real_)
      sum(s$value[sel])
    }
    n <- pick(numerator)
    d <- pick(denominator)
    if (count) return(n * scale)
    if (is.na(n) || is.na(d) || d <= 0) NA_real_ else n / d * scale
  }
  target <- indicator$target %||% NA_real_

  # the last six complete weeks, Monday to Sunday
  last_sunday <- as_of - (as.integer(format(as_of, "%u")) %% 7L)
  week_end <- last_sunday - 7 * (5:0)
  weekly <- data.frame(start = week_end - 6, end = week_end)
  weekly$label <- paste0("wk ", as.integer(format(weekly$end, "%V")))
  weekly$value <- mapply(figure, weekly$start, weekly$end)
  weekly$prior <- mapply(figure, weekly$start - 364, weekly$end - 364)
  weekly$target <- target

  # the last twelve complete months
  last_month_end <- if (is_period_end(as_of, "month")) as_of else period_start(as_of, "month") - 1
  month_start <- rev(seq(period_start(last_month_end, "month"), by = "-1 month", length.out = 12))
  monthly <- data.frame(start = month_start, end = month_end(month_start))
  monthly$label <- format(monthly$start, "%b %y")
  monthly$value <- mapply(figure, monthly$start, monthly$end)
  prior_start <- shift_year(monthly$start, -1)
  monthly$prior <- mapply(figure, prior_start, month_end(prior_start))
  monthly$target <- target

  # the to-date figures
  change_unit <- if (identical(indicator$type, "percentage")) "pp" else "%"
  change <- function(now, then) {
    if (is.na(now) || is.na(then)) return(NA_real_)
    if (change_unit == "pp") now - then else if (then == 0) NA_real_ else (now - then) / abs(then) * 100
  }
  row <- function(stat, value, comparison, then) data.frame(stat = stat, value = value, comparison = comparison, change = change(value, then), stringsAsFactors = FALSE)
  last <- nrow(weekly)
  stats <- rbind(
    row("LastWk", weekly$value[last], "WOW", weekly$value[last - 1]),
    row("LastWk", weekly$value[last], "YOY", weekly$prior[last])
  )
  if (snapshot) {
    latest <- figure(as_of, as_of)
    stats <- rbind(stats, row("Latest", latest, "YOY", figure(shift_year(as_of, -1), shift_year(as_of, -1))))
  } else {
    fy_start <- fy_start_date(as_of, start_month)
    q_start <- seq(fy_start, by = "3 months", length.out = 4)[((as.integer(format(as_of, "%m")) - start_month) %% 12L) %/% 3L + 1L]
    to_date <- function(stat, start) row(stat, figure(start, as_of), "YOY", figure(shift_year(start, -1), shift_year(as_of, -1)))
    stats <- rbind(stats, to_date("MTD", period_start(as_of, "month")), to_date("QTD", q_start), to_date("YTD", fy_start))
  }
  rownames(stats) <- NULL
  structure(list(indicator = indicator, as_of = as_of, snapshot = snapshot, weekly = weekly, monthly = monthly,
                 stats = stats, change_unit = change_unit), class = "fhperf_six_twelve")
}

month_end <- function(month_start) {
  as.Date(vapply(seq_along(month_start), function(i) as.character(seq(month_start[i], by = "1 month", length.out = 2)[2] - 1), character(1)))
}

# The same calendar date in another year; 29 February becomes 1 March.
shift_year <- function(dates, years) {
  as.Date(vapply(seq_along(dates), function(i) as.character(seq(dates[i], by = paste(years, "year"), length.out = 2)[2]), character(1)))
}

fy_start_date <- function(date, start_month) {
  y <- as.integer(format(date, "%Y"))
  m <- as.integer(format(date, "%m"))
  as.Date(sprintf("%d-%02d-01", if (m >= start_month) y else y - 1L, start_month))
}

#' @export
print.fhperf_six_twelve <- function(x, ...) {
  cat("<fhperf_six_twelve> ", x$indicator$id, ": ", x$indicator$title, ", as of ", format(x$as_of),
      if (x$snapshot) " (snapshot)", "\n", sep = "")
  print(format_six_twelve(x), row.names = FALSE)
  invisible(x)
}

#' The 6-12 summary row, formatted
#'
#' @param x A `fhperf_six_twelve` from [six_twelve()].
#' @return A one-row data frame of strings whose column names are the
#'   figures: `LastWk`, `WOW`, `YOY`, `MTD`, `YOY`, and so on, in the order
#'   the weekly business review prints them.
#' @export
format_six_twelve <- function(x) {
  fmt_change <- function(v) {
    if (is.na(v)) return("")
    if (x$change_unit == "pp") sprintf("%+.1f pp", v) else sprintf("%+.0f%%", v)
  }
  out <- list()
  seen <- character()
  for (i in seq_len(nrow(x$stats))) {
    s <- x$stats[i, ]
    if (!s$stat %in% seen) {
      out[[length(out) + 1]] <- format_value(s$value, x$indicator)
      names(out)[length(out)] <- s$stat
      seen <- c(seen, s$stat)
    }
    out[[length(out) + 1]] <- fmt_change(s$change)
    names(out)[length(out)] <- s$comparison
  }
  d <- as.data.frame(out, check.names = FALSE, stringsAsFactors = FALSE, optional = TRUE)
  names(d) <- names(out)
  d
}

#' Draw the 6-12 chart
#'
#' Two panels with their own y axes: six weekly points and twelve monthly
#' points, this year as a heavy blue line with value labels, a year earlier
#' as a thin grey line, and the target as triangles. The summary row
#' ([format_six_twelve()]) is drawn beneath the chart as part of the same
#' image, so it scales with it.
#'
#' @param x A `fhperf_six_twelve` from [six_twelve()].
#' @param stats Draw the summary row under the chart?
#' @return A ggplot; with `stats`, a patchwork of the chart over the row,
#'   which prints and saves like any ggplot.
#' @export
plot_six_twelve <- function(x, stats = TRUE) {
  chart <- six_twelve_chart(x)
  if (!stats) return(chart)
  patchwork::wrap_plots(chart, six_twelve_stats_panel(x), ncol = 1, heights = c(5, 1))
}

# The summary row as a ggplot: one column per figure, a bold header above
# its value, drawn with no axes so it reads as a table.
six_twelve_stats_panel <- function(x) {
  row <- format_six_twelve(x)
  d <- data.frame(
    col = rep(seq_along(row), 2),
    y = rep(c(1, 0), each = length(row)),
    text = c(names(row), unname(unlist(row))),
    header = rep(c(TRUE, FALSE), each = length(row)),
    stringsAsFactors = FALSE
  )
  ggplot2::ggplot(d, ggplot2::aes(x = .data$col, y = .data$y, label = .data$text)) +
    ggplot2::geom_text(ggplot2::aes(fontface = ifelse(.data$header, "bold", "plain")), size = 3.4, colour = "#1f2d3d") +
    ggplot2::annotate("segment", x = 0.6, xend = length(row) + 0.4, y = 0.5, yend = 0.5, colour = "#cfcfcf") +
    ggplot2::scale_x_continuous(limits = c(0.5, length(row) + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-0.6, 1.6), expand = c(0, 0)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 12, 4, 12))
}

six_twelve_chart <- function(x) {
  ind <- x$indicator
  w <- x$weekly
  w$panel <- "Last 6 weeks"
  m <- x$monthly
  m$panel <- "Last 12 months"
  d <- rbind(w, m)
  d$panel <- factor(d$panel, levels = c("Last 6 weeks", "Last 12 months"))
  d$label <- factor(d$label, levels = unique(d$label))
  d$text <- format_value(d$value, ind)
  cols <- c("This year" = "#2a78d6", "A year earlier" = "#a8a8a8", "Target" = "#eb6834")
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$label)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$prior, group = 1, colour = "A year earlier"), linewidth = 0.6, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$value, group = 1, colour = "This year"), linewidth = 1.1, na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(y = .data$value, colour = "This year"), size = 2.2, na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = .data$value, label = .data$text), vjust = -0.9, size = 3, colour = "#1f2d3d", na.rm = TRUE) +
    ggplot2::facet_grid(. ~ panel, scales = "free", space = "free_x") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.08, 0.2))) +
    ggplot2::labs(title = ind$title, x = NULL, y = ind$unit %||% "",
                  subtitle = paste0("As of ", format(x$as_of, "%d %B %Y"), if (x$snapshot) ". Snapshot: each point is the position at the period's end")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom", legend.title = ggplot2::element_blank(),
                   plot.title.position = "plot", strip.text = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (!all(is.na(d$target))) {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$target, colour = "Target"), shape = 17, size = 2.4, na.rm = TRUE)
  }
  p + ggplot2::scale_colour_manual(values = cols, breaks = names(cols))
}

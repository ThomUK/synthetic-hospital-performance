#' Period boundaries
#'
#' `period_start()` floors dates to the first day of their period: the first
#' of the month, the Monday of the week, or the day itself.
#' `period_seq()` lists every period start that touches a window.
#'
#' @param dates A `Date` vector.
#' @param grain `"month"`, `"week"` or `"day"`.
#' @param window A `Date` vector of length two, start and end.
#' @return A `Date` vector.
#' @examples
#' period_start(as.Date("2024-02-14"), "month")
#' period_seq(as.Date(c("2024-01-15", "2024-03-02")), "month")
#' @export
period_start <- function(dates, grain = c("month", "week", "day")) {
  grain <- match.arg(grain)
  dates <- as.Date(dates)
  switch(grain,
    month = as.Date(format(dates, "%Y-%m-01")),
    week = dates - (as.integer(format(dates, "%u")) - 1L),
    day = dates
  )
}

#' @rdname period_start
#' @export
period_seq <- function(window, grain = c("month", "week", "day")) {
  grain <- match.arg(grain)
  window <- check_window(window)
  seq(period_start(window[1], grain), period_start(window[2], grain), by = grain)
}

#' Aggregate events into periods
#'
#' Counts the events, or sums their `value`, that fall in each period. With a
#' window, events outside it are dropped first and every period of the window
#' is present in the result, with `0` where nothing happened, so that series
#' have no gaps.
#'
#' @param events A data frame from [read_component_events()].
#' @param grain `"month"`, `"week"` or `"day"`.
#' @param aggregate `"count"` or `"sum"`.
#' @param window Optional `Date` vector of length two.
#' @return A data frame with `period` (Date, the first day of the period) and
#'   `value`, ordered by period.
#' @examples
#' ev <- data.frame(event_date = as.Date(c("2024-01-03", "2024-01-20", "2024-03-01")), value = c(10, 20, 5))
#' aggregate_events(ev, "month", "count", window = as.Date(c("2024-01-01", "2024-03-31")))
#' aggregate_events(ev, "month", "sum")
#' @export
aggregate_events <- function(events, grain = c("month", "week", "day"), aggregate = c("count", "sum"), window = NULL) {
  grain <- match.arg(grain)
  aggregate <- match.arg(aggregate)
  dates <- as.Date(events$event_date)
  v <- if (aggregate == "count") rep(1, length(dates)) else as.numeric(events$value)
  if (!is.null(window)) {
    window <- check_window(window)
    keep <- !is.na(dates) & dates >= window[1] & dates <= window[2]
    dates <- dates[keep]
    v <- v[keep]
  }
  p <- period_start(dates, grain)
  s <- tapply(v, factor(format(p)), sum, na.rm = TRUE)
  out <- data.frame(period = as.Date(names(s)), value = as.numeric(s), stringsAsFactors = FALSE)
  if (!is.null(window)) out <- complete_periods(out, period_seq(window, grain))
  out <- out[order(out$period), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Make sure every period in `periods` has a row, filling value with 0.
complete_periods <- function(agg, periods) {
  missing <- periods[!periods %in% agg$period]
  if (length(missing) > 0) {
    agg <- rbind(agg, data.frame(period = missing, value = 0))
  }
  agg
}

#' Build one component: read its events and aggregate them
#'
#' @inheritParams read_component_events
#' @param grain `"month"`, `"week"` or `"day"`.
#' @param window Optional `Date` vector of length two; see [aggregate_events()].
#' @return A data frame with `period` and `value`, with the component id in
#'   the attribute `component`.
#' @export
build_component <- function(con, cat, id, grain = "month", window = NULL) {
  comp <- get_component(cat, id)
  events <- read_component_events(con, cat, id, window)
  out <- aggregate_events(events, grain, comp$aggregate, window)
  attr(out, "component") <- id
  out
}

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
#' have no gaps. With `by`, the events are split by that column first and
#' every group gets every period; events with no value in the column form
#' an `"Unknown"` group, so the groups always add up to the whole.
#'
#' @param events A data frame from [read_component_events()].
#' @param grain `"month"`, `"week"` or `"day"`.
#' @param aggregate `"count"` or `"sum"`.
#' @param window Optional `Date` vector of length two.
#' @param by Optional name of a column of `events` to group by.
#' @param snapshot Are the events snapshots rather than a flow? Then only
#'   the rows dated at a period's end are kept (month ends for months,
#'   Sundays for weeks, every date for days) so a period's value is the
#'   stock at its end, and periods without a snapshot are `NA`, not `0`.
#' @return A data frame with `period` (Date, the first day of the period),
#'   `group` (only with `by`) and `value`, ordered by group and period.
#' @examples
#' ev <- data.frame(event_date = as.Date(c("2024-01-03", "2024-01-20", "2024-03-01")), value = c(10, 20, 5))
#' aggregate_events(ev, "month", "count", window = as.Date(c("2024-01-01", "2024-03-31")))
#' aggregate_events(ev, "month", "sum")
#' @export
aggregate_events <- function(events, grain = c("month", "week", "day"), aggregate = c("count", "sum"),
                             window = NULL, by = NULL, snapshot = FALSE) {
  grain <- match.arg(grain)
  aggregate <- match.arg(aggregate)
  dates <- as.Date(events$event_date)
  v <- if (aggregate == "count") rep(1, length(dates)) else as.numeric(events$value)
  g <- NULL
  if (!is.null(by)) {
    if (!by %in% names(events)) stop("events have no column '", by, "' to group by", call. = FALSE)
    g <- as.character(events[[by]])
    g[is.na(g) | g == ""] <- "Unknown"
  }
  keep <- !is.na(dates)
  if (!is.null(window)) {
    window <- check_window(window)
    keep <- keep & dates >= window[1] & dates <= window[2]
  }
  if (snapshot) keep <- keep & is_period_end(dates, grain)
  dates <- dates[keep]
  v <- v[keep]
  g <- g[keep]
  fill <- if (snapshot) NA_real_ else 0
  p <- format(period_start(dates, grain))
  if (is.null(g)) {
    s <- tapply(v, factor(p), sum, na.rm = TRUE)
    out <- data.frame(period = as.Date(names(s)), value = as.numeric(s), stringsAsFactors = FALSE)
    if (!is.null(window)) out <- complete_periods(out, period_seq(window, grain), fill = fill)
    out <- out[order(out$period), , drop = FALSE]
  } else {
    s <- tapply(v, list(factor(p), factor(g)), sum, na.rm = TRUE)
    out <- data.frame(
      period = as.Date(rep(rownames(s), times = ncol(s))),
      group = rep(colnames(s), each = nrow(s)),
      value = as.numeric(s), stringsAsFactors = FALSE
    )
    out$value[is.na(out$value)] <- 0
    if (!is.null(window)) out <- complete_periods(out, period_seq(window, grain), groups = unique(out$group), fill = fill)
    out <- out[order(out$group, out$period), , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

# Is each date the last day of its period?
is_period_end <- function(dates, grain) {
  switch(grain,
    month = format(dates + 1, "%d") == "01",
    week = format(dates, "%u") == "7",
    day = rep(TRUE, length(dates))
  )
}

# Make sure every period in `periods` (for every group, if any) has a row,
# filling value with `fill`.
complete_periods <- function(agg, periods, groups = NULL, fill = 0) {
  if (is.null(groups)) {
    missing <- periods[!periods %in% agg$period]
    if (length(missing) > 0) agg <- rbind(agg, data.frame(period = missing, value = fill))
    return(agg)
  }
  full <- expand.grid(period = periods, group = groups, stringsAsFactors = FALSE)
  key <- paste(format(full$period), full$group)
  have <- paste(format(agg$period), agg$group)
  missing <- full[!key %in% have, , drop = FALSE]
  if (nrow(missing) > 0) agg <- rbind(agg, data.frame(period = missing$period, group = missing$group, value = fill))
  agg
}

#' Build one component: read its events and aggregate them
#'
#' @inheritParams read_component_events
#' @param grain `"month"`, `"week"` or `"day"`.
#' @param window Optional `Date` vector of length two; see [aggregate_events()].
#' @param by Optional dimension id the component declares; see
#'   [aggregate_events()].
#' @return A data frame with `period`, `group` (with `by`) and `value`, with
#'   the component id in the attribute `component`.
#' @export
build_component <- function(con, cat, id, grain = "month", window = NULL, by = NULL) {
  comp <- get_component(cat, id)
  if (!is.null(by) && !by %in% comp$dimensions) stop("Component '", id, "' does not carry dimension '", by, "'", call. = FALSE)
  events <- read_component_events(con, cat, id, window)
  out <- aggregate_events(events, grain, comp$aggregate, window, by, snapshot = comp$snapshot)
  attr(out, "component") <- id
  out
}

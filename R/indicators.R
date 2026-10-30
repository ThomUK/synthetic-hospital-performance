#' Combine aggregated components into an indicator series
#'
#' The value of a period is `numerator / denominator * multiplier / divisor`,
#' or `numerator * multiplier / divisor` for a count. A period whose
#' denominator is zero has value `NA`.
#'
#' @param numerator,denominator Data frames with `period` and `value`, as
#'   returned by [aggregate_events()], and `group` if they were aggregated
#'   by a dimension. `denominator` is `NULL` for a count.
#' @param indicator An indicator from [get_indicator()].
#' @return A data frame of class `fhperf_indicator_data` with `period`,
#'   `group` (if the components were grouped), `numerator`, `denominator`
#'   (`NA` for a count) and `value`, and the indicator definition in the
#'   attribute `indicator`.
#' @examples
#' cat <- read_catalogue()
#' m <- get_indicator(cat, "ed_003")
#' num <- data.frame(period = as.Date(c("2024-01-01", "2024-02-01")), value = c(90, 80))
#' den <- data.frame(period = as.Date(c("2024-01-01", "2024-02-01")), value = c(100, 100))
#' combine_components(num, den, m)
#' @export
combine_components <- function(numerator, denominator = NULL, indicator) {
  if (identical(indicator$type, "count") && !is.null(denominator)) stop("A count indicator takes no denominator", call. = FALSE)
  if (!identical(indicator$type, "count") && is.null(denominator)) stop("A ", indicator$type, " indicator needs a denominator", call. = FALSE)
  scale <- indicator$multiplier / indicator$divisor
  grouped <- "group" %in% names(numerator)
  if (!is.null(denominator) && grouped != ("group" %in% names(denominator))) {
    stop("numerator and denominator must both be grouped, or neither", call. = FALSE)
  }
  keys <- if (grouped) c("period", "group") else "period"
  out <- numerator[, keys, drop = FALSE]
  out$numerator <- numerator$value
  if (is.null(denominator)) {
    out$denominator <- NA_real_
    out$value <- out$numerator * scale
  } else {
    den <- denominator[, keys, drop = FALSE]
    den$denominator <- denominator$value
    out <- merge(out, den, by = keys, all = TRUE)
    out$numerator[is.na(out$numerator)] <- 0
    out$denominator[is.na(out$denominator)] <- 0
    out$value <- ifelse(out$denominator > 0, out$numerator / out$denominator * scale, NA_real_)
  }
  out <- out[do.call(order, out[, rev(keys), drop = FALSE]), c(keys, "numerator", "denominator", "value")]
  rownames(out) <- NULL
  structure(out, class = c("fhperf_indicator_data", "data.frame"), indicator = indicator)
}

#' Build an indicator from the database
#'
#' Reads and aggregates the indicator's numerator (and denominator) and
#' combines them with [combine_components()].
#'
#' @inheritParams read_component_events
#' @param id An indicator id.
#' @param window A `Date` vector of length two, the first and last day of
#'   the series. `NULL` uses [dataset_window()] if the `fakehospdata` package
#'   can supply it, and otherwise the range of the events themselves.
#' @param cache An environment in which each component's events are kept
#'   once read, so a denominator used by several indicators, or by several
#'   breakdowns of one, is queried once. [build_indicators()] and
#'   [build_breakdowns()] share one when given it.
#' @param by Optional dimension id to break the indicator down by; it must
#'   be one of [indicator_breakdowns()]. The result then has a `group`
#'   column and the attribute `dimension`.
#' @return See [combine_components()].
#' @examples
#' \dontrun{
#' con <- perf_connect("sample")
#' cat <- read_catalogue()
#' x <- build_indicator(con, cat, "ed_003")
#' plot_indicator(x)
#' }
#' @export
build_indicator <- function(con, cat, id, window = NULL, cache = new.env(), by = NULL) {
  m <- get_indicator(cat, id)
  window <- resolve_window(window)
  if (!is.null(by) && !by %in% indicator_breakdowns(cat, id)) {
    stop("Indicator '", id, "' cannot be broken down by '", by, "'; available: ",
         paste(indicator_breakdowns(cat, id), collapse = ", "), call. = FALSE)
  }
  get <- function(component) {
    comp <- get_component(cat, component)
    events <- cached_events(con, cat, component, window, cache)
    aggregate_events(events, m$grain, comp$aggregate, window, by, snapshot = comp$snapshot)
  }
  num <- get(m$numerator)
  den <- if (is.null(m$denominator)) NULL else get(m$denominator)
  out <- combine_components(num, den, m)
  if (!is.null(by)) attr(out, "dimension") <- get_dimension(cat, by)
  out
}

cached_events <- function(con, cat, component, window, cache, snapshot_dates = NULL) {
  key <- paste("events", component, paste(format(window), collapse = "/"), paste(format(snapshot_dates), collapse = ","), sep = "|")
  if (is.null(cache[[key]])) cache[[key]] <- read_component_events(con, cat, component, window, snapshot_dates)
  cache[[key]]
}

#' @rdname build_indicator
#' @param ids Indicator ids to build; default every indicator in the catalogue.
#' @return `build_indicators()` returns a list of indicator data frames named by id.
#' @export
build_indicators <- function(con, cat, ids = indicator_ids(cat), window = NULL, cache = new.env()) {
  window <- resolve_window(window)
  out <- lapply(ids, function(id) build_indicator(con, cat, id, window, cache))
  names(out) <- ids
  out
}

resolve_window <- function(window) {
  if (!is.null(window)) return(check_window(window))
  tryCatch(dataset_window(), error = function(e) NULL)
}

#' The dates the dataset covers
#'
#' Reads the window from the dataset manifest through `fakehospdata`.
#'
#' @param version Dataset version.
#' @return A `Date` vector of length two.
#' @export
dataset_window <- function(version = "v2") {
  if (!requireNamespace("fakehospdata", quietly = TRUE)) stop("The fakehospdata package is needed to read the dataset window", call. = FALSE)
  m <- fakehospdata::fh_manifest(version)
  as.Date(c(m$window$start, m$window$end))
}

#' @export
print.fhperf_indicator_data <- function(x, ...) {
  m <- attr(x, "indicator")
  dim <- attr(x, "dimension")
  cat("<fhperf_indicator_data> ", m$id, ": ", m$title, " (", m$type, ", by ", m$grain,
      if (!is.null(dim)) paste0(", broken down by ", dim$id), ")\n", sep = "")
  print(as.data.frame(x), ...)
  invisible(x)
}

#' Format an indicator value for display
#'
#' @param value Numeric values.
#' @param indicator An indicator from [get_indicator()].
#' @return A character vector: counts without decimals, percentages with
#'   one and a `%` sign, rates and means with one decimal.
#' @export
format_value <- function(value, indicator) {
  out <- switch(indicator$type,
    count = formatC(value, format = "f", digits = 0, big.mark = ","),
    percentage = paste0(formatC(value, format = "f", digits = 1), "%"),
    formatC(value, format = "f", digits = 1)
  )
  out[is.na(value)] <- NA_character_
  out
}

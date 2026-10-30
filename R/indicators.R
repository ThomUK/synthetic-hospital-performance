#' Combine aggregated components into an indicator series
#'
#' The value of a period is `numerator / denominator * multiplier / divisor`,
#' or `numerator * multiplier / divisor` for a count. A period whose
#' denominator is zero has value `NA`.
#'
#' @param numerator,denominator Data frames with `period` and `value`, as
#'   returned by [aggregate_events()]. `denominator` is `NULL` for a count.
#' @param indicator An indicator from [get_indicator()].
#' @return A data frame of class `fhperf_indicator_data` with `period`,
#'   `numerator`, `denominator` (`NA` for a count) and `value`, and the
#'   indicator definition in the attribute `indicator`.
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
  out <- data.frame(period = numerator$period, numerator = numerator$value)
  if (is.null(denominator)) {
    out$denominator <- NA_real_
    out$value <- out$numerator * scale
  } else {
    out <- merge(out, data.frame(period = denominator$period, denominator = denominator$value), by = "period", all = TRUE)
    out$numerator[is.na(out$numerator)] <- 0
    out$denominator[is.na(out$denominator)] <- 0
    out$value <- ifelse(out$denominator > 0, out$numerator / out$denominator * scale, NA_real_)
  }
  out <- out[order(out$period), c("period", "numerator", "denominator", "value")]
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
#' @param cache An environment used to share component builds between
#'   indicators; [build_indicators()] passes one so a denominator used by several
#'   indicators is read once.
#' @return See [combine_components()].
#' @examples
#' \dontrun{
#' con <- perf_connect("sample")
#' cat <- read_catalogue()
#' x <- build_indicator(con, cat, "ed_003")
#' plot_indicator(x)
#' }
#' @export
build_indicator <- function(con, cat, id, window = NULL, cache = new.env()) {
  m <- get_indicator(cat, id)
  window <- resolve_window(window)
  get <- function(component) {
    key <- paste(component, m$grain, paste(format(window), collapse = "/"), sep = "|")
    if (is.null(cache[[key]])) cache[[key]] <- build_component(con, cat, component, m$grain, window)
    cache[[key]]
  }
  num <- get(m$numerator)
  den <- if (is.null(m$denominator)) NULL else get(m$denominator)
  combine_components(num, den, m)
}

#' @rdname build_indicator
#' @param ids Indicator ids to build; default every indicator in the catalogue.
#' @return `build_indicators()` returns a list of indicator data frames named by id.
#' @export
build_indicators <- function(con, cat, ids = indicator_ids(cat), window = NULL) {
  window <- resolve_window(window)
  cache <- new.env()
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
  cat("<fhperf_indicator_data> ", m$id, ": ", m$title, " (", m$type, ", by ", m$grain, ")\n", sep = "")
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

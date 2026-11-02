#' A component as a series of its own
#'
#' A component is a count or a sum of events, so it can be charted like a
#' count indicator: the numerator and denominator of a rate, shown beside
#' the rate itself. `component_indicator()` describes the component as a
#' count-type indicator (its own id and title, the component's unit, no
#' direction or target). `component_series()` builds its series at a grain
#' and `six_twelve_component()` its 6-12 view, so [plot_indicator()],
#' [plot_indicator_seasons()] and [plot_six_twelve()] all apply.
#'
#' @inheritParams build_indicator
#' @param id A component id.
#' @param grain `"month"`, `"week"` or `"day"`.
#' @return `component_indicator()` a list shaped like [get_indicator()]'s
#'   result; `component_series()` a `fhperf_indicator_data`;
#'   `six_twelve_component()` a `fhperf_six_twelve`.
#' @export
component_indicator <- function(cat, id) {
  comp <- get_component(cat, id)
  list(
    id = id, title = comp$title, description = comp$description, type = "count",
    numerator = id, multiplier = 1, divisor = 1, grain = "month",
    improvement_direction = "neutral", unit = comp$unit %||% comp$aggregate,
    component = TRUE
  )
}

#' @rdname component_indicator
#' @export
component_series <- function(con, cat, id, grain = "month", window = NULL, cache = new.env()) {
  ind <- component_indicator(cat, id)
  ind$grain <- grain
  window <- resolve_window(window)
  comp <- get_component(cat, id)
  events <- cached_events(con, cat, id, window, cache)
  num <- aggregate_events(events, grain, comp$aggregate, window, snapshot = comp$snapshot)
  combine_components(num, NULL, ind)
}

#' @rdname component_indicator
#' @inheritParams six_twelve
#' @export
six_twelve_component <- function(con, cat, id, as_of = NULL, window = NULL, cache = new.env(), start_month = 4) {
  ind <- component_indicator(cat, id)
  six_twelve_build(con, cat, ind, get_component(cat, id)$snapshot, as_of, window, cache, NULL, NULL, start_month)
}

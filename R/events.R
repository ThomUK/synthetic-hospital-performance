#' Read the events of a component
#'
#' Runs the component's SQL against a database connection and checks the
#' result keeps the component contract: one row per event with an
#' `event_date` column (a date, or something `as.Date()` accepts) and, for a
#' component that sums, a numeric `value` column. Rows with no `event_date`
#' cannot be placed in a period and are dropped with a warning.
#'
#' The SQL may contain the placeholders `{{window_start}}` and
#' `{{window_end}}`, which are replaced by `DATE 'yyyy-mm-dd'` literals from
#' `window`. Snapshot components (for example a waiting list at each month
#' end) use them to know which period ends to generate; such a component
#' cannot be read without a window.
#'
#' @param con A `DBI` connection with the dataset attached, for example from
#'   [perf_connect()], or a function that takes a SQL string and returns a
#'   data frame, for a pipeline that supplies its own reader.
#' @param cat A catalogue from [read_catalogue()].
#' @param id A component id.
#' @param window Optional `Date` vector of length two, the first and last
#'   day of interest. Events are not filtered here; see [aggregate_events()].
#' @return A data frame with `event_date` (Date) and any other columns the
#'   query returned.
#' @export
read_component_events <- function(con, cat, id, window = NULL) {
  comp <- get_component(cat, id)
  sql <- substitute_window(component_sql(cat, id), window, id)
  events <- run_sql(con, sql)
  check_events(events, comp)
}

run_sql <- function(con, sql) {
  if (is.function(con)) return(as.data.frame(con(sql)))
  DBI::dbGetQuery(con, sql)
}

substitute_window <- function(sql, window, id) {
  has <- grepl("{{window_start}}", sql, fixed = TRUE) || grepl("{{window_end}}", sql, fixed = TRUE)
  if (!has) return(sql)
  if (is.null(window)) stop("Component '", id, "' uses {{window_start}}/{{window_end}} and needs a window", call. = FALSE)
  window <- check_window(window)
  sql <- gsub("{{window_start}}", sprintf("DATE '%s'", format(window[1])), sql, fixed = TRUE)
  gsub("{{window_end}}", sprintf("DATE '%s'", format(window[2])), sql, fixed = TRUE)
}

check_window <- function(window) {
  window <- tryCatch(as.Date(window), error = function(e) NULL)
  if (is.null(window) || length(window) != 2 || anyNA(window) || window[1] > window[2]) {
    stop("window must be two dates, start then end", call. = FALSE)
  }
  window
}

check_events <- function(events, comp) {
  where <- paste0("Component '", comp$id, "'")
  if (!"event_date" %in% names(events)) stop(where, " returned no event_date column", call. = FALSE)
  events$event_date <- tryCatch(as.Date(events$event_date), error = function(e) {
    stop(where, ": event_date cannot be read as a date", call. = FALSE)
  })
  if (identical(comp$aggregate, "sum")) {
    if (!"value" %in% names(events)) stop(where, " sums a value but returned no value column", call. = FALSE)
    if (!is.numeric(events$value)) stop(where, ": value must be numeric", call. = FALSE)
  }
  for (dim in comp$dimensions) {
    if (!dim %in% names(events)) stop(where, " declares dimension '", dim, "' but returned no such column", call. = FALSE)
    events[[dim]] <- as.character(events[[dim]])
  }
  undated <- is.na(events$event_date)
  if (any(undated)) {
    warning(where, ": ", sum(undated), " of ", nrow(events), " events have no event_date and were dropped", call. = FALSE)
    events <- events[!undated, , drop = FALSE]
  }
  rownames(events) <- NULL
  events
}

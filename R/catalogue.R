#' The catalogue directory shipped with the package
#'
#' The catalogue is three folders: `metrics/` (one YAML per metric),
#' `components/` (one YAML per numerator or denominator) and `sql/` (one
#' query per component, named `<component id>.sql` unless the component's
#' `sql` field says otherwise).
#'
#' @return A directory path.
#' @export
fhperf_catalogue_dir <- function() {
  system.file(package = "fhperf")
}

#' Read a catalogue of metrics and components
#'
#' @param path Directory holding `metrics/`, `components/` and `sql/`.
#'   Defaults to the catalogue shipped with the package.
#' @param validate Run [validate_catalogue()] on the result? Set to `FALSE`
#'   only to inspect a broken catalogue.
#' @return An object of class `fhperf_catalogue`: a list with `metrics` and
#'   `components` (each a list named by id), `sql_dir` and `path`.
#' @examples
#' cat <- read_catalogue()
#' metric_ids(cat)
#' get_metric(cat, "ed_4h_performance")
#' @export
read_catalogue <- function(path = fhperf_catalogue_dir(), validate = TRUE) {
  if (!dir.exists(path)) stop("Catalogue directory does not exist: ", path, call. = FALSE)
  cat <- structure(
    list(
      metrics = read_yaml_dir(file.path(path, "metrics")),
      components = read_yaml_dir(file.path(path, "components")),
      sql_dir = file.path(path, "sql"),
      path = path
    ),
    class = "fhperf_catalogue"
  )
  if (validate) validate_catalogue(cat)
  cat
}

# Read every YAML file in a directory into a list named by id. The file stem
# must equal the id inside it so that a file can be found from its id.
read_yaml_dir <- function(dir) {
  if (!dir.exists(dir)) return(list())
  files <- sort(list.files(dir, pattern = "\\.ya?ml$", full.names = TRUE))
  out <- lapply(files, function(f) {
    x <- yaml::read_yaml(f)
    stem <- sub("\\.ya?ml$", "", basename(f))
    if (is.null(x$id)) stop("No id in ", f, call. = FALSE)
    if (!identical(x$id, stem)) stop("File ", basename(f), " has id '", x$id, "'; the file must be named after its id", call. = FALSE)
    x$file <- f
    x
  })
  names(out) <- vapply(out, function(x) x$id, character(1))
  out
}

#' @export
print.fhperf_catalogue <- function(x, ...) {
  cat("<fhperf_catalogue> ", length(x$metrics), " metrics, ", length(x$components), " components\n", sep = "")
  cat("  ", x$path, "\n", sep = "")
  invisible(x)
}

#' Ids, metrics and components of a catalogue
#'
#' @param cat A catalogue from [read_catalogue()].
#' @param id A metric or component id.
#' @return `metric_ids()` and `component_ids()` return character vectors;
#'   `get_metric()` and `get_component()` return the parsed YAML as a list,
#'   with defaults filled in (`multiplier`, `divisor`, `grain`, `sql`);
#'   `component_sql()` returns the component's query as a string;
#'   `metric_components()` returns the component ids a metric uses, named
#'   `numerator` and (unless the metric is a count) `denominator`.
#' @name catalogue-access
#' @export
metric_ids <- function(cat) names(cat$metrics)

#' @rdname catalogue-access
#' @export
component_ids <- function(cat) names(cat$components)

#' @rdname catalogue-access
#' @export
get_metric <- function(cat, id) {
  m <- cat$metrics[[id]]
  if (is.null(m)) stop("No metric with id '", id, "'", call. = FALSE)
  m$multiplier <- m$multiplier %||% unname(default_multipliers[m$type])
  m$divisor <- m$divisor %||% 1
  m$grain <- m$grain %||% "month"
  m$improvement_direction <- m$improvement_direction %||% "neutral"
  m
}

#' @rdname catalogue-access
#' @export
get_component <- function(cat, id) {
  x <- cat$components[[id]]
  if (is.null(x)) stop("No component with id '", id, "'", call. = FALSE)
  x$sql <- x$sql %||% paste0(id, ".sql")
  x
}

#' @rdname catalogue-access
#' @export
component_sql <- function(cat, id) {
  x <- get_component(cat, id)
  f <- file.path(cat$sql_dir, x$sql)
  if (!file.exists(f)) stop("SQL file for component '", id, "' not found: ", f, call. = FALSE)
  paste(readLines(f, warn = FALSE), collapse = "\n")
}

#' @rdname catalogue-access
#' @param metric A metric from [get_metric()].
#' @export
metric_components <- function(metric) {
  out <- c(numerator = metric$numerator)
  if (!is.null(metric$denominator)) out <- c(out, denominator = metric$denominator)
  out
}

#' Metrics of a catalogue as a data frame
#'
#' One row per metric with the scalar fields of its YAML, convenient for an
#' index page or for mapping a pipeline over.
#'
#' @inheritParams catalogue-access
#' @return A data frame.
#' @export
metric_table <- function(cat) {
  rows <- lapply(metric_ids(cat), function(id) {
    m <- get_metric(cat, id)
    data.frame(
      id = id, title = m$title, category = m$category %||% NA_character_, type = m$type,
      numerator = m$numerator, denominator = m$denominator %||% NA_character_,
      grain = m$grain, unit = m$unit %||% NA_character_,
      improvement_direction = m$improvement_direction,
      target = as.numeric(m$target %||% NA_real_),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

#' Check a catalogue is consistent
#'
#' Every problem found is reported in a single error: unknown fields,
#' missing required fields, illegal values, references to components that
#' do not exist, missing SQL files, a denominator on a count metric or none
#' on any other, and an id used by more than one metric or component (ids
#' share one namespace, so a rate and its numerator can never be confused).
#'
#' @inheritParams catalogue-access
#' @return `TRUE`, invisibly, or an error listing every problem.
#' @export
validate_catalogue <- function(cat) {
  problems <- c(check_components(cat), check_metrics(cat))
  dup <- intersect(metric_ids(cat), component_ids(cat))
  if (length(dup) > 0) problems <- c(problems, paste0("id used by both a metric and a component: ", dup))
  if (length(problems) > 0) {
    stop("Invalid catalogue:\n", paste0("  - ", problems, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

check_components <- function(cat) {
  unlist(lapply(cat$components, function(x) {
    p <- character()
    where <- paste0("component '", x$id, "'")
    extra <- setdiff(names(x), c(component_fields, "file"))
    if (length(extra) > 0) p <- c(p, paste0(where, ": unknown field(s) ", paste(extra, collapse = ", ")))
    for (f in c("title", "aggregate")) if (is.null(x[[f]])) p <- c(p, paste0(where, ": missing ", f))
    if (!is.null(x$aggregate) && !x$aggregate %in% component_aggregates) {
      p <- c(p, paste0(where, ": aggregate must be one of ", paste(component_aggregates, collapse = ", ")))
    }
    sql <- file.path(cat$sql_dir, x$sql %||% paste0(x$id, ".sql"))
    if (!file.exists(sql)) p <- c(p, paste0(where, ": SQL file not found: ", basename(sql)))
    p
  }), use.names = FALSE)
}

check_metrics <- function(cat) {
  unlist(lapply(cat$metrics, function(m) {
    p <- character()
    where <- paste0("metric '", m$id, "'")
    extra <- setdiff(names(m), c(metric_fields, "file"))
    if (length(extra) > 0) p <- c(p, paste0(where, ": unknown field(s) ", paste(extra, collapse = ", ")))
    for (f in c("title", "type", "numerator")) if (is.null(m[[f]])) p <- c(p, paste0(where, ": missing ", f))
    if (!is.null(m$type) && !m$type %in% metric_types) {
      p <- c(p, paste0(where, ": type must be one of ", paste(metric_types, collapse = ", ")))
    }
    if (!is.null(m$numerator) && is.null(cat$components[[m$numerator]])) {
      p <- c(p, paste0(where, ": numerator component '", m$numerator, "' does not exist"))
    }
    if (identical(m$type, "count") && !is.null(m$denominator)) {
      p <- c(p, paste0(where, ": a count has no denominator"))
    }
    if (!is.null(m$type) && m$type != "count" && is.null(m$denominator)) {
      p <- c(p, paste0(where, ": a ", m$type, " needs a denominator"))
    }
    if (!is.null(m$denominator) && is.null(cat$components[[m$denominator]])) {
      p <- c(p, paste0(where, ": denominator component '", m$denominator, "' does not exist"))
    }
    if (!is.null(m$grain) && !m$grain %in% grains) p <- c(p, paste0(where, ": grain must be one of ", paste(grains, collapse = ", ")))
    if (!is.null(m$improvement_direction) && !m$improvement_direction %in% improvement_directions) {
      p <- c(p, paste0(where, ": improvement_direction must be one of ", paste(improvement_directions, collapse = ", ")))
    }
    for (f in c("multiplier", "divisor", "target")) {
      if (!is.null(m[[f]]) && !(is.numeric(m[[f]]) && length(m[[f]]) == 1 && is.finite(m[[f]]))) {
        p <- c(p, paste0(where, ": ", f, " must be a single number"))
      }
    }
    for (f in c("multiplier", "divisor")) if (is.numeric(m[[f]]) && any(m[[f]] <= 0)) p <- c(p, paste0(where, ": ", f, " must be positive"))
    if (!is.null(m$rebase)) {
      ok <- tryCatch(!anyNA(as.Date(unlist(m$rebase))), error = function(e) FALSE)
      if (!ok) p <- c(p, paste0(where, ": rebase must be a list of dates"))
    }
    p
  }), use.names = FALSE)
}

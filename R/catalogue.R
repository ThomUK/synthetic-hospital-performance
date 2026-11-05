#' The catalogue directory shipped with the package
#'
#' The catalogue is three folders and one file: `indicators/` (one YAML per
#' indicator), `components/` (one YAML per numerator or denominator), `sql/`
#' (one query per component, named `<component id>.sql` unless the
#' component's `sql` field says otherwise), `domains.yaml`, the list of
#' domain codes that prefix every id, and `dimensions.yaml`, the dimensions
#' a component may carry (see [indicator_breakdowns()]).
#'
#' Every id is `<domain>_<nnn>`, for example `ed_003`. Indicators and
#' components share the numbering within a domain, and the only id that may
#' appear in both folders is a count indicator whose numerator is itself: such
#' an indicator omits `numerator`, and the component with the same id supplies
#' the events. So a count is one id and a rate is three.
#'
#' @return A directory path.
#' @export
fhperf_catalogue_dir <- function() {
  system.file(package = "fhperf")
}

#' Read a catalogue of indicators and components
#'
#' @param path Directory holding `indicators/`, `components/` and `sql/`.
#'   Defaults to the catalogue shipped with the package.
#' @param validate Run [validate_catalogue()] on the result? Set to `FALSE`
#'   only to inspect a broken catalogue.
#' @return An object of class `fhperf_catalogue`: a list with `indicators`,
#'   `components` and `domains` (each a list named by id or code), `sql_dir`
#'   and `path`.
#' @examples
#' cat <- read_catalogue()
#' indicator_ids(cat)
#' get_indicator(cat, "ed_003")
#' @export
read_catalogue <- function(path = fhperf_catalogue_dir(), validate = TRUE) {
  if (!dir.exists(path)) stop("Catalogue directory does not exist: ", path, call. = FALSE)
  cat <- structure(
    list(
      indicators = read_yaml_dir(file.path(path, "indicators")),
      components = read_yaml_dir(file.path(path, "components")),
      domains = read_domains(file.path(path, "domains.yaml")),
      dimensions = read_dimensions(file.path(path, "dimensions.yaml")),
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

# Read domains.yaml into a list named by code. A missing file gives an empty
# list, and validation then rejects every id.
read_domains <- function(file) {
  if (!file.exists(file)) return(list())
  x <- yaml::read_yaml(file)
  if (length(x) == 0) return(list())
  codes <- vapply(x, function(d) as.character(d$code %||% NA_character_), character(1))
  if (anyNA(codes)) stop("Every domain in ", file, " needs a code", call. = FALSE)
  if (anyDuplicated(codes)) stop("Duplicate domain code in ", file, ": ", codes[duplicated(codes)][1], call. = FALSE)
  names(x) <- codes
  x
}

# Read dimensions.yaml into a list named by id.
read_dimensions <- function(file) {
  if (!file.exists(file)) return(list())
  x <- yaml::read_yaml(file)
  if (length(x) == 0) return(list())
  ids <- vapply(x, function(d) as.character(d$id %||% NA_character_), character(1))
  if (anyNA(ids)) stop("Every dimension in ", file, " needs an id", call. = FALSE)
  if (anyDuplicated(ids)) stop("Duplicate dimension id in ", file, ": ", ids[duplicated(ids)][1], call. = FALSE)
  for (d in x) if (is.null(d$title)) stop("Dimension '", d$id, "' in ", file, " needs a title", call. = FALSE)
  names(x) <- ids
  x
}

#' @export
print.fhperf_catalogue <- function(x, ...) {
  cat("<fhperf_catalogue> ", length(x$indicators), " indicators, ", length(x$components), " components, ",
      length(x$domains), " domains, ", length(x$dimensions), " dimensions\n", sep = "")
  cat("  ", x$path, "\n", sep = "")
  invisible(x)
}

#' Ids, indicators and components of a catalogue
#'
#' @param cat A catalogue from [read_catalogue()].
#' @param id An indicator or component id.
#' @return `indicator_ids()`, `component_ids()` and `domain_codes()` return
#'   character vectors; `id_domain()` returns the domain code of each id;
#'   `get_indicator()` and `get_component()` return the parsed YAML as a list,
#'   with defaults filled in (`multiplier`, `divisor`, `grain`, `sql`, and
#'   for a count indicator with no `numerator`, the indicator's own id);
#'   `component_sql()` returns the component's query as a string;
#'   `indicator_components()` returns the component ids an indicator uses, named
#'   `numerator` and (unless the indicator is a count) `denominator`.
#' @name catalogue-access
#' @export
indicator_ids <- function(cat) names(cat$indicators)

#' @rdname catalogue-access
#' @export
component_ids <- function(cat) names(cat$components)

#' @rdname catalogue-access
#' @export
domain_codes <- function(cat) names(cat$domains)

#' @rdname catalogue-access
#' @export
dimension_ids <- function(cat) names(cat$dimensions)

#' @rdname catalogue-access
#' @export
get_dimension <- function(cat, id) {
  d <- cat$dimensions[[id]]
  if (is.null(d)) stop("No dimension with id '", id, "'", call. = FALSE)
  d
}

#' @rdname catalogue-access
#' @export
id_domain <- function(id) sub(id_pattern, "\\1", id)

#' @rdname catalogue-access
#' @export
get_indicator <- function(cat, id) {
  m <- cat$indicators[[id]]
  if (is.null(m)) stop("No indicator with id '", id, "'", call. = FALSE)
  if (identical(m$type, "count")) m$numerator <- m$numerator %||% id
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
  x$dimensions <- as.character(unlist(x$dimensions))
  x$snapshot <- isTRUE(x$snapshot)
  x
}

#' Is an indicator a snapshot?
#'
#' A snapshot component measures a stock at a moment, such as a waiting
#' list, rather than a flow of events; its YAML says `snapshot: true` and
#' its SQL takes `{{snapshot_dates}}`. A period's value is then the
#' snapshot at the period's end, and periods are never summed. An
#' indicator is a snapshot when its components are.
#'
#' @inheritParams catalogue-access
#' @return `TRUE` or `FALSE`.
#' @export
is_snapshot <- function(cat, id) {
  comps <- indicator_components(get_indicator(cat, id))
  all(vapply(comps, function(c) get_component(cat, c)$snapshot, logical(1)))
}

#' Which data version a definition needs
#'
#' A component or indicator whose YAML says `since: v3` can only be built
#' from dataset version 3 or later, because the columns it reads do not
#' exist before that. An indicator needs at least what its components need.
#' `available_indicators()` and `available_components()` list what a given
#' version can build, so a site can carry definitions ahead of the data.
#'
#' @inheritParams catalogue-access
#' @param version A dataset version such as `"v2"`.
#' @return `indicator_since()` and `component_since()` return a version
#'   string, or `NA` when nothing is required; the `available_*()` functions
#'   return character vectors of ids.
#' @export
component_since <- function(cat, id) {
  get_component(cat, id)$since %||% NA_character_
}

#' @rdname component_since
#' @export
indicator_since <- function(cat, id) {
  ind <- get_indicator(cat, id)
  needs <- unname(unlist(c(list(ind$since), lapply(indicator_components(ind), function(c) get_component(cat, c)$since))))
  needs <- needs[vapply(needs, function(v) is.character(v) && grepl(version_pattern, v), logical(1))]
  if (length(needs) == 0) return(NA_character_)
  needs[which.max(version_number(needs))]
}

#' @rdname component_since
#' @export
available_indicators <- function(cat, version) {
  ids <- indicator_ids(cat)
  ids[vapply(ids, function(id) version_allows(indicator_since(cat, id), version), logical(1))]
}

#' @rdname component_since
#' @export
available_components <- function(cat, version) {
  ids <- component_ids(cat)
  ids[vapply(ids, function(id) version_allows(component_since(cat, id), version), logical(1))]
}

version_allows <- function(since, version) {
  if (is.na(since)) return(TRUE)
  if (!grepl(version_pattern, version)) stop("version must look like v2", call. = FALSE)
  version_number(version) >= version_number(since)
}

#' The dimensions an indicator can be broken down by
#'
#' A breakdown splits every component of the indicator by one dimension and
#' recomputes the series per group, so a dimension is available only when
#' all of the indicator's components carry it. An indicator's YAML may
#' narrow the choice with a `breakdowns` list; by default every shared
#' dimension is offered.
#'
#' @inheritParams catalogue-access
#' @return A character vector of dimension ids, possibly empty.
#' @export
indicator_breakdowns <- function(cat, id) {
  ind <- get_indicator(cat, id)
  shared <- Reduce(intersect, lapply(indicator_components(ind), function(c) get_component(cat, c)$dimensions))
  if (!is.null(ind$breakdowns)) return(intersect(as.character(unlist(ind$breakdowns)), shared))
  shared
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
#' @param indicator An indicator from [get_indicator()].
#' @export
indicator_components <- function(indicator) {
  out <- c(numerator = indicator$numerator)
  if (!is.null(indicator$denominator)) out <- c(out, denominator = indicator$denominator)
  out
}

#' Indicators of a catalogue as a data frame
#'
#' One row per indicator with the scalar fields of its YAML, convenient for an
#' index page or for mapping a pipeline over.
#'
#' @inheritParams catalogue-access
#' @return A data frame.
#' @export
indicator_table <- function(cat) {
  rows <- lapply(indicator_ids(cat), function(id) {
    m <- get_indicator(cat, id)
    d <- id_domain(id)
    data.frame(
      id = id, domain = d, domain_title = cat$domains[[d]]$title %||% d,
      title = m$title, type = m$type,
      numerator = m$numerator, denominator = m$denominator %||% NA_character_,
      grain = m$grain, unit = m$unit %||% NA_character_,
      improvement_direction = m$improvement_direction,
      target = as.numeric(m$target %||% NA_real_),
      breakdowns = paste(indicator_breakdowns(cat, id), collapse = ", "),
      since = indicator_since(cat, id),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

#' Components of a catalogue as a data frame
#'
#' One row per component with its domain, aggregate, source and the indicators
#' that use it, so reuse is visible.
#'
#' @inheritParams catalogue-access
#' @return A data frame with `id`, `domain`, `domain_title`, `title`,
#'   `aggregate`, `unit`, `source`, `sql` and `used_by` (indicator ids,
#'   comma-separated, each suffixed with its role).
#' @export
component_table <- function(cat) {
  uses <- lapply(indicator_ids(cat), function(id) {
    comps <- indicator_components(get_indicator(cat, id))
    data.frame(component = unname(comps), use = paste0(id, " (", names(comps), ")"), stringsAsFactors = FALSE)
  })
  uses <- do.call(rbind, c(uses, list(make.row.names = FALSE)))
  rows <- lapply(component_ids(cat), function(id) {
    x <- get_component(cat, id)
    d <- id_domain(id)
    data.frame(
      id = id, domain = d, domain_title = cat$domains[[d]]$title %||% d,
      title = x$title, aggregate = x$aggregate, snapshot = x$snapshot, unit = x$unit %||% NA_character_,
      source = x$source %||% NA_character_, sql = x$sql,
      dimensions = paste(x$dimensions, collapse = ", "),
      since = x$since %||% NA_character_,
      used_by = paste(uses$use[uses$component == id], collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, c(rows, list(make.row.names = FALSE)))
}

#' Check a catalogue is consistent
#'
#' Every problem found is reported in a single error: ids that are not
#' `<domain>_<nnn>` or whose domain is not in `domains.yaml`, unknown
#' fields, missing required fields, illegal values, references to
#' components that do not exist, missing SQL files, a denominator on a count
#' indicator or none on any other, and an id used by both an indicator and a
#' component unless the indicator is a count whose numerator is itself.
#'
#' @inheritParams catalogue-access
#' @return `TRUE`, invisibly, or an error listing every problem.
#' @export
validate_catalogue <- function(cat) {
  problems <- c(check_ids(cat), check_components(cat), check_indicators(cat))
  for (id in intersect(indicator_ids(cat), component_ids(cat))) {
    m <- cat$indicators[[id]]
    self <- identical(m$type, "count") && (is.null(m$numerator) || identical(m$numerator, id))
    if (!self) problems <- c(problems, paste0("id used by both an indicator and a component: ", id, " (only a count whose numerator is itself may share its id)"))
  }
  if (length(problems) > 0) {
    stop("Invalid catalogue:\n", paste0("  - ", problems, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

check_ids <- function(cat) {
  ids <- c(indicator_ids(cat), component_ids(cat))
  bad <- ids[!grepl(id_pattern, ids)]
  p <- if (length(bad) > 0) paste0("id '", unique(bad), "' is not <domain>_<nnn>, for example ed_001") else character()
  ok <- setdiff(ids, bad)
  unknown <- ok[!id_domain(ok) %in% domain_codes(cat)]
  if (length(unknown) > 0) {
    p <- c(p, paste0("id '", unique(unknown), "' has a domain that is not in domains.yaml (known: ",
                     paste(domain_codes(cat), collapse = ", "), ")"))
  }
  p
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
    dims <- as.character(unlist(x$dimensions))
    unknown <- setdiff(dims, names(cat$dimensions))
    if (length(unknown) > 0) p <- c(p, paste0(where, ": dimension(s) not in dimensions.yaml: ", paste(unknown, collapse = ", ")))
    if (!is.null(x$snapshot) && !is.logical(x$snapshot)) p <- c(p, paste0(where, ": snapshot must be true or false"))
    if (!is.null(x$since) && !(is.character(x$since) && length(x$since) == 1 && grepl(version_pattern, x$since))) {
      p <- c(p, paste0(where, ": since must be a dataset version such as v3"))
    }
    if (file.exists(sql)) {
      uses <- any(grepl("{{snapshot_dates}}", readLines(sql, warn = FALSE), fixed = TRUE))
      if (isTRUE(x$snapshot) && !uses) p <- c(p, paste0(where, ": a snapshot component's SQL must use {{snapshot_dates}}"))
      if (!isTRUE(x$snapshot) && uses) p <- c(p, paste0(where, ": SQL uses {{snapshot_dates}} but the component is not marked snapshot: true"))
    }
    p
  }), use.names = FALSE)
}

check_indicators <- function(cat) {
  unlist(lapply(cat$indicators, function(m) {
    p <- character()
    where <- paste0("indicator '", m$id, "'")
    extra <- setdiff(names(m), c(indicator_fields, "file"))
    if (length(extra) > 0) p <- c(p, paste0(where, ": unknown field(s) ", paste(extra, collapse = ", ")))
    for (f in c("title", "type")) if (is.null(m[[f]])) p <- c(p, paste0(where, ": missing ", f))
    if (is.null(m$numerator)) {
      if (!identical(m$type, "count")) {
        p <- c(p, paste0(where, ": missing numerator"))
      } else if (is.null(cat$components[[m$id]])) {
        p <- c(p, paste0(where, ": a count with no numerator needs a component with the same id"))
      }
    }
    if (!is.null(m$type) && !m$type %in% indicator_types) {
      p <- c(p, paste0(where, ": type must be one of ", paste(indicator_types, collapse = ", ")))
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
    comps <- c(m$numerator %||% if (identical(m$type, "count")) m$id, m$denominator)
    comps <- comps[comps %in% names(cat$components)]
    snaps <- vapply(comps, function(c) isTRUE(cat$components[[c]]$snapshot), logical(1))
    if (length(snaps) > 1 && length(unique(snaps)) > 1) {
      p <- c(p, paste0(where, ": numerator and denominator must both be snapshots, or neither"))
    }
    if (!is.null(m$since) && !(is.character(m$since) && length(m$since) == 1 && grepl(version_pattern, m$since))) {
      p <- c(p, paste0(where, ": since must be a dataset version such as v3"))
    }
    comp_since <- unlist(lapply(comps, function(c) cat$components[[c]]$since))
    comp_since <- comp_since[vapply(comp_since, function(v) is.character(v) && grepl(version_pattern, v), logical(1))]
    if (length(comp_since) > 0) {
      need <- comp_since[which.max(version_number(comp_since))]
      own <- if (is.character(m$since) && grepl(version_pattern, m$since)) m$since else "v0"
      if (version_number(own) < version_number(need)) p <- c(p, paste0(where, ": must declare since: ", need, " because a component needs it"))
    }
    if (!is.null(m$breakdowns)) {
      shared <- Reduce(intersect, lapply(comps, function(c) as.character(unlist(cat$components[[c]]$dimensions))), accumulate = FALSE)
      bad <- setdiff(as.character(unlist(m$breakdowns)), shared %||% character())
      if (length(bad) > 0) p <- c(p, paste0(where, ": breakdown(s) not carried by every component: ", paste(bad, collapse = ", ")))
    }
    p
  }), use.names = FALSE)
}

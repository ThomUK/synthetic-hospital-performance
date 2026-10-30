#' Build every breakdown of an indicator
#'
#' For each dimension the indicator can be broken down by, builds the
#' indicator per group, attaches labels from the dimension's `labels` query,
#' and optionally folds small groups into `"Other"`.
#'
#' @inheritParams build_indicator
#' @param dimensions Dimension ids to build; default all of
#'   [indicator_breakdowns()].
#' @param min_per_period Groups whose denominator (numerator, for a count)
#'   averages fewer than this per period over the window are folded into one
#'   `"Other"` group; see [fold_small_groups()]. `0` folds nothing.
#' @return A list named by dimension id of grouped indicator series, each
#'   with `group` and `label` columns and the attribute `dimension`.
#' @export
build_breakdowns <- function(con, cat, id, window = NULL, cache = new.env(),
                             dimensions = indicator_breakdowns(cat, id), min_per_period = 0) {
  window <- resolve_window(window)
  out <- lapply(dimensions, function(dim) {
    x <- build_indicator(con, cat, id, window, cache, by = dim)
    x <- fold_small_groups(x, min_per_period)
    label_groups(x, dimension_labels(con, cat, dim, cache))
  })
  names(out) <- dimensions
  out
}

#' Labels for a dimension's codes
#'
#' Runs the dimension's `labels` query, if it has one.
#'
#' @inheritParams build_indicator
#' @param id A dimension id.
#' @return A data frame with `code` and `label`, or `NULL` when the
#'   dimension has no labels query.
#' @export
dimension_labels <- function(con, cat, id, cache = new.env()) {
  dim <- get_dimension(cat, id)
  if (is.null(dim$labels)) return(NULL)
  key <- paste("labels", id, sep = "|")
  if (is.null(cache[[key]])) {
    lab <- run_sql(con, dim$labels)
    if (!all(c("code", "label") %in% names(lab))) stop("The labels query of dimension '", id, "' must return code and label", call. = FALSE)
    lab$code <- as.character(lab$code)
    lab$label <- as.character(lab$label)
    cache[[key]] <- lab[!duplicated(lab$code), c("code", "label")]
  }
  cache[[key]]
}

# Add a label column to a grouped series: the dimension's label for the
# code, or the code itself when there is none. "Unknown" and "Other" stay.
label_groups <- function(data, labels) {
  if (!"group" %in% names(data)) stop("data is not grouped", call. = FALSE)
  lab <- data$group
  if (!is.null(labels)) {
    hit <- match(data$group, labels$code)
    lab <- ifelse(is.na(hit), data$group, labels$label[hit])
  }
  if ("label" %in% names(data)) lab <- ifelse(data$group == "Other", data$label, lab)
  attrs <- attributes(data)
  data$label <- lab
  data <- data[, c("period", "group", "label", setdiff(names(data), c("period", "group", "label")))]
  attributes(data) <- c(attributes(data)[c("names", "row.names")], attrs[setdiff(names(attrs), c("names", "row.names"))])
  data
}

#' Fold small groups of a breakdown into "Other"
#'
#' A group whose denominator (numerator, for a count) averages fewer than
#' `min_per_period` events per period is merged with the others like it
#' into one group coded `"Other"`, whose label says how many groups it
#' holds. Values are recomputed from the summed components, so nothing is
#' averaged. `"Unknown"` is never folded.
#'
#' @param data A grouped indicator series from [build_indicator()] with `by`.
#' @param min_per_period The threshold; `0` folds nothing.
#' @return The series with the small groups replaced by `"Other"`.
#' @export
fold_small_groups <- function(data, min_per_period = 0) {
  if (!"group" %in% names(data)) stop("data is not grouped", call. = FALSE)
  if (min_per_period <= 0) return(data)
  ind <- attr(data, "indicator")
  size <- if (ind$type == "count") data$numerator else data$denominator
  per_group <- tapply(size, data$group, mean)
  small <- names(per_group)[per_group < min_per_period & names(per_group) != "Unknown"]
  if (length(small) < 2) return(data)
  attrs <- attributes(data)
  d <- as.data.frame(data)
  d$group[d$group %in% small] <- "Other"
  num <- stats::aggregate(numerator ~ period + group, d, sum)
  den <- stats::aggregate(denominator ~ period + group, d, sum, na.action = stats::na.pass)
  d <- merge(num, den, by = c("period", "group"))
  scale <- ind$multiplier / ind$divisor
  d$value <- if (ind$type == "count") d$numerator * scale else ifelse(d$denominator > 0, d$numerator / d$denominator * scale, NA_real_)
  d$label <- ifelse(d$group == "Other", paste0("Other (", length(small), " groups)"), NA_character_)
  d <- d[order(d$group, d$period), c("period", "group", "label", "numerator", "denominator", "value")]
  rownames(d) <- NULL
  attributes(d) <- c(attributes(d)[c("names", "row.names")], attrs[setdiff(names(attrs), c("names", "row.names"))])
  d
}

#' One group of a breakdown as an ordinary indicator series
#'
#' @param data A grouped indicator series.
#' @param code A value of its `group` column.
#' @return An ungrouped indicator series for that group, whose indicator
#'   title carries the group label, with the attribute `group` (a list of
#'   `code` and `label`).
#' @export
group_series <- function(data, code) {
  if (!"group" %in% names(data)) stop("data is not grouped", call. = FALSE)
  rows <- data$group == code
  if (!any(rows)) stop("No group '", code, "'", call. = FALSE)
  ind <- attr(data, "indicator")
  label <- if ("label" %in% names(data)) data$label[rows][1] else code
  ind$title <- paste0(ind$title, ": ", label)
  out <- as.data.frame(data)[rows, c("period", "numerator", "denominator", "value")]
  rownames(out) <- NULL
  structure(out, class = c("fhperf_indicator_data", "data.frame"), indicator = ind,
            dimension = attr(data, "dimension"), group = list(code = code, label = label))
}

#' Summarise every group of a breakdown
#'
#' One row per group with the latest point's value, variation and
#' assurance (see [indicator_summary()]), the plot-the-dots icon names for
#' them, and the group's size, ordered by size.
#'
#' @param data A grouped indicator series.
#' @return A data frame with `group`, `label`, `size` (total denominator, or
#'   numerator for a count), `periods` (with a value), `period`, `value`,
#'   `mean`, `variation`, `variation_icon`, `target`, `assurance` and
#'   `assurance_icon`.
#' @export
breakdown_summary <- function(data) {
  if (!"group" %in% names(data)) stop("data is not grouped", call. = FALSE)
  ind <- attr(data, "indicator")
  rows <- lapply(unique(data$group), function(code) {
    g <- group_series(data, code)
    s <- indicator_summary(g)
    size <- if (ind$type == "count") sum(g$numerator) else sum(g$denominator)
    data.frame(
      group = code, label = attr(g, "group")$label, size = size, periods = sum(!is.na(g$value)),
      period = s$period, value = s$value, mean = s$mean,
      variation = s$variation, variation_icon = variation_icon(s$variation, s$value, s$mean),
      target = s$target, assurance = s$assurance, assurance_icon = assurance_icon(s$assurance),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, c(rows, list(make.row.names = FALSE)))
  out <- out[order(-out$size), ]
  rownames(out) <- NULL
  out
}

# The plot-the-dots icon for a variation type. Improvement and concern
# icons differ by whether the point sits above or below the mean.
variation_icon <- function(variation, value, mean) {
  high <- !is.na(value) && !is.na(mean) && value >= mean
  switch(variation,
    common_cause = "common_cause",
    special_cause_improvement = if (high) "improvement_high" else "improvement_low",
    special_cause_concern = if (high) "concern_high" else "concern_low",
    special_cause_neutral_high = "neutral_high",
    special_cause_neutral_low = "neutral_low",
    NA_character_
  )
}

assurance_icon <- function(assurance) {
  if (is.na(assurance)) return(NA_character_)
  switch(assurance, pass = "pass", fail = "fail", inconsistent = "inconsistent", NA_character_)
}

#' A plot-the-dots icon as inline SVG
#'
#' Reads one of the Making Data Count icons shipped with NHSRplotthedots and
#' returns its markup sized for a table cell, for use in HTML output.
#'
#' @param name An icon name from [breakdown_summary()], such as
#'   `"common_cause"` or `"pass"`.
#' @param type `"variation"` or `"assurance"`.
#' @param size Width and height in pixels.
#' @return A string of SVG, or `""` for a missing name.
#' @export
ptd_icon_svg <- function(name, type = c("variation", "assurance"), size = 28) {
  type <- match.arg(type)
  if (is.na(name) || !nzchar(name)) return("")
  file <- system.file("icons", type, paste0(name, ".svg"), package = "NHSRplotthedots")
  if (!nzchar(file)) return("")
  svg <- paste(readLines(file, warn = FALSE), collapse = "")
  svg <- sub('<svg width="[0-9.]+" height="[0-9.]+"', sprintf('<svg width="%d" height="%d" viewBox="0 0 378 378" role="img" aria-label="%s"', size, size, gsub("_", " ", name)), svg)
  svg
}

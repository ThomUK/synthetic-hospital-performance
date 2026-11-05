# Pre-render step for the Quarto site.
#
# Builds every indicator once, saves the results to _data/, and writes one
# indicators/<id>.qmd page per indicator from _templates/indicator.qmd. The pages
# read from _data/ so no page opens the database. This script is the thing
# a {targets} pipeline will replace: it is the only place the database is
# touched during a render.
library(fhperf)

scale <- Sys.getenv("FHPERF_SCALE", unset = "sample")
data_version <- Sys.getenv("FHPERF_DATA_VERSION", unset = "v3")
message("Building indicators from the ", scale, " set of data version ", data_version)

cat <- read_catalogue()
con <- perf_connect(scale, version = data_version, quiet = TRUE)
on.exit(perf_disconnect(con), add = TRUE)
ids <- indicator_ids(cat)
comp_ids <- component_ids(cat)

window <- dataset_window(data_version)
cache <- new.env()
indicators <- build_indicators(con, cat, window = window, cache = cache)
summaries <- do.call(rbind, lapply(indicators, indicator_summary))

# Breakdowns: every indicator by every dimension its components share.
# Groups averaging fewer than this many events per period fold into "Other".
min_per_period <- as.numeric(Sys.getenv("FHPERF_MIN_PER_PERIOD", unset = "5"))
breakdowns <- lapply(ids, function(id) {
  build_breakdowns(con, cat, id, window = window, cache = cache, min_per_period = min_per_period)
})
names(breakdowns) <- ids
breakdown_summaries <- lapply(breakdowns, function(by_dim) lapply(by_dim, breakdown_summary))

# Every component charted as a series of its own, with its 6-12 view: the
# component pages, and the numerator and denominator charts under each
# indicator's main chart.
components <- lapply(comp_ids, function(c) list(
  series = component_series(con, cat, c, grain = "month", window = window, cache = cache),
  six_twelve = six_twelve_component(con, cat, c, window = window, cache = cache)
))
names(components) <- comp_ids

# The 6-12 view for every indicator and every breakdown group, as of the
# window's last day.
six <- lapply(ids, function(id) six_twelve(con, cat, id, window = window, cache = cache))
names(six) <- ids
six_breakdowns <- lapply(ids, function(id) {
  lapply(breakdowns[[id]], function(x) {
    dim <- attr(x, "dimension")$id
    codes <- unique(x$group)
    out <- lapply(codes, function(code) six_twelve(con, cat, id, window = window, cache = cache, by = dim, group = group_codes(x, code)))
    names(out) <- codes
    out
  })
})
names(six_breakdowns) <- ids

dir.create("_data", showWarnings = FALSE)
saveRDS(list(
  built_at = Sys.time(), scale = scale, data_version = data_version,
  window = window, min_per_period = min_per_period,
  catalogue = cat, table = indicator_table(cat), indicators = indicators, summaries = summaries,
  breakdowns = breakdowns, breakdown_summaries = breakdown_summaries,
  six_twelve = six, six_twelve_breakdowns = six_breakdowns, components = components
), "_data/indicators.rds")

# One page per indicator. Existing generated pages are removed first so that a
# indicator deleted from the catalogue disappears from the site.
dir.create("indicators", showWarnings = FALSE)
unlink(list.files("indicators", pattern = "\\.qmd$", full.names = TRUE))
template <- readLines("_templates/indicator.qmd")
for (id in ids) {
  m <- get_indicator(cat, id)
  page <- template
  page <- gsub("{{id}}", id, page, fixed = TRUE)
  page <- gsub("{{title}}", gsub('"', "'", m$title, fixed = TRUE), page, fixed = TRUE)
  writeLines(page, file.path("indicators", paste0(id, ".qmd")))
}
n_groups <- sum(unlist(lapply(breakdowns, function(b) vapply(b, function(x) length(unique(x$group)), numeric(1)))))
# And one page per component.
dir.create("components", showWarnings = FALSE)
unlink(list.files("components", pattern = "\\.qmd$", full.names = TRUE))
template <- readLines("_templates/component.qmd")
for (id in comp_ids) {
  comp <- get_component(cat, id)
  page <- template
  page <- gsub("{{id}}", id, page, fixed = TRUE)
  page <- gsub("{{title}}", gsub('"', "'", comp$title, fixed = TRUE), page, fixed = TRUE)
  writeLines(page, file.path("components", paste0(id, ".qmd")))
}
message("Built ", length(indicators), " indicators, ", length(components), " components, ", n_groups,
        " breakdown groups, and wrote ", length(ids) + length(comp_ids), " pages")

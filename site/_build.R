# Pre-render step for the Quarto site.
#
# Builds every indicator once, saves the results to _data/, and writes one
# indicators/<id>.qmd page per indicator from _templates/indicator.qmd. The pages
# read from _data/ so no page opens the database. This script is the thing
# a {targets} pipeline will replace: it is the only place the database is
# touched during a render.
library(fhperf)

scale <- Sys.getenv("FHPERF_SCALE", unset = "sample")
message("Building indicators from the ", scale, " set")

cat <- read_catalogue()
con <- perf_connect(scale, quiet = TRUE)
on.exit(perf_disconnect(con), add = TRUE)

window <- dataset_window()
cache <- new.env()
indicators <- build_indicators(con, cat, window = window, cache = cache)
summaries <- do.call(rbind, lapply(indicators, indicator_summary))

# Breakdowns: every indicator by every dimension its components share.
# Groups averaging fewer than this many events per period fold into "Other".
min_per_period <- as.numeric(Sys.getenv("FHPERF_MIN_PER_PERIOD", unset = "5"))
breakdowns <- lapply(indicator_ids(cat), function(id) {
  build_breakdowns(con, cat, id, window = window, cache = cache, min_per_period = min_per_period)
})
names(breakdowns) <- indicator_ids(cat)
breakdown_summaries <- lapply(breakdowns, function(by_dim) lapply(by_dim, breakdown_summary))

dir.create("_data", showWarnings = FALSE)
saveRDS(list(
  built_at = Sys.time(), scale = scale, window = window, min_per_period = min_per_period,
  catalogue = cat, table = indicator_table(cat), indicators = indicators, summaries = summaries,
  breakdowns = breakdowns, breakdown_summaries = breakdown_summaries
), "_data/indicators.rds")

# One page per indicator. Existing generated pages are removed first so that a
# indicator deleted from the catalogue disappears from the site.
dir.create("indicators", showWarnings = FALSE)
unlink(list.files("indicators", pattern = "\\.qmd$", full.names = TRUE))
template <- readLines("_templates/indicator.qmd")
for (id in indicator_ids(cat)) {
  m <- get_indicator(cat, id)
  page <- template
  page <- gsub("{{id}}", id, page, fixed = TRUE)
  page <- gsub("{{title}}", gsub('"', "'", m$title, fixed = TRUE), page, fixed = TRUE)
  writeLines(page, file.path("indicators", paste0(id, ".qmd")))
}
n_groups <- sum(unlist(lapply(breakdowns, function(b) vapply(b, function(x) length(unique(x$group)), numeric(1)))))
message("Built ", length(indicators), " indicators, ", n_groups, " breakdown groups, and wrote ", length(indicator_ids(cat)), " pages")

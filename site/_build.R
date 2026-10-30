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
indicators <- build_indicators(con, cat, window = window)
summaries <- do.call(rbind, lapply(indicators, indicator_summary))

dir.create("_data", showWarnings = FALSE)
saveRDS(list(
  built_at = Sys.time(), scale = scale, window = window,
  catalogue = cat, table = indicator_table(cat), indicators = indicators, summaries = summaries
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
message("Built ", length(indicators), " indicators and wrote ", length(indicator_ids(cat)), " pages")

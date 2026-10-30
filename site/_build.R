# Pre-render step for the Quarto site.
#
# Builds every metric once, saves the results to _data/, and writes one
# metrics/<id>.qmd page per metric from _templates/metric.qmd. The pages
# read from _data/ so no page opens the database. This script is the thing
# a {targets} pipeline will replace: it is the only place the database is
# touched during a render.
library(fhperf)

scale <- Sys.getenv("FHPERF_SCALE", unset = "sample")
message("Building metrics from the ", scale, " set")

cat <- read_catalogue()
con <- perf_connect(scale, quiet = TRUE)
on.exit(perf_disconnect(con), add = TRUE)

window <- dataset_window()
metrics <- build_metrics(con, cat, window = window)
summaries <- do.call(rbind, lapply(metrics, metric_summary))

dir.create("_data", showWarnings = FALSE)
saveRDS(list(
  built_at = Sys.time(), scale = scale, window = window,
  catalogue = cat, table = metric_table(cat), metrics = metrics, summaries = summaries
), "_data/metrics.rds")

# One page per metric. Existing generated pages are removed first so that a
# metric deleted from the catalogue disappears from the site.
dir.create("metrics", showWarnings = FALSE)
unlink(list.files("metrics", pattern = "\\.qmd$", full.names = TRUE))
template <- readLines("_templates/metric.qmd")
for (id in metric_ids(cat)) {
  m <- get_metric(cat, id)
  page <- template
  page <- gsub("{{id}}", id, page, fixed = TRUE)
  page <- gsub("{{title}}", gsub('"', "'", m$title, fixed = TRUE), page, fixed = TRUE)
  writeLines(page, file.path("metrics", paste0(id, ".qmd")))
}
message("Built ", length(metrics), " metrics and wrote ", length(metric_ids(cat)), " pages")

# A DuckDB connection to the sample, for the integration tests.
#
# Three ways to find the data, tried in order:
#   1. FHPERF_SAMPLE_DIR: a directory holding the sample's layer folders
#      (layer1/, reference/, derived/, ...), for example a checkout of
#      synthetic-hospital-data at data/v2/sample. Views are created directly.
#   2. A sibling checkout of synthetic-hospital-data next to this repository.
#   3. fakehospdata::fh_connect("sample"), which downloads into its cache.
# Without duckdb, or without any of these, the tests that need data skip.
local_sample_con <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("duckdb")
  dir <- Sys.getenv("FHPERF_SAMPLE_DIR", unset = "")
  if (!nzchar(dir)) {
    sibling <- testthat::test_path("..", "..", "..", "synthetic-hospital-data", "data", "v2", "sample")
    if (dir.exists(sibling)) dir <- sibling
  }
  con <- if (nzchar(dir) && dir.exists(dir)) {
    sample_dir_con(dir)
  } else if (requireNamespace("fakehospdata", quietly = TRUE) && !identical(Sys.getenv("FHPERF_OFFLINE"), "true")) {
    fakehospdata::fh_connect("sample", quiet = TRUE)
  } else {
    testthat::skip("No sample data available")
  }
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = env)
  con
}

sample_dir_con <- function(dir) {
  con <- DBI::dbConnect(duckdb::duckdb())
  for (schema in c("layer1", "reference", "derived", "aggregate")) {
    files <- list.files(file.path(dir, schema), pattern = "\\.parquet$", full.names = TRUE)
    if (length(files) == 0) next
    DBI::dbExecute(con, paste0("CREATE SCHEMA IF NOT EXISTS ", schema))
    for (f in files) {
      name <- sub("\\.parquet$", "", basename(f))
      DBI::dbExecute(con, sprintf("CREATE VIEW %s.%s AS SELECT * FROM read_parquet('%s')", schema, name, gsub("'", "''", normalizePath(f))))
    }
  }
  con
}

sample_window <- as.Date(c("2023-04-01", "2026-03-31"))

# Write a throwaway catalogue to a temp directory. `indicators` and
# `components` are lists of lists; `sql` is a named character vector of
# component id -> query.
write_catalogue <- function(indicators = list(), components = list(), sql = character(),
                            domains = list(list(code = "t", title = "Test"), list(code = "u", title = "Other")),
                            env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  for (d in c("indicators", "components", "sql")) dir.create(file.path(dir, d))
  if (!is.null(domains)) yaml::write_yaml(domains, file.path(dir, "domains.yaml"))
  for (m in indicators) yaml::write_yaml(m, file.path(dir, "indicators", paste0(m$id, ".yaml")))
  for (x in components) yaml::write_yaml(x, file.path(dir, "components", paste0(x$id, ".yaml")))
  for (id in names(sql)) writeLines(sql[[id]], file.path(dir, "sql", paste0(id, ".sql")))
  dir
}

count_component <- function(id = "t_001", sql = "SELECT d AS event_date FROM t") {
  list(component = list(id = id, title = id, aggregate = "count"), sql = stats::setNames(sql, id))
}

toy_indicator <- function(id = "t_002", type = "count", numerator = "t_001", denominator = NULL, ...) {
  m <- list(id = id, title = id, type = type, numerator = numerator, ...)
  if (!is.null(denominator)) m$denominator <- denominator
  m
}

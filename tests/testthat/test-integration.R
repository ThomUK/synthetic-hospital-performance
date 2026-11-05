# These tests need DuckDB and the sample data; see helper-data.R.

test_that("every component in the catalogue runs against the sample", {
  con <- local_sample_con()
  cat <- read_catalogue()
  for (id in available_components(cat, sample_version)) {
    ev <- read_component_events(con, cat, id, window = sample_window)
    expect_gt(nrow(ev), 0, label = paste("rows of", id))
    expect_s3_class(ev$event_date, "Date")
    if (get_component(cat, id)$aggregate == "sum") expect_true(all(ev$value >= 0), info = id)
  }
})

test_that("every indicator builds a complete monthly series from the sample", {
  con <- local_sample_con()
  cat <- read_catalogue()
  ids <- available_indicators(cat, sample_version)
  res <- build_indicators(con, cat, ids = ids, window = sample_window)
  expect_named(res, ids)
  for (id in names(res)) {
    x <- res[[id]]
    expect_equal(nrow(x), 36, info = id)
    expect_equal(x$period[1], as.Date("2023-04-01"), info = id)
    expect_false(anyNA(x$value), info = id)
    expect_s3_class(plot_indicator(x), "ggplot")
    expect_equal(nrow(indicator_summary(x)), 1)
  }
  pct <- indicator_table(cat)$id[indicator_table(cat)$type == "percentage"]
  for (id in pct) expect_true(all(res[[id]]$value >= 0 & res[[id]]$value <= 100), info = id)
  # the window clips events before the dataset starts
  expect_equal(sum(res$apc_001$numerator), DBI::dbGetQuery(con, "SELECT count(*) AS n FROM derived.spells WHERE emergency AND admidate >= DATE '2023-04-01'")$n)
  # reused denominators agree between indicators, and a count is its own numerator
  expect_equal(res$ed_003$denominator, res$diag_002$denominator)
  expect_equal(res$ed_003$denominator, res$ed_001$numerator)
})

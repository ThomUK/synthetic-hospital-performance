# These tests need DuckDB and the sample data; see helper-data.R.

test_that("every component in the catalogue runs against the sample", {
  con <- local_sample_con()
  cat <- read_catalogue()
  for (id in component_ids(cat)) {
    ev <- read_component_events(con, cat, id, window = sample_window)
    expect_gt(nrow(ev), 0, label = paste("rows of", id))
    expect_s3_class(ev$event_date, "Date")
    if (get_component(cat, id)$aggregate == "sum") expect_true(all(ev$value >= 0), info = id)
  }
})

test_that("every metric builds a complete monthly series from the sample", {
  con <- local_sample_con()
  cat <- read_catalogue()
  res <- build_metrics(con, cat, window = sample_window)
  expect_named(res, metric_ids(cat))
  for (id in names(res)) {
    x <- res[[id]]
    expect_equal(nrow(x), 36, info = id)
    expect_equal(x$period[1], as.Date("2023-04-01"), info = id)
    expect_false(anyNA(x$value), info = id)
    expect_s3_class(plot_metric(x), "ggplot")
    expect_equal(nrow(metric_summary(x)), 1)
  }
  pct <- metric_table(cat)$id[metric_table(cat)$type == "percentage"]
  for (id in pct) expect_true(all(res[[id]]$value >= 0 & res[[id]]$value <= 100), info = id)
  # the window clips events before the dataset starts
  expect_equal(sum(res$emergency_admissions_count$numerator), DBI::dbGetQuery(con, "SELECT count(*) AS n FROM derived.spells WHERE emergency AND admidate >= DATE '2023-04-01'")$n)
  # reused denominators agree between metrics
  expect_equal(res$ed_4h_performance$denominator, res$imaging_per_1000_ed_attendances$denominator)
  expect_equal(res$ed_4h_performance$denominator, res$ed_attendances_count$numerator)
})

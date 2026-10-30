test_that("window placeholders are substituted or demanded", {
  sql <- "SELECT d FROM t WHERE d BETWEEN {{window_start}} AND {{window_end}}"
  w <- as.Date(c("2024-01-01", "2024-12-31"))
  expect_equal(
    fhperf:::substitute_window(sql, w, "x"),
    "SELECT d FROM t WHERE d BETWEEN DATE '2024-01-01' AND DATE '2024-12-31'"
  )
  expect_error(fhperf:::substitute_window(sql, NULL, "x"), "needs a window")
  expect_equal(fhperf:::substitute_window("SELECT 1", NULL, "x"), "SELECT 1")
})

test_that("events are checked against the component contract", {
  cnt <- list(id = "c", aggregate = "count")
  sm <- list(id = "s", aggregate = "sum")
  expect_error(fhperf:::check_events(data.frame(x = 1), cnt), "no event_date column")
  expect_error(fhperf:::check_events(data.frame(event_date = "2024-01-01"), sm), "no value column")
  expect_error(fhperf:::check_events(data.frame(event_date = "2024-01-01", value = "a"), sm), "must be numeric")
  expect_error(fhperf:::check_events(data.frame(event_date = "yesterday"), cnt), "cannot be read as a date")
  expect_warning(
    ok <- fhperf:::check_events(data.frame(event_date = c("2024-01-01", NA), value = c(1, 2)), sm),
    "1 of 2 events have no event_date"
  )
  expect_equal(nrow(ok), 1)
  expect_s3_class(ok$event_date, "Date")
  ts <- fhperf:::check_events(data.frame(event_date = as.POSIXct("2024-01-01 10:00:00", tz = "UTC")), cnt)
  expect_equal(ts$event_date, as.Date("2024-01-01"))
})

test_that("read_component_events runs the SQL through DBI", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE t AS SELECT * FROM (VALUES (DATE '2024-01-01', 5), (DATE '2024-01-02', 7)) v(d, n)")
  comp <- list(id = "t_001", title = "m", aggregate = "sum")
  dir <- write_catalogue(components = list(comp), sql = c(t_001 = "SELECT d AS event_date, n AS value FROM t WHERE d <= {{window_end}}"))
  cat <- read_catalogue(dir)
  ev <- read_component_events(con, cat, "t_001", window = as.Date(c("2024-01-01", "2024-01-01")))
  expect_equal(ev$value, 5)
  agg <- build_component(con, cat, "t_001", "day", window = as.Date(c("2024-01-01", "2024-01-03")))
  expect_equal(agg$value, c(5, 7, 0))
  expect_equal(attr(agg, "component"), "t_001")
})

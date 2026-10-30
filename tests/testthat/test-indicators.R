periods <- as.Date(c("2024-01-01", "2024-02-01", "2024-03-01"))
num <- data.frame(period = periods, value = c(90, 80, 0))
den <- data.frame(period = periods, value = c(100, 100, 0))

test_that("combine_components computes each indicator type", {
  pct <- combine_components(num, den, list(id = "p", title = "p", type = "percentage", multiplier = 100, divisor = 1, grain = "month"))
  expect_s3_class(pct, "fhperf_indicator_data")
  expect_equal(names(pct), c("period", "numerator", "denominator", "value"))
  expect_equal(pct$value, c(90, 80, NA))
  rate <- combine_components(num, den, list(id = "r", title = "r", type = "rate", multiplier = 1000, divisor = 1, grain = "month"))
  expect_equal(rate$value, c(900, 800, NA))
  mean_days <- combine_components(
    data.frame(period = periods, value = c(2880, 1440, 0)), data.frame(period = periods, value = c(2, 2, 1)),
    list(id = "m", title = "m", type = "mean", multiplier = 1, divisor = 1440, grain = "month")
  )
  expect_equal(mean_days$value, c(1, 0.5, 0))
  cnt <- combine_components(num, NULL, list(id = "c", title = "c", type = "count", multiplier = 1, divisor = 1, grain = "month"))
  expect_equal(cnt$value, c(90, 80, 0))
  expect_true(all(is.na(cnt$denominator)))
  expect_output(print(cnt), "fhperf_indicator_data")
})

test_that("combine_components aligns periods and checks the denominator", {
  m <- list(id = "p", title = "p", type = "percentage", multiplier = 100, divisor = 1, grain = "month")
  short_den <- den[1:2, ]
  out <- combine_components(num, short_den, m)
  expect_equal(out$denominator, c(100, 100, 0))
  expect_equal(out$value, c(90, 80, NA))
  out <- combine_components(num[2:3, ], den, m)
  expect_equal(out$numerator, c(0, 80, 0))
  expect_error(combine_components(num, den, list(type = "count", multiplier = 1, divisor = 1)), "takes no denominator")
  expect_error(combine_components(num, NULL, list(type = "rate", multiplier = 1, divisor = 1)), "needs a denominator")
})

test_that("format_value follows the indicator type", {
  expect_equal(format_value(c(1234.4, NA), list(type = "count")), c("1,234", NA))
  expect_equal(format_value(95.456, list(type = "percentage")), "95.5%")
  expect_equal(format_value(3.14159, list(type = "mean")), "3.1")
  expect_equal(format_value(176, list(type = "rate")), "176.0")
})

test_that("build_indicator shares components through the cache", {
  skip_if_not_installed("duckdb")
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  DBI::dbExecute(con, "CREATE TABLE t AS SELECT * FROM (VALUES (DATE '2024-01-05', TRUE), (DATE '2024-01-20', FALSE), (DATE '2024-02-02', TRUE)) v(d, hit)")
  all <- count_component("t_001", "SELECT d AS event_date FROM t")
  hits <- count_component("t_002", "SELECT d AS event_date FROM t WHERE hit")
  dir <- write_catalogue(
    indicators = list(
      list(id = "t_001", title = "all, counted", type = "count"),
      toy_indicator("t_003", "percentage", "t_002", "t_001", target = 50, improvement_direction = "increase"),
      toy_indicator("t_004", "rate", "t_002", "t_001")
    ),
    components = list(all$component, hits$component), sql = c(all$sql, hits$sql)
  )
  cat <- read_catalogue(dir)
  w <- as.Date(c("2024-01-01", "2024-03-31"))
  res <- build_indicators(con, cat, window = w)
  expect_named(res, c("t_001", "t_003", "t_004"))
  expect_equal(res$t_001$value, c(2, 1, 0))
  expect_equal(res$t_003$value, c(50, 100, NA))
  expect_equal(res$t_004$value, c(500, 1000, NA))
  # a cache passed explicitly is filled with one entry per component
  cache <- new.env()
  build_indicator(con, cat, "t_003", window = w, cache = cache)
  build_indicator(con, cat, "t_001", window = w, cache = cache)
  expect_equal(length(ls(cache)), 2)
})

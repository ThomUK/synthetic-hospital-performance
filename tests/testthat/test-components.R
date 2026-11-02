test_that("a component can be charted as a count series of its own", {
  cat <- read_catalogue()
  ind <- component_indicator(cat, "apc_003")
  expect_equal(ind$type, "count")
  expect_equal(ind$numerator, "apc_003")
  expect_equal(ind$unit, "minutes")
  expect_equal(ind$title, "Inpatient bed-minutes")
  expect_true(ind$component)
  expect_error(component_indicator(cat, "zz_999"), "No component")
})

test_that("component_series and six_twelve_component agree with the indicator's parts", {
  days <- seq(as.Date("2024-04-01"), as.Date("2026-03-31"), by = "day")
  a <- count_component("t_001", "SELECT d AS event_date FROM t")
  b <- list(component = list(id = "t_002", title = "minutes", aggregate = "sum", unit = "minutes"), sql = c(t_002 = "SELECT d AS event_date, v AS value FROM t"))
  dir <- write_catalogue(
    indicators = list(list(id = "t_001", title = "all", type = "count"), toy_indicator("t_003", "mean", "t_002", "t_001")),
    components = list(a$component, b$component), sql = c(a$sql, b$sql)
  )
  cat <- read_catalogue(dir)
  reader <- function(sql) {
    if (grepl("v AS value", sql)) data.frame(event_date = days, value = 30) else data.frame(event_date = rep(days, each = 2))
  }
  w <- as.Date(c("2024-04-01", "2026-03-31"))
  cache <- new.env()
  mean_ind <- build_indicator(reader, cat, "t_003", window = w, cache = cache)
  num <- component_series(reader, cat, "t_002", window = w, cache = cache)
  den <- component_series(reader, cat, "t_001", window = w, cache = cache)
  expect_s3_class(num, "fhperf_indicator_data")
  expect_equal(num$value, mean_ind$numerator)
  expect_equal(den$value, mean_ind$denominator)
  expect_equal(attr(num, "indicator")$title, "minutes")
  expect_true(all(is.na(num$denominator)))
  expect_equal(nrow(component_series(reader, cat, "t_001", grain = "week", window = w, cache = cache)), 105)
  # every chart type draws it
  expect_s3_class(plot_indicator(num), "ggplot")
  expect_s3_class(plot_indicator_seasons(num), "ggplot")
  six <- six_twelve_component(reader, cat, "t_002", window = w, cache = cache)
  expect_s3_class(six, "fhperf_six_twelve")
  expect_equal(six$weekly$value, rep(7 * 30, 6))
  expect_equal(six$indicator$id, "t_002")
  expect_equal(six$stats$value[six$stats$stat == "YTD"], 365 * 30)
  expect_s3_class(plot_six_twelve(six), "ggplot")
  # the reader was called once per component across all of these
  n <- 0
  counting <- function(sql) { n <<- n + 1; reader(sql) }
  cache2 <- new.env()
  build_indicator(counting, cat, "t_003", window = w, cache = cache2)
  component_series(counting, cat, "t_002", window = w, cache = cache2)
  six_twelve_component(counting, cat, "t_001", window = w, cache = cache2)
  expect_equal(n, 2)
})

test_that("snapshot components chart as stocks", {
  con <- local_sample_con()
  cat <- read_catalogue()
  s <- component_series(con, cat, "rtt_001", window = sample_window)
  expect_equal(nrow(s), 36)
  expect_false(anyNA(s$value))
  six <- six_twelve_component(con, cat, "rtt_001", window = sample_window)
  expect_true(six$snapshot)
  expect_equal(six$stats$stat, c("LastWk", "LastWk", "Latest"))
})

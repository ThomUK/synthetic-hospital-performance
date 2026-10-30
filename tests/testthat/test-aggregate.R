test_that("period_start floors to month, Monday and day", {
  d <- as.Date(c("2024-02-14", "2024-02-29", "2024-03-04"))
  expect_equal(period_start(d, "month"), as.Date(c("2024-02-01", "2024-02-01", "2024-03-01")))
  expect_equal(period_start(d, "week"), as.Date(c("2024-02-12", "2024-02-26", "2024-03-04")))
  expect_equal(period_start(d, "day"), d)
  expect_equal(period_start("2024-01-31", "month"), as.Date("2024-01-01"))
})

test_that("period_seq covers every period touching the window", {
  expect_equal(period_seq(as.Date(c("2024-01-15", "2024-03-02")), "month"), as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")))
  expect_equal(length(period_seq(as.Date(c("2023-04-01", "2026-03-31")), "month")), 36)
  expect_equal(period_seq(as.Date(c("2024-01-03", "2024-01-09")), "week"), as.Date(c("2024-01-01", "2024-01-08")))
  expect_error(period_seq(as.Date(c("2024-01-09", "2024-01-03")), "week"), "start then end")
  expect_error(period_seq("2024-01-09", "day"), "two dates")
})

test_that("aggregate_events counts and sums into complete periods", {
  ev <- data.frame(
    event_date = as.Date(c("2024-01-03", "2024-01-20", "2024-03-01", "2023-12-31", "2024-04-01")),
    value = c(10, 20, 5, 100, 100)
  )
  w <- as.Date(c("2024-01-01", "2024-03-31"))
  n <- aggregate_events(ev, "month", "count", window = w)
  expect_equal(n$period, as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")))
  expect_equal(n$value, c(2, 0, 1))
  s <- aggregate_events(ev, "month", "sum", window = w)
  expect_equal(s$value, c(30, 0, 5))
  # without a window: no clipping, no filling
  all <- aggregate_events(ev, "month", "count")
  expect_equal(all$period, as.Date(c("2023-12-01", "2024-01-01", "2024-03-01", "2024-04-01")))
  expect_equal(all$value, c(1, 2, 1, 1))
  # weekly and daily grains
  wk <- aggregate_events(ev, "week", "count", window = as.Date(c("2024-01-01", "2024-01-21")))
  expect_equal(wk$value, c(1, 0, 1))
  dy <- aggregate_events(ev, "day", "sum", window = as.Date(c("2024-01-02", "2024-01-04")))
  expect_equal(dy$value, c(0, 10, 0))
})

test_that("aggregate_events handles no events in the window", {
  ev <- data.frame(event_date = as.Date("2020-01-01"))
  out <- aggregate_events(ev, "month", "count", window = as.Date(c("2024-01-01", "2024-02-28")))
  expect_equal(out$value, c(0, 0))
  out <- aggregate_events(ev[0, , drop = FALSE], "month", "count")
  expect_equal(nrow(out), 0)
})

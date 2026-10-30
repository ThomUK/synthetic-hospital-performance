series <- function(values, metric) {
  periods <- seq(as.Date("2023-04-01"), by = "month", length.out = length(values))
  combine_components(data.frame(period = periods, value = values), NULL, metric)
}
count_metric <- list(id = "c", title = "Counts", type = "count", multiplier = 1, divisor = 1, grain = "month", improvement_direction = "neutral", unit = "things")

test_that("metric_spc returns plot-the-dots data with the metric's options", {
  set.seed(1)
  x <- series(round(100 + rnorm(24, sd = 5)), count_metric)
  spc <- metric_spc(x)
  expect_s3_class(spc, "ptd_spc_df")
  expect_equal(nrow(spc), 24)
  expect_true(all(c("mean_col", "lpl", "upl", "point_type") %in% names(spc)))
  with_target <- series(round(100 + rnorm(24, sd = 5)), modifyList(count_metric, list(target = 90, improvement_direction = "increase")))
  spc2 <- metric_spc(with_target)
  expect_equal(unique(spc2$target), 90)
  expect_error(metric_spc(data.frame(period = 1, value = 1)), "no metric attribute")
})

test_that("periods without a value are left out of the analysis", {
  x <- series(c(10, 12, NA, 11, 9, 10, 12, 11, 10, 9, 11, 12, 10, 11, 12, 9, 10, 11), count_metric)
  spc <- metric_spc(x)
  expect_equal(nrow(spc), 17)
})

test_that("rebase dates from the YAML are honoured", {
  m <- modifyList(count_metric, list(rebase = list("2024-07-01")))
  x <- series(c(rep(10, 15), rep(20, 15)) + rep(c(0, 1), 15), m)
  spc <- as.data.frame(metric_spc(x))
  expect_equal(length(unique(spc$rebase_group)), 2)
})

test_that("plot_metric returns a ggplot", {
  set.seed(2)
  x <- series(round(100 + rnorm(24, sd = 5)), count_metric)
  p <- plot_metric(x)
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$title, "Counts")
  p2 <- plot_metric(x, point_size = 1, main_title = "Other")
  expect_equal(p2$labels$title, "Other")
})

test_that("metric_summary reports the latest point and assurance", {
  set.seed(3)
  m <- modifyList(count_metric, list(target = 50, improvement_direction = "increase"))
  x <- series(round(100 + rnorm(24, sd = 5)), m)
  s <- metric_summary(x)
  expect_equal(nrow(s), 1)
  expect_equal(s$period, as.Date("2025-03-01"))
  expect_equal(s$value, x$value[24])
  expect_equal(s$assurance, "pass")
  expect_true(s$variation %in% c("common_cause", "special_cause_improvement", "special_cause_concern", "special_cause_neither"))
  s2 <- metric_summary(series(round(100 + rnorm(24, sd = 5)), count_metric))
  expect_true(is.na(s2$assurance))
  expect_true(is.na(s2$target))
})

test_that("assurance_type follows the plot-the-dots icon rules", {
  at <- fhperf:::assurance_type
  expect_equal(at(80, 120, 50, "increase"), "pass")
  expect_equal(at(80, 120, 150, "increase"), "fail")
  expect_equal(at(80, 120, 100, "increase"), "inconsistent")
  expect_equal(at(80, 120, 150, "decrease"), "pass")
  expect_equal(at(80, 120, 50, "decrease"), "fail")
  expect_equal(at(80, 120, 100, "decrease"), "inconsistent")
  expect_true(is.na(at(80, 120, 100, "neutral")))
  expect_true(is.na(at(NA, 120, 100, "increase")))
})

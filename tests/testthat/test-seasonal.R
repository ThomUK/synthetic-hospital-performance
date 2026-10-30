series <- function(values, indicator, start = "2023-04-01") {
  periods <- seq(as.Date(start), by = "month", length.out = length(values))
  combine_components(data.frame(period = periods, value = values), NULL, indicator)
}
count_ind <- list(id = "t_001", title = "Counts", type = "count", multiplier = 1, divisor = 1, grain = "month",
                  improvement_direction = "neutral", unit = "things")

test_that("financial_year labels April to March", {
  d <- as.Date(c("2023-04-01", "2024-03-31", "2024-04-01", "2025-01-15", "1999-04-01"))
  expect_equal(financial_year(d), c("2023/24", "2023/24", "2024/25", "2024/25", "1999/00"))
  expect_equal(financial_year(d, start_month = 1), c("2023", "2024", "2024", "2025", "1999"))
  expect_equal(financial_year(as.Date("2024-09-30"), start_month = 10), "2023/24")
})

test_that("fy_position counts from the start of the financial year", {
  d <- as.Date(c("2024-04-01", "2024-12-01", "2025-03-01"))
  expect_equal(fy_position(d, "month"), c(1L, 9L, 12L))
  expect_equal(fy_position(as.Date(c("2024-04-01", "2024-04-07", "2024-04-08", "2025-03-31")), "week"), c(1L, 1L, 2L, 53L))
  expect_equal(fy_position(as.Date(c("2024-04-01", "2025-03-31")), "day"), c(1L, 365L))
  expect_equal(fy_position(as.Date("2024-01-01"), "month", start_month = 1), 1L)
})

test_that("plot_indicator_seasons draws one line per financial year", {
  set.seed(4)
  x <- series(round(100 + rnorm(36, sd = 5)), count_ind)
  p <- plot_indicator_seasons(x)
  expect_s3_class(p, "ggplot")
  d <- fhperf:::season_data(x)
  expect_equal(levels(d$year), c("2023/24", "2024/25", "2025/26"))
  expect_equal(range(d$position), c(1, 12))
  expect_equal(p$labels$title, "Counts")
  # a target becomes a dashed line; NA values are tolerated
  with_target <- series(c(NA, round(100 + rnorm(23, sd = 5))), modifyList(count_ind, list(target = 90)))
  p2 <- plot_indicator_seasons(with_target)
  expect_true(any(vapply(p2$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))))
  expect_false(any(vapply(plot_indicator_seasons(with_target, show_target = FALSE)$layers, function(l) inherits(l$geom, "GeomHline"), logical(1))))
  expect_error(plot_indicator_seasons(data.frame(period = 1, value = 1)), "no indicator attribute")
})

test_that("more than eight years fold the oldest into Earlier", {
  x <- series(rep(10, 12 * 10), count_ind, start = "2015-04-01")
  d <- fhperf:::season_data(x)
  expect_equal(levels(d$year)[1], "Earlier")
  expect_equal(length(levels(d$year)), 8)
  expect_equal(sum(d$year == "Earlier"), 36)
  expect_s3_class(plot_indicator_seasons(x), "ggplot")
})

test_that("a partial first year starts mid-axis", {
  x <- series(1:6, count_ind, start = "2024-10-01")
  d <- fhperf:::season_data(x)
  expect_equal(d$position, 7:12)
  expect_equal(unique(as.character(d$year)), "2024/25")
})

test_that("spread_labels separates close labels and leaves distant ones", {
  sp <- fhperf:::spread_labels
  expect_equal(sp(c(10, 50), 5), c(10, 50))
  out <- sp(c(76.5, 76.6, 77.5), 2)
  expect_true(all(diff(sort(out)) >= 2 - 1e-9))
  expect_equal(mean(out), mean(c(76.5, 76.6, 77.5)))
  expect_equal(order(out), order(c(76.5, 76.6, 77.5)))
  expect_equal(sp(5, 1), 5)
  expect_equal(sp(c(1, 1.1), 0), c(1, 1.1))
})

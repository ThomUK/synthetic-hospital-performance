# A two-year daily flow: numerator 10 a day this year and 8 a day the year
# before, denominator 100 a day throughout. as_of is Tuesday 31 March 2026.
days <- seq(as.Date("2024-04-01"), as.Date("2026-03-31"), by = "day")
num <- data.frame(period = days, value = ifelse(days >= as.Date("2025-04-01"), 10, 8))
den <- data.frame(period = days, value = 100)
as_of <- as.Date("2026-03-31")
pct <- list(id = "t_003", title = "Rate", type = "percentage", multiplier = 100, divisor = 1, grain = "month",
            improvement_direction = "increase", target = 12, unit = "%")
cnt <- list(id = "t_001", title = "Count", type = "count", multiplier = 1, divisor = 1, grain = "month",
            improvement_direction = "neutral", unit = "things")

test_that("the six weeks are the last complete Monday-to-Sunday weeks", {
  x <- six_twelve_data(num, NULL, cnt, as_of)
  expect_s3_class(x, "fhperf_six_twelve")
  expect_equal(nrow(x$weekly), 6)
  expect_equal(x$weekly$end[6], as.Date("2026-03-29"))
  expect_equal(x$weekly$start[1], as.Date("2026-02-16"))
  expect_equal(format(x$weekly$start, "%u"), rep("1", 6))
  expect_equal(x$weekly$value, rep(70, 6))
  expect_equal(x$weekly$prior, rep(56, 6))
  expect_equal(x$weekly$label, paste0("wk ", 8:13))
  # a Sunday as_of counts its own week
  y <- six_twelve_data(num, NULL, cnt, as.Date("2026-03-29"))
  expect_equal(y$weekly$end[6], as.Date("2026-03-29"))
})

test_that("the twelve months are the last complete months with prior-year partners", {
  x <- six_twelve_data(num, NULL, cnt, as_of)
  expect_equal(nrow(x$monthly), 12)
  expect_equal(x$monthly$start[1], as.Date("2025-04-01"))
  expect_equal(x$monthly$end[12], as.Date("2026-03-31"))
  expect_equal(x$monthly$label[c(1, 12)], c("Apr 25", "Mar 26"))
  expect_equal(x$monthly$value, as.numeric(x$monthly$end - x$monthly$start + 1) * 10)
  expect_equal(x$monthly$prior[1], 30 * 8)
  expect_equal(x$monthly$prior[12], 31 * 8)
  # mid-month as_of drops the partial month
  y <- six_twelve_data(num, NULL, cnt, as.Date("2026-03-15"))
  expect_equal(y$monthly$end[12], as.Date("2026-02-28"))
  expect_equal(y$monthly$prior[12], 28 * 8)
  # a range outside the data is NA, not zero
  short <- num[num$period >= as.Date("2025-06-01"), ]
  z <- six_twelve_data(short, NULL, cnt, as_of)
  expect_true(is.na(z$monthly$value[1]))
  expect_true(all(is.na(z$monthly$prior)))
})

test_that("to-date figures run from the month, April quarter and April year to as_of", {
  x <- six_twelve_data(num, NULL, cnt, as_of)
  s <- x$stats
  expect_equal(s$stat, c("LastWk", "LastWk", "MTD", "QTD", "YTD"))
  expect_equal(s$comparison, c("WOW", "YOY", "YOY", "YOY", "YOY"))
  expect_equal(s$value, c(70, 70, 310, 900, 3650))
  expect_equal(s$change, c(0, 25, 25, 25, 25))
  y <- six_twelve_data(num, NULL, cnt, as.Date("2026-02-10"))
  expect_equal(y$stats$value[y$stats$stat == "MTD"], 100)
  expect_equal(y$stats$value[y$stats$stat == "QTD"], 410)
  expect_equal(y$stats$value[y$stats$stat == "YTD"], 3160)
  # the year can start in another month
  z <- six_twelve_data(num, NULL, cnt, as_of, start_month = 1)
  expect_equal(z$stats$value[z$stats$stat == "YTD"], 900)
  expect_equal(z$stats$value[z$stats$stat == "QTD"], 900)
})

test_that("rates divide summed components and percentages change in points", {
  x <- six_twelve_data(num, den, pct, as_of)
  expect_equal(x$weekly$value, rep(10, 6))
  expect_equal(x$weekly$prior, rep(8, 6))
  expect_equal(x$change_unit, "pp")
  expect_equal(x$stats$change, c(0, 2, 2, 2, 2))
  expect_equal(unique(x$weekly$target), 12)
  f <- format_six_twelve(x)
  expect_equal(names(f), c("LastWk", "WOW", "YOY", "MTD", "YOY", "QTD", "YOY", "YTD", "YOY"))
  expect_equal(unname(unlist(f)), c("10.0%", "+0.0 pp", "+2.0 pp", "10.0%", "+2.0 pp", "10.0%", "+2.0 pp", "10.0%", "+2.0 pp"))
  g <- format_six_twelve(six_twelve_data(num, NULL, cnt, as_of))
  expect_equal(unname(unlist(g))[1:3], c("70", "+0%", "+25%"))
  expect_output(print(x), "as of 2026-03-31")
  # a zero denominator gives NA, and a zero prior gives no change
  zero_den <- den
  zero_den$value[zero_den$period >= as.Date("2026-03-23")] <- 0
  z <- six_twelve_data(num, zero_den, pct, as_of)
  expect_true(is.na(z$weekly$value[6]))
  expect_equal(format_six_twelve(z)$WOW, "")
})

test_that("snapshots take the stock at each range's end", {
  snaps <- snapshot_dates(as.Date(c("2024-04-01", "2026-03-31")))
  stock <- data.frame(period = days, value = NA_real_)
  stock$value[stock$period %in% snaps] <- as.numeric(format(stock$period[stock$period %in% snaps], "%j"))
  x <- six_twelve_data(stock, NULL, cnt, as_of, snapshot = TRUE)
  expect_equal(x$weekly$value, as.numeric(format(x$weekly$end, "%j")))
  expect_equal(x$monthly$value, as.numeric(format(x$monthly$end, "%j")))
  prior_ends <- fhperf:::month_end(fhperf:::shift_year(x$monthly$start, -1))
  expect_equal(x$monthly$prior, as.numeric(format(prior_ends, "%j")))
  expect_equal(x$stats$stat, c("LastWk", "LastWk", "Latest"))
  expect_equal(x$stats$value[3], 90)
  expect_equal(x$stats$change[3], 0)
  expect_equal(names(format_six_twelve(x)), c("LastWk", "WOW", "YOY", "Latest", "YOY"))
  expect_true(grepl("Snapshot", plot_six_twelve(x, stats = FALSE)$labels$subtitle))
})

test_that("plot_six_twelve returns a two-panel chart with the summary row beneath", {
  x <- six_twelve_data(num, den, pct, as_of)
  p <- plot_six_twelve(x, stats = FALSE)
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_equal(length(unique(b$layout$layout$PANEL)), 2)
  expect_equal(p$labels$title, "Rate")
  # no target, no triangle layer
  p2 <- plot_six_twelve(six_twelve_data(num, NULL, cnt, as_of), stats = FALSE)
  expect_false(any(vapply(p2$layers, function(l) identical(l$aes_params$shape, 17), logical(1))))
  expect_true(any(vapply(p$layers, function(l) identical(l$aes_params$shape, 17), logical(1))))
  # with the row, the result is still a ggplot (a patchwork of two)
  full <- plot_six_twelve(x)
  expect_s3_class(full, "ggplot")
  expect_s3_class(full, "patchwork")
  expect_equal(length(full$patches$plots), 1)
  panel <- fhperf:::six_twelve_stats_panel(x)
  expect_equal(nrow(panel$data), 18)
  expect_equal(panel$data$text[1:3], c("LastWk", "WOW", "YOY"))
  expect_equal(panel$data$text[10:12], c("10.0%", "+0.0 pp", "+2.0 pp"))
})

test_that("snapshot_dates and the placeholder substitution", {
  d <- snapshot_dates(as.Date(c("2024-01-01", "2024-02-15")), extra = as.Date("2023-02-15"))
  expect_true(all(as.Date(c("2024-01-31", "2024-01-07", "2024-02-11", "2024-02-15", "2023-02-15")) %in% d))
  expect_false(as.Date("2024-01-02") %in% d)
  expect_equal(d, sort(unique(d)))
  sql <- fhperf:::substitute_window("SELECT unnest({{snapshot_dates}}) AS d", as.Date(c("2024-01-01", "2024-01-08")), "x")
  expect_equal(sql, "SELECT unnest([DATE '2024-01-07', DATE '2024-01-08']) AS d")
  expect_error(fhperf:::substitute_window("SELECT {{snapshot_dates}}", NULL, "x"), "needs a window")
})

test_that("aggregate_events keeps only period-end snapshots and fills NA", {
  ev <- data.frame(event_date = as.Date(c("2024-01-07", "2024-01-14", "2024-01-31", "2024-02-04", "2024-02-29")))
  w <- as.Date(c("2024-01-01", "2024-03-31"))
  monthly <- aggregate_events(ev, "month", "count", window = w, snapshot = TRUE)
  expect_equal(monthly$value, c(1, 1, NA))
  weekly <- aggregate_events(ev, "week", "count", window = w, snapshot = TRUE)
  expect_equal(weekly$value[1:3], c(1, 1, NA))
  expect_equal(sum(weekly$value, na.rm = TRUE), 3)
  daily <- aggregate_events(ev, "day", "count", window = w, snapshot = TRUE)
  expect_equal(nrow(daily), 91)
  expect_equal(sum(!is.na(daily$value)), 5)
  grouped <- aggregate_events(cbind(ev, g = c("a", "a", "b", "b", "b")), "month", "count", window = w, by = "g", snapshot = TRUE)
  # a group with no period-end snapshot has no rows at all
  expect_equal(unique(grouped$group), "b")
  expect_equal(grouped$value[grouped$group == "b"], c(1, 1, NA))
})

test_that("the validator checks snapshot flags against the SQL", {
  cat <- read_catalogue()
  expect_true(is_snapshot(cat, "rtt_003"))
  expect_false(is_snapshot(cat, "ed_003"))
  expect_true(component_table(cat)$snapshot[component_table(cat)$id == "rtt_001"])
  flagged <- count_component("t_001", "SELECT d AS event_date FROM t")
  flagged$component$snapshot <- TRUE
  unflagged <- count_component("t_002", "SELECT unnest({{snapshot_dates}}) AS event_date")
  snap <- count_component("t_003", "SELECT unnest({{snapshot_dates}}) AS event_date")
  snap$component$snapshot <- TRUE
  flow <- count_component("t_004")
  dir <- write_catalogue(
    indicators = list(toy_indicator("t_005", "percentage", "t_003", "t_004")),
    components = list(flagged$component, unflagged$component, snap$component, flow$component),
    sql = c(flagged$sql, unflagged$sql, snap$sql, flow$sql)
  )
  err <- tryCatch(read_catalogue(dir), error = function(e) conditionMessage(e))
  expect_match(err, "t_001': a snapshot component's SQL must use")
  expect_match(err, "t_002': SQL uses \\{\\{snapshot_dates\\}\\} but the component is not marked")
  expect_match(err, "t_005': numerator and denominator must both be snapshots, or neither")
})

test_that("six_twelve runs through a function connection, whole and by group", {
  a <- count_component("t_001", "SELECT d AS event_date, g FROM t", dimensions = "g")
  b <- count_component("t_002", "SELECT d AS event_date, g FROM t WHERE hit", dimensions = "g")
  dir <- write_catalogue(
    indicators = list(list(id = "t_001", title = "all", type = "count"), toy_indicator("t_003", "percentage", "t_002", "t_001")),
    components = list(a$component, b$component), sql = c(a$sql, b$sql)
  )
  cat <- read_catalogue(dir)
  reader <- function(sql) {
    all <- data.frame(event_date = rep(days, each = 2), g = rep(c("x", "y"), length(days)))
    # hits are Monday to Wednesday, so every week has 3 of 7 for each group
    if (grepl("WHERE hit", sql)) all[as.integer(format(all$event_date, "%u")) <= 3, ] else all
  }
  w <- as.Date(c("2024-04-01", "2026-03-31"))
  x <- six_twelve(reader, cat, "t_001", window = w)
  expect_equal(x$as_of, as_of)
  expect_equal(x$weekly$value, rep(14, 6))
  expect_equal(x$stats$value[x$stats$stat == "YTD"], 730)
  gx <- six_twelve(reader, cat, "t_001", window = w, by = "g", group = "x")
  expect_equal(gx$weekly$value, rep(7, 6))
  both <- six_twelve(reader, cat, "t_001", window = w, by = "g", group = c("x", "y"))
  expect_equal(both$weekly$value, rep(14, 6))
  r <- six_twelve(reader, cat, "t_003", window = w, by = "g", group = "x")
  expect_equal(r$weekly$value, rep(300 / 7, 6))
  expect_error(six_twelve(reader, cat, "t_001", window = w, by = "g"), "group is needed")
  expect_error(six_twelve(reader, cat, "t_001", window = w, by = "h", group = "x"), "cannot be broken down")
})

test_that("group_codes expands Other to the folded codes", {
  x <- grouped_series(list(A = rep(100, 3), B = rep(1, 3), C = rep(2, 3)), cnt)
  f <- fold_small_groups(x, 5)
  expect_equal(sort(group_codes(f, "Other")), c("B", "C"))
  expect_equal(group_codes(f, "A"), "A")
  expect_equal(group_codes(x, "Other"), "Other")
})

test_that("the RTT snapshot has weekly points on the sample", {
  con <- local_sample_con()
  cat <- read_catalogue()
  x <- six_twelve(con, cat, "rtt_003", window = sample_window)
  expect_true(x$snapshot)
  expect_false(anyNA(x$weekly$value))
  expect_false(anyNA(x$monthly$value))
  expect_true(all(x$weekly$value > 0 & x$weekly$value < 100))
  expect_equal(x$stats$stat, c("LastWk", "LastWk", "Latest"))
  whole <- build_indicator(con, cat, "rtt_003", window = sample_window)
  expect_equal(x$monthly$value[12], whole$value[36])
  ed <- six_twelve(con, cat, "ed_003", window = sample_window)
  expect_false(anyNA(ed$weekly$value))
  expect_false(anyNA(ed$stats$value))
})

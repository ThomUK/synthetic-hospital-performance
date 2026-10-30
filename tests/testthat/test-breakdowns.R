pct <- list(id = "t_003", title = "Rate", type = "percentage", multiplier = 100, divisor = 1, grain = "month",
            improvement_direction = "increase", target = 50)
cnt <- list(id = "t_001", title = "Count", type = "count", multiplier = 1, divisor = 1, grain = "month",
            improvement_direction = "neutral")

test_that("aggregate_events splits by a column and completes every group", {
  ev <- data.frame(
    event_date = as.Date(c("2024-01-03", "2024-01-20", "2024-03-01", "2024-02-10", "2024-02-11")),
    value = c(10, 20, 5, 1, 2),
    site = c("A", "A", "B", NA, "")
  )
  w <- as.Date(c("2024-01-01", "2024-03-31"))
  out <- aggregate_events(ev, "month", "count", window = w, by = "site")
  expect_equal(names(out), c("period", "group", "value"))
  expect_equal(nrow(out), 9)
  expect_equal(sort(unique(out$group)), c("A", "B", "Unknown"))
  expect_equal(out$value[out$group == "A"], c(2, 0, 0))
  expect_equal(out$value[out$group == "B"], c(0, 0, 1))
  expect_equal(out$value[out$group == "Unknown"], c(0, 2, 0))
  # groups add up to the whole
  whole <- aggregate_events(ev, "month", "count", window = w)
  expect_equal(tapply(out$value, out$period, sum), tapply(whole$value, whole$period, sum))
  sums <- aggregate_events(ev, "month", "sum", window = w, by = "site")
  expect_equal(sums$value[sums$group == "A"], c(30, 0, 0))
  expect_error(aggregate_events(ev, "month", "count", by = "ward"), "no column 'ward'")
  # without a window, only observed periods appear, per group
  nowin <- aggregate_events(ev, "month", "count", by = "site")
  expect_equal(nrow(nowin), 9)
})

test_that("combine_components aligns grouped numerators and denominators", {
  periods <- as.Date(c("2024-01-01", "2024-02-01"))
  num <- data.frame(period = rep(periods, 2), group = rep(c("A", "B"), each = 2), value = c(5, 6, 1, 0))
  den <- data.frame(period = rep(periods, 2), group = rep(c("A", "B"), each = 2), value = c(10, 12, 2, 0))
  out <- combine_components(num, den, pct)
  expect_equal(names(out), c("period", "group", "numerator", "denominator", "value"))
  expect_equal(out$value[out$group == "A"], c(50, 50))
  expect_equal(out$value[out$group == "B"], c(50, NA))
  # a group missing from one side is filled with zeros
  out2 <- combine_components(num[num$group == "A", ], den, pct)
  expect_equal(out2$numerator[out2$group == "B"], c(0, 0))
  expect_error(combine_components(num, den[, c("period", "value")], pct), "both be grouped")
  cnt_out <- combine_components(num, NULL, cnt)
  expect_equal(cnt_out$value, c(5, 6, 1, 0))
  expect_output(print(structure(out, dimension = list(id = "site"))), "broken down by site")
})

test_that("group_series extracts one group and indicator_spc refuses several", {
  x <- grouped_series(list(A = c(40, 45, 50, 55), B = c(10, 20, 30, 40)), pct)
  x <- fhperf:::label_groups(x, data.frame(code = c("A", "B"), label = c("Alpha", "Beta")))
  expect_equal(names(x)[1:3], c("period", "group", "label"))
  g <- group_series(x, "B")
  expect_s3_class(g, "fhperf_indicator_data")
  expect_equal(g$value, c(10, 20, 30, 40))
  expect_equal(attr(g, "indicator")$title, "Rate: Beta")
  expect_equal(attr(g, "group"), list(code = "B", label = "Beta"))
  expect_equal(attr(g, "dimension")$id, "g")
  expect_error(group_series(x, "Z"), "No group")
  expect_error(indicator_spc(x), "several groups")
  expect_error(group_series(as.data.frame(g), "A"), "not grouped")
  # codes without a label keep the code
  y <- fhperf:::label_groups(grouped_series(list(A = 1:3, Q = 1:3), cnt), data.frame(code = "A", label = "Alpha"))
  expect_equal(unique(y$label[y$group == "Q"]), "Q")
  z <- fhperf:::label_groups(grouped_series(list(A = 1:3), cnt), NULL)
  expect_equal(unique(z$label), "A")
})

test_that("fold_small_groups merges small groups into Other and keeps totals", {
  vals <- list(A = rep(50, 6), B = rep(20, 6), C = rep(30, 6), Unknown = rep(10, 6))
  dens <- list(A = rep(1000, 6), B = rep(4, 6), C = rep(6, 6), Unknown = rep(2, 6))
  x <- grouped_series(vals, pct, dens)
  before <- tapply(x$numerator, x$period, sum)
  f <- fold_small_groups(x, min_per_period = 10)
  expect_equal(sort(unique(f$group)), c("A", "Other", "Unknown"))
  expect_equal(unique(f$label[f$group == "Other"]), "Other (2 groups)")
  expect_equal(unique(f$numerator[f$group == "Other"]), 50)
  expect_equal(unique(f$denominator[f$group == "Other"]), 10)
  expect_equal(unique(f$value[f$group == "Other"]), 500)
  expect_equal(tapply(f$numerator, f$period, sum), before)
  expect_s3_class(f, "fhperf_indicator_data")
  expect_equal(attr(f, "indicator")$id, "t_003")
  # one small group alone is not folded; threshold 0 folds nothing
  expect_equal(sort(unique(fold_small_groups(grouped_series(vals[c("A", "B")], pct, dens[c("A", "B")]), 10)$group)), c("A", "B"))
  expect_identical(fold_small_groups(x, 0), x)
  # counts fold on their numerator, and labels survive a later label pass
  c_ <- fold_small_groups(grouped_series(list(A = rep(100, 3), B = rep(1, 3), C = rep(2, 3)), cnt), 5)
  expect_equal(unique(c_$value[c_$group == "Other"]), 3)
  c_ <- fhperf:::label_groups(c_, data.frame(code = "A", label = "Alpha"))
  expect_equal(unique(c_$label[c_$group == "Other"]), "Other (2 groups)")
})

test_that("breakdown_summary gives one row per group with icons, largest first", {
  set.seed(5)
  x <- grouped_series(
    list(A = round(60 + rnorm(24, sd = 3)), B = round(40 + rnorm(24, sd = 3))), pct,
    list(A = rep(100, 24), B = rep(500, 24))
  )
  x <- fhperf:::label_groups(x, data.frame(code = c("A", "B"), label = c("Alpha", "Beta")))
  s <- breakdown_summary(x)
  expect_equal(s$group, c("B", "A"))
  expect_equal(s$label, c("Beta", "Alpha"))
  expect_equal(s$size, c(12000, 2400))
  expect_equal(s$periods, c(24, 24))
  expect_equal(s$assurance, c("fail", "pass"))
  expect_equal(s$assurance_icon, c("fail", "pass"))
  expect_true(all(s$variation_icon %in% c("common_cause", "improvement_high", "improvement_low", "concern_high", "concern_low")))
  expect_error(breakdown_summary(group_series(x, "A")), "not grouped")
})

test_that("icon names follow plot-the-dots conventions", {
  vi <- fhperf:::variation_icon
  expect_equal(vi("common_cause", 1, 2), "common_cause")
  expect_equal(vi("special_cause_improvement", 5, 2), "improvement_high")
  expect_equal(vi("special_cause_improvement", 1, 2), "improvement_low")
  expect_equal(vi("special_cause_concern", 5, 2), "concern_high")
  expect_equal(vi("special_cause_concern", 1, 2), "concern_low")
  expect_equal(vi("special_cause_neutral_high", 5, 2), "neutral_high")
  expect_true(is.na(vi("odd", 1, 2)))
  ai <- fhperf:::assurance_icon
  expect_equal(ai("pass"), "pass")
  expect_true(is.na(ai(NA)))
  svg <- ptd_icon_svg("common_cause", "variation", size = 20)
  expect_match(svg, '^<svg width="20" height="20" viewBox="0 0 378 378"')
  expect_match(svg, 'aria-label="common cause"')
  expect_equal(ptd_icon_svg(NA, "assurance"), "")
  expect_equal(ptd_icon_svg("nope", "assurance"), "")
})

test_that("indicator_breakdowns is the shared dimensions unless narrowed", {
  cat <- read_catalogue()
  expect_equal(dimension_ids(cat), c("site", "specialty", "arrival_mode"))
  expect_equal(indicator_breakdowns(cat, "apc_004"), c("site", "specialty"))
  expect_equal(indicator_breakdowns(cat, "ed_003"), "arrival_mode")
  expect_equal(indicator_breakdowns(cat, "ed_008"), c("arrival_mode", "specialty"))
  expect_equal(indicator_breakdowns(cat, "ed_001"), "arrival_mode")
  expect_equal(indicator_breakdowns(cat, "diag_002"), character(0))
  expect_equal(indicator_breakdowns(cat, "rtt_003"), "specialty")
  expect_equal(get_dimension(cat, "site")$title, "Site")
  expect_error(get_dimension(cat, "ward"), "No dimension")
  tab <- indicator_table(cat)
  expect_equal(tab$breakdowns[tab$id == "apc_004"], "site, specialty")
  expect_equal(component_table(cat)$dimensions[component_table(cat)$id == "apc_002"], "site, specialty")
  # an explicit list narrows, and the validator rejects one a component lacks
  a <- count_component("t_001", "SELECT d AS event_date, g, h FROM t", dimensions = c("g", "h"))
  b <- count_component("t_002", "SELECT d AS event_date, g FROM t", dimensions = "g")
  dir <- write_catalogue(
    indicators = list(toy_indicator("t_003", "percentage", "t_002", "t_001"), toy_indicator("t_004", "percentage", "t_002", "t_001", breakdowns = list("g")),
                      list(id = "t_001", title = "self", type = "count", breakdowns = list("h"))),
    components = list(a$component, b$component), sql = c(a$sql, b$sql)
  )
  cat2 <- read_catalogue(dir)
  expect_equal(indicator_breakdowns(cat2, "t_003"), "g")
  expect_equal(indicator_breakdowns(cat2, "t_004"), "g")
  expect_equal(indicator_breakdowns(cat2, "t_001"), "h")
  bad <- count_component("t_005", "SELECT d AS event_date FROM t", dimensions = "ward")
  dir2 <- write_catalogue(
    indicators = list(toy_indicator("t_006", "percentage", "t_002", "t_001", breakdowns = list("h"))),
    components = list(a$component, b$component, bad$component), sql = c(a$sql, b$sql, bad$sql)
  )
  err <- tryCatch(read_catalogue(dir2), error = function(e) conditionMessage(e))
  expect_match(err, "t_005': dimension\\(s\\) not in dimensions.yaml: ward")
  expect_match(err, "t_006': breakdown\\(s\\) not carried by every component: h")
  dir3 <- write_catalogue(dimensions = NULL, components = list(bad$component), sql = bad$sql)
  expect_error(read_catalogue(dir3), "not in dimensions.yaml")
})

test_that("a function can stand in for the connection", {
  a <- count_component("t_001", "SELECT d AS event_date, g FROM t", dimensions = "g")
  dir <- write_catalogue(indicators = list(list(id = "t_001", title = "self", type = "count")),
                         components = list(a$component), sql = a$sql)
  cat <- read_catalogue(dir)
  reader <- function(sql) {
    if (grepl("labels", sql)) return(data.frame(code = "x", label = "Ex"))
    data.frame(event_date = as.Date(c("2024-01-01", "2024-01-02", "2024-02-01")), g = c("x", "y", NA))
  }
  w <- as.Date(c("2024-01-01", "2024-02-29"))
  whole <- build_indicator(reader, cat, "t_001", window = w)
  expect_equal(whole$value, c(2, 1))
  bd <- build_breakdowns(reader, cat, "t_001", window = w, dimensions = "g")
  expect_named(bd, "g")
  expect_equal(sort(unique(bd$g$group)), c("Unknown", "x", "y"))
  expect_equal(tapply(bd$g$numerator, bd$g$period, sum), tapply(whole$numerator, whole$period, sum))
  expect_equal(attr(bd$g, "dimension")$title, "Group")
  expect_error(build_indicator(reader, cat, "t_001", window = w, by = "h"), "cannot be broken down by 'h'")
  # events are read once per component even across the whole and the breakdown
  n <- 0
  counting <- function(sql) { n <<- n + 1; reader(sql) }
  cache <- new.env()
  build_indicator(counting, cat, "t_001", window = w, cache = cache)
  build_breakdowns(counting, cat, "t_001", window = w, cache = cache)
  expect_equal(n, 1)
  # the labels query of dimension h is used and checked
  expect_equal(dimension_labels(reader, cat, "h")$label, "Ex")
  expect_null(dimension_labels(reader, cat, "g"))
  expect_error(dimension_labels(function(sql) data.frame(x = 1), cat, "h"), "must return code and label")
})

test_that("breakdowns of the sample reconcile to the whole", {
  con <- local_sample_con()
  cat <- read_catalogue()
  cache <- new.env()
  whole <- build_indicator(con, cat, "apc_004", window = sample_window, cache = cache)
  bd <- build_breakdowns(con, cat, "apc_004", window = sample_window, cache = cache, min_per_period = 5)
  expect_named(bd, c("site", "specialty"))
  for (dim in names(bd)) {
    x <- bd[[dim]]
    expect_equal(tapply(x$numerator, x$period, sum), tapply(whole$numerator, whole$period, sum), info = dim)
    expect_equal(tapply(x$denominator, x$period, sum), tapply(whole$denominator, whole$period, sum), info = dim)
    s <- breakdown_summary(x)
    expect_equal(nrow(s), length(unique(x$group)))
    expect_s3_class(plot_indicator(group_series(x, s$group[1])), "ggplot")
  }
  expect_true(all(bd$site$label %in% c("Hospital A", "Hospital B", "Unknown")))
  expect_true("General Internal Medicine" %in% bd$specialty$label)
  ed <- build_breakdowns(con, cat, "ed_003", window = sample_window, cache = cache)
  expect_equal(sort(unique(ed$arrival_mode$label)), sort(unique(ed$arrival_mode$group)))
})

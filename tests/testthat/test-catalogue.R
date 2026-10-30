test_that("the shipped catalogue is valid and well formed", {
  cat <- read_catalogue()
  expect_s3_class(cat, "fhperf_catalogue")
  expect_true(validate_catalogue(cat))
  expect_gte(length(metric_ids(cat)), 10)
  expect_false(anyDuplicated(c(metric_ids(cat), component_ids(cat))) > 0)
  tab <- metric_table(cat)
  expect_equal(nrow(tab), length(metric_ids(cat)))
  expect_true(all(is.na(tab$denominator[tab$type == "count"])))
  expect_true(all(!is.na(tab$denominator[tab$type != "count"])))
  expect_true(all(tab$improvement_direction %in% c("increase", "decrease", "neutral")))
  expect_output(print(cat), "metrics")
})

test_that("every component's SQL can be read and mentions event_date", {
  cat <- read_catalogue()
  for (id in component_ids(cat)) {
    sql <- component_sql(cat, id)
    expect_match(sql, "event_date", info = id)
    comp <- get_component(cat, id)
    if (comp$aggregate == "sum") expect_match(sql, "AS value", info = id)
  }
})

test_that("defaults are filled in by get_metric and get_component", {
  cat <- read_catalogue()
  m <- get_metric(cat, "ed_4h_performance")
  expect_equal(m$multiplier, 100)
  expect_equal(m$divisor, 1)
  expect_equal(m$grain, "month")
  expect_equal(metric_components(m), c(numerator = "ed_within_4h", denominator = "ed_attendances"))
  los <- get_metric(cat, "mean_los_days")
  expect_equal(los$multiplier, 1)
  expect_equal(los$divisor, 1440)
  cnt <- get_metric(cat, "ed_attendances_count")
  expect_equal(metric_components(cnt), c(numerator = "ed_attendances"))
  expect_equal(get_component(cat, "ed_attendances")$sql, "ed_attendances.sql")
  expect_error(get_metric(cat, "nope"), "No metric")
  expect_error(get_component(cat, "nope"), "No component")
})

test_that("a valid toy catalogue reads", {
  ev <- count_component("events")
  dir <- write_catalogue(
    metrics = list(toy_metric("n", "count", "events"), toy_metric("pct", "percentage", "events", "events")),
    components = list(ev$component), sql = ev$sql
  )
  cat <- read_catalogue(dir)
  expect_equal(metric_ids(cat), c("n", "pct"))
  expect_equal(component_ids(cat), "events")
})

test_that("validation reports every problem at once", {
  ev <- count_component("events")
  dir <- write_catalogue(
    metrics = list(
      toy_metric("count_with_den", "count", "events", "events"),
      toy_metric("rate_without_den", "rate", "events"),
      toy_metric("bad_refs", "percentage", "ghost", "phantom", grain = "fortnight", improvement_direction = "up", target = "high", multiplier = -1),
      toy_metric("typo", "count", "events", colour = "red"),
      toy_metric("events", "count", "events")
    ),
    components = list(ev$component, list(id = "nosql", title = "x", aggregate = "max", extra = 1)),
    sql = ev$sql
  )
  err <- tryCatch(read_catalogue(dir), error = function(e) conditionMessage(e))
  expect_match(err, "count_with_den': a count has no denominator")
  expect_match(err, "rate_without_den': a rate needs a denominator")
  expect_match(err, "numerator component 'ghost' does not exist")
  expect_match(err, "denominator component 'phantom' does not exist")
  expect_match(err, "grain must be one of")
  expect_match(err, "improvement_direction must be one of")
  expect_match(err, "target must be a single number")
  expect_match(err, "multiplier must be positive")
  expect_match(err, "typo': unknown field\\(s\\) colour")
  expect_match(err, "id used by both a metric and a component: events")
  expect_match(err, "nosql': unknown field\\(s\\) extra")
  expect_match(err, "nosql': aggregate must be one of")
  expect_match(err, "nosql': SQL file not found")
  # the broken catalogue can still be inspected
  expect_silent(cat <- read_catalogue(dir, validate = FALSE))
  expect_equal(length(cat$metrics), 5)
})

test_that("a YAML file must be named after its id", {
  dir <- write_catalogue()
  yaml::write_yaml(list(id = "a", title = "a", aggregate = "count"), file.path(dir, "components", "b.yaml"))
  expect_error(read_catalogue(dir), "must be named after its id")
  unlink(file.path(dir, "components", "b.yaml"))
  yaml::write_yaml(list(title = "a"), file.path(dir, "components", "c.yaml"))
  expect_error(read_catalogue(dir), "No id")
  expect_error(read_catalogue(file.path(dir, "missing")), "does not exist")
})

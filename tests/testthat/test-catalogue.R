test_that("the shipped catalogue is valid and well formed", {
  cat <- read_catalogue()
  expect_s3_class(cat, "fhperf_catalogue")
  expect_true(validate_catalogue(cat))
  expect_gte(length(indicator_ids(cat)), 10)
  expect_equal(domain_codes(cat), c("ed", "apc", "op", "rtt", "diag"))
  ids <- c(indicator_ids(cat), component_ids(cat))
  expect_true(all(grepl("^[a-z]+_[0-9]{3}$", ids)))
  expect_true(all(id_domain(ids) %in% domain_codes(cat)))
  tab <- indicator_table(cat)
  expect_equal(nrow(tab), length(indicator_ids(cat)))
  expect_true(all(is.na(tab$denominator[tab$type == "count"])))
  expect_true(all(!is.na(tab$denominator[tab$type != "count"])))
  expect_true(all(tab$improvement_direction %in% c("increase", "decrease", "neutral")))
  expect_true(all(tab$domain_title != tab$domain))
  # shared ids are exactly the self-numerator counts
  shared <- intersect(indicator_ids(cat), component_ids(cat))
  expect_equal(sort(shared), sort(tab$id[tab$type == "count" & tab$numerator == tab$id]))
  expect_true(length(shared) >= 2)
  ctab <- component_table(cat)
  expect_equal(nrow(ctab), length(component_ids(cat)))
  expect_match(ctab$used_by[ctab$id == "ed_001"], "ed_001 \\(numerator\\)")
  expect_match(ctab$used_by[ctab$id == "ed_001"], "ed_003 \\(denominator\\)")
  expect_match(ctab$used_by[ctab$id == "ed_001"], "diag_002 \\(denominator\\)")
  expect_output(print(cat), "5 domains, 3 dimensions")
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

test_that("defaults are filled in by get_indicator and get_component", {
  cat <- read_catalogue()
  m <- get_indicator(cat, "ed_003")
  expect_equal(m$multiplier, 100)
  expect_equal(m$divisor, 1)
  expect_equal(m$grain, "month")
  expect_equal(indicator_components(m), c(numerator = "ed_002", denominator = "ed_001"))
  los <- get_indicator(cat, "apc_004")
  expect_equal(los$multiplier, 1)
  expect_equal(los$divisor, 1440)
  cnt <- get_indicator(cat, "ed_001")
  expect_equal(indicator_components(cnt), c(numerator = "ed_001"))
  expect_equal(get_component(cat, "ed_001")$sql, "ed_001.sql")
  expect_equal(id_domain(c("ed_001", "diag_002")), c("ed", "diag"))
  expect_error(get_indicator(cat, "nope"), "No indicator")
  expect_error(get_component(cat, "nope"), "No component")
})

test_that("a valid toy catalogue reads", {
  ev <- count_component("t_001")
  dir <- write_catalogue(
    indicators = list(
      list(id = "t_001", title = "self count", type = "count"),
      toy_indicator("t_002", "count", "t_001"),
      toy_indicator("u_001", "percentage", "t_001", "t_001")
    ),
    components = list(ev$component), sql = ev$sql
  )
  cat <- read_catalogue(dir)
  expect_equal(indicator_ids(cat), c("t_001", "t_002", "u_001"))
  expect_equal(component_ids(cat), "t_001")
  expect_equal(get_indicator(cat, "t_001")$numerator, "t_001")
  expect_equal(indicator_table(cat)$numerator, c("t_001", "t_001", "t_001"))
})

test_that("validation reports every problem at once", {
  ev <- count_component("t_001")
  dir <- write_catalogue(
    indicators = list(
      toy_indicator("t_010", "count", "t_001", "t_001"),
      toy_indicator("t_011", "rate", "t_001"),
      toy_indicator("t_012", "percentage", "t_900", "t_901", grain = "fortnight", improvement_direction = "up", target = "high", multiplier = -1),
      toy_indicator("t_013", "count", "t_001", colour = "red"),
      toy_indicator("t_001", "percentage", "t_001", "t_001"),
      list(id = "t_014", title = "orphan count", type = "count"),
      toy_indicator("count_with_words", "count", "t_001"),
      toy_indicator("zz_001", "count", "t_001")
    ),
    components = list(ev$component, list(id = "t_020", title = "x", aggregate = "max", extra = 1)),
    sql = ev$sql
  )
  err <- tryCatch(read_catalogue(dir), error = function(e) conditionMessage(e))
  expect_match(err, "t_010': a count has no denominator")
  expect_match(err, "t_011': a rate needs a denominator")
  expect_match(err, "numerator component 't_900' does not exist")
  expect_match(err, "denominator component 't_901' does not exist")
  expect_match(err, "grain must be one of")
  expect_match(err, "improvement_direction must be one of")
  expect_match(err, "target must be a single number")
  expect_match(err, "multiplier must be positive")
  expect_match(err, "t_013': unknown field\\(s\\) colour")
  expect_match(err, "id used by both an indicator and a component: t_001 \\(only a count")
  expect_match(err, "t_014': a count with no numerator needs a component with the same id")
  expect_match(err, "id 'count_with_words' is not <domain>_<nnn>")
  expect_match(err, "id 'zz_001' has a domain that is not in domains.yaml \\(known: t, u\\)")
  expect_match(err, "t_020': unknown field\\(s\\) extra")
  expect_match(err, "t_020': aggregate must be one of")
  expect_match(err, "t_020': SQL file not found")
  # the broken catalogue can still be inspected
  expect_silent(cat <- read_catalogue(dir, validate = FALSE))
  expect_equal(length(cat$indicators), 8)
})

test_that("domains.yaml is checked", {
  ev <- count_component("t_001")
  dir <- write_catalogue(components = list(ev$component), sql = ev$sql, domains = NULL)
  expect_error(read_catalogue(dir), "not in domains.yaml \\(known: \\)")
  yaml::write_yaml(list(list(code = "t"), list(code = "t")), file.path(dir, "domains.yaml"))
  expect_error(read_catalogue(dir), "Duplicate domain code")
  yaml::write_yaml(list(list(title = "no code")), file.path(dir, "domains.yaml"))
  expect_error(read_catalogue(dir), "needs a code")
})

test_that("a YAML file must be named after its id", {
  dir <- write_catalogue()
  yaml::write_yaml(list(id = "t_001", title = "a", aggregate = "count"), file.path(dir, "components", "b.yaml"))
  expect_error(read_catalogue(dir), "must be named after its id")
  unlink(file.path(dir, "components", "b.yaml"))
  yaml::write_yaml(list(title = "a"), file.path(dir, "components", "c.yaml"))
  expect_error(read_catalogue(dir), "No id")
  expect_error(read_catalogue(file.path(dir, "missing")), "does not exist")
})

test_that("definitions can run ahead of the data", {
  cat <- read_catalogue()
  v2 <- available_indicators(cat, "v2")
  v3 <- available_indicators(cat, "v3")
  expect_true(all(v2 %in% v3))
  pathways <- c("apc_008", "apc_009", "apc_010", "apc_011", "apc_012")
  expect_false(any(pathways %in% v2))
  expect_true(all(pathways %in% v3))
  expect_equal(indicator_since(cat, "apc_008"), "v3")
  expect_true(is.na(indicator_since(cat, "ed_003")))
  expect_equal(component_since(cat, "apc_012"), "v3")
  expect_false("apc_012" %in% available_components(cat, "v2"))
  tab <- indicator_table(cat)
  expect_equal(tab$since[tab$id == "apc_009"], "v3")
  expect_true(is.na(tab$since[tab$id == "ed_001"]))
  expect_equal(component_table(cat)$since[component_table(cat)$id == "apc_010"], "v3")
  expect_error(available_indicators(cat, "latest"), "must look like v2")
  for (id in pathways) expect_match(component_sql(cat, id), "discharge_pathway")
})

test_that("the validator checks since", {
  comp <- count_component("t_001", "SELECT d AS event_date FROM t")
  comp$component$since <- "v3"
  later <- count_component("t_002", "SELECT d AS event_date FROM t")
  dir <- write_catalogue(
    indicators = list(toy_indicator("t_003", "percentage", "t_001", "t_002"), list(id = "t_001", title = "self", type = "count", since = "v3"),
                      toy_indicator("t_004", "percentage", "t_001", "t_002", since = "v4"), toy_indicator("t_005", "count", "t_002", since = "soon")),
    components = list(comp$component, later$component), sql = c(comp$sql, later$sql)
  )
  err <- tryCatch(read_catalogue(dir), error = function(e) conditionMessage(e))
  expect_match(err, "t_003': must declare since: v3 because a component needs it")
  expect_match(err, "t_005': since must be a dataset version such as v3")
  expect_false(grepl("t_004'", err))
  expect_false(grepl("t_001'", err))
  cat <- read_catalogue(dir, validate = FALSE)
  expect_equal(indicator_since(cat, "t_004"), "v4")
  expect_equal(available_indicators(cat, "v3"), c("t_001", "t_003", "t_005"))   # an unvalidated since is ignored
})

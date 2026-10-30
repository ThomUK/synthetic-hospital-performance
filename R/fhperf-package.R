#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||%
NULL

# Field names a component YAML may carry. `sql` defaults to `<id>.sql`.
component_fields <- c("id", "title", "description", "source", "aggregate", "unit", "sql")

# Field names a metric YAML may carry.
metric_fields <- c(
  "id", "title", "description", "type", "numerator", "denominator",
  "multiplier", "divisor", "unit", "grain", "improvement_direction", "target", "rebase"
)

# Every id is a domain code, an underscore and a three-digit number.
id_pattern <- "^([a-z]+)_([0-9]{3})$"

metric_types <- c("count", "percentage", "rate", "mean")
component_aggregates <- c("count", "sum")
grains <- c("month", "week", "day")
improvement_directions <- c("increase", "decrease", "neutral")

# The multiplier a metric type uses when its YAML does not set one.
default_multipliers <- c(count = 1, percentage = 100, rate = 1000, mean = 1)

# Column names passed to NHSRplotthedots::ptd_spc() by bare name in metric_spc().
utils::globalVariables(c("value", "period"))

#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||% .data
NULL

# Field names a component YAML may carry. `sql` defaults to `<id>.sql`.
component_fields <- c("id", "title", "description", "source", "aggregate", "unit", "sql")

# Field names a indicator YAML may carry.
indicator_fields <- c(
  "id", "title", "description", "type", "numerator", "denominator",
  "multiplier", "divisor", "unit", "grain", "improvement_direction", "target", "rebase"
)

# Every id is a domain code, an underscore and a three-digit number.
id_pattern <- "^([a-z]+)_([0-9]{3})$"

indicator_types <- c("count", "percentage", "rate", "mean")
component_aggregates <- c("count", "sum")
grains <- c("month", "week", "day")
improvement_directions <- c("increase", "decrease", "neutral")

# The multiplier a indicator type uses when its YAML does not set one.
default_multipliers <- c(count = 1, percentage = 100, rate = 1000, mean = 1)

# Column names passed to NHSRplotthedots::ptd_spc() by bare name in indicator_spc().
utils::globalVariables(c("value", "period"))

#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||% .data
#' @importFrom utils globalVariables
NULL

# Field names a component YAML may carry. `sql` defaults to `<id>.sql`.
component_fields <- c("id", "title", "description", "source", "aggregate", "unit", "sql", "dimensions", "snapshot")

# Field names an indicator YAML may carry.
indicator_fields <- c(
  "id", "title", "description", "type", "numerator", "denominator",
  "multiplier", "divisor", "unit", "grain", "improvement_direction", "target", "rebase", "breakdowns"
)

# Every id is a domain code, an underscore and a three-digit number.
id_pattern <- "^([a-z]+)_([0-9]{3})$"

indicator_types <- c("count", "percentage", "rate", "mean")
component_aggregates <- c("count", "sum")
grains <- c("month", "week", "day")
improvement_directions <- c("increase", "decrease", "neutral")

# The multiplier an indicator type uses when its YAML does not set one.
default_multipliers <- c(count = 1, percentage = 100, rate = 1000, mean = 1)

# Column names passed to NHSRplotthedots::ptd_spc() by bare name in indicator_spc().
globalVariables(c("value", "period"))

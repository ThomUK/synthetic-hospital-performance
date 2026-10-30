#' Financial years and positions within them
#'
#' `financial_year()` labels each date with its financial year, `"2023/24"`
#' for April 2023 to March 2024. `fy_position()` gives the position of a
#' period within its financial year: month 1 to 12 (April is 1), week 1 to
#' 53, or day 1 to 366.
#'
#' @param dates A `Date` vector.
#' @param start_month The month the year starts in; 4 for April.
#' @param grain `"month"`, `"week"` or `"day"`.
#' @return `financial_year()` a character vector; `fy_position()` an integer
#'   vector.
#' @examples
#' financial_year(as.Date(c("2024-03-31", "2024-04-01")))
#' fy_position(as.Date(c("2024-04-01", "2025-03-01")), "month")
#' @export
financial_year <- function(dates, start_month = 4) {
  dates <- as.Date(dates)
  y <- as.integer(format(dates, "%Y"))
  m <- as.integer(format(dates, "%m"))
  start <- ifelse(m >= start_month, y, y - 1L)
  if (start_month == 1) return(as.character(start))
  paste0(start, "/", sprintf("%02d", (start + 1L) %% 100L))
}

#' @rdname financial_year
#' @export
fy_position <- function(dates, grain = c("month", "week", "day"), start_month = 4) {
  grain <- match.arg(grain)
  dates <- as.Date(dates)
  if (grain == "month") {
    m <- as.integer(format(dates, "%m"))
    return(((m - start_month) %% 12L) + 1L)
  }
  y <- as.integer(format(dates, "%Y"))
  m <- as.integer(format(dates, "%m"))
  fy_start <- as.Date(sprintf("%d-%02d-01", ifelse(m >= start_month, y, y - 1L), start_month))
  days <- as.integer(dates - fy_start)
  if (grain == "week") days %/% 7L + 1L else days + 1L
}

# Fixed categorical hues, oldest year first. Validated for adjacent-pair
# colour-vision separation on a light surface.
season_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300", "#6250d6", "#e34948")

#' Seasonal view of an indicator
#'
#' One line per financial year, April to March on the x axis, so the same
#' month can be compared across years. The latest year is drawn heavier.
#' At most eight years are shown in colour; earlier years are grouped as
#' "Earlier" in grey.
#'
#' @param data An indicator series from [build_indicator()] or
#'   [combine_components()].
#' @param start_month The month the year starts in; 4 for April.
#' @param show_target Draw the target as a dashed line, if the indicator
#'   has one?
#' @return A ggplot.
#' @export
plot_indicator_seasons <- function(data, start_month = 4, show_target = TRUE) {
  ind <- attr(data, "indicator")
  if (is.null(ind)) stop("data has no indicator attribute; build it with build_indicator() or combine_components()", call. = FALSE)
  d <- season_data(data, start_month)
  years <- levels(d$year)
  latest <- years[length(years)]
  cols <- stats::setNames(season_palette[seq_along(years)], years)
  if ("Earlier" %in% years) cols["Earlier"] <- "#9a9a9a"
  ends <- d[!is.na(d$value), ]
  ends <- ends[order(ends$position), ]
  ends <- ends[!duplicated(ends$year, fromLast = TRUE), ]

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$position, y = .data$value, colour = .data$year, group = .data$year)) +
    ggplot2::geom_line(ggplot2::aes(linewidth = .data$year == latest), na.rm = TRUE) +
    ggplot2::geom_point(size = 2, na.rm = TRUE) +
    ggplot2::geom_text(data = ends, ggplot2::aes(label = .data$year), hjust = -0.15, size = 3.2, show.legend = FALSE, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = cols, name = "Financial year") +
    ggplot2::scale_linewidth_manual(values = c(`TRUE` = 1.1, `FALSE` = 0.6), guide = "none") +
    ggplot2::labs(title = ind$title, subtitle = "The same period in each financial year",
                  x = NULL, y = ind$unit %||% "") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom",
                   plot.title.position = "plot")
  p <- p + season_x_scale(ind$grain, start_month)
  if (show_target && !is.null(ind$target)) {
    p <- p + ggplot2::geom_hline(yintercept = ind$target, linetype = "dashed", colour = "#555555")
  }
  p
}

# Add year, position and a text label for each row of an indicator series.
season_data <- function(data, start_month = 4) {
  ind <- attr(data, "indicator")
  d <- as.data.frame(data)
  d$year <- financial_year(d$period, start_month)
  d$position <- fy_position(d$period, ind$grain, start_month)
  years <- sort(unique(d$year))
  if (length(years) > 8) {
    keep <- years[(length(years) - 6):length(years)]
    d$year[!d$year %in% keep] <- "Earlier"
    years <- c("Earlier", keep)
  }
  d$year <- factor(d$year, levels = years)
  d
}

season_x_scale <- function(grain, start_month) {
  if (grain == "month") {
    labels <- month.abb[((start_month - 1 + 0:11) %% 12) + 1]
    return(ggplot2::scale_x_continuous(breaks = 1:12, labels = labels, expand = ggplot2::expansion(mult = c(0.02, 0.08))))
  }
  if (grain == "week") {
    return(ggplot2::scale_x_continuous(breaks = seq(1, 53, by = 4), expand = ggplot2::expansion(mult = c(0.02, 0.08)),
                                       name = "Week of financial year"))
  }
  ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.08)), name = "Day of financial year")
}

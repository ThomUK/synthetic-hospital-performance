#' Open the dataset
#'
#' A thin wrapper around [fakehospdata::fh_connect()]: downloads the chosen
#' scale into the local cache if needed and returns a DuckDB connection with
#' the `layer1`, `reference`, `derived`, `aggregate` and `answers` schemas
#' attached as views. Any `DBI` connection with the same schemas works with
#' the rest of the package, so a pipeline can supply its own.
#'
#' @param scale `"sample"` (5% of patients, about 130 MB) or `"full"` (about
#'   1.1 GB).
#' @param ... Passed to [fakehospdata::fh_connect()].
#' @return A `DBI` connection. Close it with [perf_disconnect()].
#' @export
perf_connect <- function(scale = c("sample", "full"), ...) {
  scale <- match.arg(scale)
  if (!requireNamespace("fakehospdata", quietly = TRUE)) {
    stop("Install fakehospdata to open the dataset: remotes::install_github('ThomUK/synthetic-hospital-data', subdir = 'fakehospdata')", call. = FALSE)
  }
  fakehospdata::fh_connect(scale, ...)
}

#' @rdname perf_connect
#' @param con A connection from [perf_connect()].
#' @export
perf_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

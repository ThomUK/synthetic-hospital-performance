# synthetic-hospital-performance

SPC charts for the [Fake NHS Trust synthetic hospital dataset](https://github.com/ThomUK/synthetic-hospital-data):
an R package, `fhperf`, holding a catalogue of metrics, and a Quarto site
that renders every metric as an [NHSRplotthedots](https://nhs-r-community.github.io/NHSRplotthedots/)
chart. Nothing in the data is real.

## The idea

- A **component** is a SQL query that returns one row per event, plus a YAML
  file saying whether the events are counted or their `value` summed. The
  components are the numerators and denominators, and each has its own id
  because denominators are reused across metrics.
- A **metric** is a YAML file naming a numerator, a denominator unless it is
  a count, a type (`count`, `percentage`, `rate`, `mean`), the period grain,
  the improvement direction and optionally a target and rebase dates.
- The package reads the events, aggregates them into complete periods,
  combines numerator and denominator, and hands the series to
  `NHSRplotthedots::ptd_spc()`.

```
inst/
  metrics/      one YAML per metric          ed_4h_performance.yaml
  components/   one YAML per component       ed_within_4h.yaml, ed_attendances.yaml
  sql/          one query per component      ed_within_4h.sql, ed_attendances.sql
```

The shipped catalogue has ten metrics: ED attendances, ED four-hour
performance, mean time in ED for admitted and for non-admitted patients,
emergency admissions, mean length of stay in bed-minutes (shown as days),
emergency readmissions within 30 days, outpatient DNA rate, RTT incomplete
pathways within 18 weeks (a month-end snapshot) and imaging requests per
1,000 ED attendances.

## Use

```r
remotes::install_github("ThomUK/synthetic-hospital-data", subdir = "fakehospdata")
remotes::install_github("ThomUK/synthetic-hospital-performance")
library(fhperf)

cat <- read_catalogue()
con <- perf_connect("sample")          # downloads ~130 MB once, opens DuckDB
x <- build_metric(con, cat, "ed_4h_performance")
plot_metric(x)
metric_summary(x)
all <- build_metrics(con, cat)         # every metric; shared components read once
perf_disconnect(con)
```

Any DBI connection with the dataset's schemas (`derived`, `layer1`, ...)
works in place of `perf_connect()`, so a pipeline can bring its own.

### Adding a metric

1. Write the component SQL in `inst/sql/<id>.sql`, returning `event_date`
   and, for a sum, `value`. Reuse an existing denominator if one fits.
2. Describe it in `inst/components/<id>.yaml`.
3. Add `inst/metrics/<id>.yaml`.
4. `read_catalogue()` validates the lot and names every problem; the tests
   run every query against the sample.

## The site

`site/` is a Quarto website. Its pre-render script `_build.R` builds every
metric, saves the results to `site/_data/` and writes one page per metric
from `site/_templates/metric.qmd`; the pages only read the saved data.

```sh
cd site && quarto render        # needs fhperf installed and the sample cached
```

The `pages` workflow renders and deploys it to GitHub Pages on every push
to `main`. The repository's Pages source must be set to GitHub Actions.

## Development

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
```

The integration tests need `duckdb` and the sample: set `FHPERF_SAMPLE_DIR`
to a sample directory (a checkout of `synthetic-hospital-data` at
`data/v2/sample`), keep that repository checked out next to this one, or let
`fakehospdata` download it. Otherwise they skip.

## Later

The build is deliberately split so a `{targets}` pipeline can replace
`_build.R` with one target per component and per metric, cached between
renders. The aggregation is one function, `aggregate_events()`, so it can
move into SQL for the full set without touching anything else.

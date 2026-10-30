# synthetic-hospital-performance

SPC charts for the [Fake NHS Trust synthetic hospital dataset](https://github.com/ThomUK/synthetic-hospital-data):
an R package, `fhperf`, holding a catalogue of indicators, and a Quarto site
that renders every indicator as an [NHSRplotthedots](https://nhs-r-community.github.io/NHSRplotthedots/)
chart. Nothing in the data is real.

## The idea

- A **component** is a SQL query that returns one row per event, plus a YAML
  file saying whether the events are counted or their `value` summed. The
  components are the numerators and denominators, and each has its own id
  because denominators are reused across indicators.
- A **indicator** is a YAML file naming a numerator, a denominator unless it is
  a count, a type (`count`, `percentage`, `rate`, `mean`), the period grain,
  the improvement direction and optionally a target and rebase dates.
- A component may declare **dimensions** (`site`, `specialty`, ...), columns
  its SQL returns with each event; `inst/dimensions.yaml` lists them with a
  labels query. An indicator can be broken down by any dimension all of its
  components carry. `build_breakdowns()` computes every group, folds small
  ones into "Other", and `breakdown_summary()` gives the plot-the-dots
  summary table; the groups always add up to the whole.
- Every id is `<domain>_<nnn>`. The domain (`ed`, `apc`, `op`, `rtt`,
  `diag`, listed in `inst/domains.yaml`) names the subject matter expert
  group that owns the definition, and indicators and components share one
  numbering within it. A count is one id, because a count indicator's numerator
  is the component with the same id. A rate is three.
- The package reads the events, aggregates them into complete periods,
  combines numerator and denominator, and hands the series to
  `NHSRplotthedots::ptd_spc()`.

```
inst/
  domains.yaml     the domain codes and their owners
  dimensions.yaml  the dimensions components may carry
  indicators/      one YAML per indicator          ed_003.yaml   (four-hour performance = ed_002 / ed_001)
  components/   one YAML per component       ed_002.yaml, ed_001.yaml
  sql/          one query per component      ed_002.sql,  ed_001.sql
```

The shipped catalogue has ten indicators: ED attendances, ED four-hour
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
indicator_table(cat)                      # every indicator with its numerator and denominator ids
x <- build_indicator(con, cat, "ed_003")  # ED four-hour performance
plot_indicator(x)
indicator_summary(x)
all <- build_indicators(con, cat)         # every indicator; shared components read once
by <- build_breakdowns(con, cat, "apc_004", min_per_period = 5)   # by site and specialty
breakdown_summary(by$specialty)           # one row per specialty with icons
plot_indicator(group_series(by$site, "RZ902"))
perf_disconnect(con)
```

Any DBI connection with the dataset's schemas (`derived`, `layer1`, ...)
works in place of `perf_connect()`, so a pipeline can bring its own.

### Adding an indicator

1. Pick the owning domain and take its next free number for each new id.
   Reuse an existing denominator if one fits; `component_table()` shows
   what each component is already used by.
2. Write the component SQL in `inst/sql/<id>.sql`, returning `event_date`
   and, for a sum, `value`, and describe it in `inst/components/<id>.yaml`.
3. Add `inst/indicators/<id>.yaml`. For a count, give the indicator the same id
   as its component and leave `numerator` out.
4. `read_catalogue()` validates the lot and names every problem; the tests
   run every query against the sample.

## The site

`site/` is a Quarto website: a home page of latest values and charts, an
index of every indicator and component with their ids, a definitions page
showing every YAML and SQL file as written, a how-it-works page and one
page per indicator. Its pre-render script `_build.R` builds every indicator,
saves the results to `site/_data/` and writes the indicator pages from
`site/_templates/indicator.qmd`; the pages only read the saved data.

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
`_build.R` with one target per component and per indicator, cached between
renders. The aggregation is one function, `aggregate_events()`, so it can
move into SQL for the full set without touching anything else.

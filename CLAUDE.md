# synthetic-hospital-performance

An R package, `fhperf`, with a catalogue of performance indicators for the
Fake NHS Trust synthetic dataset, and a Quarto site in `site/` that renders
each indicator as an NHSRplotthedots SPC chart. Deployed to GitHub Pages from
`main` by `.github/workflows/pages.yml`.

## Working rules

- **Merge finished work to `main` and push it.** There is no review gate and
  Pages deploys from `main`. Work on the session branch if one is assigned,
  then fast-forward it into `main` and push `main`. Do not open a pull
  request unless asked.
- **Commit as Claude.** Author and committer are `Claude <noreply@anthropic.com>`.
- **One YAML per indicator and per component; ids are `<domain>_<nnn>`.** The
  domain codes live in `inst/domains.yaml`, one per subject matter expert
  group; a new id takes the owning domain and its next free number, and
  indicators and components share the numbering. An indicator names its
  `numerator` and (unless a count) `denominator` component; a count indicator
  shares its id with its component and omits `numerator`. A component's SQL
  returns one row per event with `event_date` and, for `aggregate: sum`,
  `value`. `read_catalogue()` validates everything; keep the validator in
  step with any new field.
- **Dimensions live in `inst/dimensions.yaml`.** A component lists the
  ones its SQL returns under `dimensions:`; an indicator's breakdowns are the
  dimensions all its components share. Breakdowns are views of an indicator,
  never new ids, and their groups must add up to the whole (the integration
  test checks this). Site and specialty of a ratio's numerator must be taken
  from the same record as the denominator's.
- **Reuse denominators.** Before adding a component, check whether one in
  `inst/components/` already defines the population.
- **Keep the database out of the pages.** `site/_build.R` is the only
  place a render opens the data; pages read `site/_data/indicators.rds`. This
  is the seam a `{targets}` pipeline will replace.
- Tests: `Rscript -e 'devtools::test()'`. The integration tests need
  `duckdb` and the sample (see README) and skip without them.
- Regenerate docs after changing roxygen comments: `Rscript -e 'devtools::document()'`.

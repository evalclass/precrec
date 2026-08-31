# precrec development

## Commands

```r
devtools::load_all(".")          # compile C++ + load; the main dev loop
devtools::document()             # roxygen -> NAMESPACE + man/*.Rd (also Rcpp stubs)
devtools::test()                 # all testthat tests
testthat::test_file("tests/testthat/test_pl5_1_create_roc.R")   # one file
devtools::check()                # full R CMD check, what CI runs
devtools::spell_check()          # en-US; whitelist lives in inst/WORDLIST
styler::style_pkg(exclude_files = "R/RcppExports.R")   # tidyverse style
lintr::lint_package()            # config in .lintr; expects 0 lints
pkgdown::build_site(run_dont_run = TRUE)   # local docs preview
```

`lintr::lint_package()` needs precrec *installed* (it has compiled code), or
`object_usage_linter` reports every package function as undefined. styler owns
formatting, so `indentation_linter` is switched off in `.lintr` where the two
disagree.

Regenerate `README.md` from `README.Rmd` with
`rmarkdown::render("README.Rmd")` — never edit `README.md` directly.

After changing anything in `src/`, run `devtools::document()` too: it
refreshes `src/RcppExports.cpp` and `R/RcppExports.R` (it runs
`Rcpp::compileAttributes()`; call that directly if you skip roxygen).

`src/*.o` and `src/precrec.so` are build output. They are gitignored; if they
show up in `git status`, something is misconfigured — don't commit them.
`src/Makevars` is deliberately absent (removed in 0.10.1) so the shared
object stays unstripped.

## Tests

`tests/testthat/`, one `test_*.R` per source file, named after the source
file it covers. Names sort into the pipeline order (`test_mm*`, `test_pl*`,
`test_g_*`), plus `test_uc1/uc2_usecases.R` for end-to-end scenarios.

### ggplot snapshots

Plot tests go through `check_ggplot_fig(title, plot)` in
`tests/testthat/setup.R`, which:

- on CI, only asserts the object is a `ggplot` (rendering differs across
  platforms and ggplot2 versions);
- locally, runs `vdiffr::expect_doppelganger()` against
  `tests/testthat/_snaps/`.

**The `.svg` snapshot files are gitignored on purpose** (`_snaps/*/*.svg`).
So a local vdiffr "failure" on a clean checkout usually means *no baseline
exists yet*, not a regression. Generate baselines locally, compare, and don't
try to commit them.

`tests/testthat/Rplots.pdf` is base-graphics test output, also gitignored.

**A changed snapshot is not a diagnosis.** vdiffr compares the SVG as text,
byte for byte, and svglite bakes the measured text widths of the local "sans"
font into every `<text>` element — so a baseline belongs to the machine that
made it, and any change to the metric list, the panel layout, ggplot2, or
svglite invalidates the lot at once. Accepting the new baseline is usually
right, but read the rendered diff first: a real bug hides in the same "the
snapshot changed" as a font difference. Accepting without looking is how the
`autoplot()` panel-title bug got into the 0.15.0 development line.

Because the baselines cannot travel, the machine-independent assertions in
`test_etc_utils_autoplot.R` are what actually guards the plot code — panel
count, which measure each panel draws, titles and axis labels, read off the
ggplot object without rendering. They run on CI, where vdiffr does not. Add
to them when you change what a plot contains; `gg_panels()` and `gg_labs()`
in `setup.R` are the helpers.

### CRAN-sensitive tests

Some tests are skipped on CRAN (`skip_on_cran()`) to keep check time and
platform variance down. Keep new slow or graphics-heavy tests in that
pattern.

## Branching (git-flow)

- `main` — released code, tagged `v<version>`.
- `develop` — integration branch; **default working branch**.
- `feature/<Name>` — branched from and merged back to `develop`
  (CamelCase names are the existing convention, e.g. `feature/ReformatTest`).
- `release/<version>` — merged into both `main` and `develop`.

Merges are kept as real merge commits (`--no-ff`), which is what makes the
history graph readable. CI (`.github/workflows/`) runs R-CMD-check,
test-coverage, and pkgdown on `main` only — the `_develop` branch filter is
**deliberate**: CI on `develop` is disabled because the full matrix is too
heavy to run on every push. Don't "fix" it; run `devtools::check()` locally
before merging to `develop` instead.

## Release checklist

1. `feature/*` work merged into `develop`.
2. Bump `Version:` and `Date:` in `DESCRIPTION`.
3. Add a `# precrec <version>` section to `NEWS.md` with a bullet per
   user-visible change.
4. Rewrite `cran-comments.md`: what changed, the version bump, test
   environments actually used, and R CMD check results. The known-and-accepted
   NOTE is the >1Mb `libs` sub-directory (`precrec.so` is ~4 MB).
5. `devtools::document()`, `devtools::check()`, `devtools::spell_check()`.
6. `devtools::check_win_devel()` / `check_win_release()`, and
   `devtools::revdep_check()` for reverse dependencies.
7. `release/<version>` branch, merge to `main` + `develop`, tag `v<version>`.
8. `devtools::release()` for the CRAN submission.

## Repo layout notes

- `data/` — the shipped datasets (`P10N10`, `B500`, `B1000`, `IB500`,
  `IB1000`, `M2N50F5`), regenerated by the scripts in `data-raw/`.
  Documented in `R/precrec.R`.
- `inst/CITATION` — the Bioinformatics 2017 citation.
- `scripts/html_update/` — helpers for the external classeval website, not
  part of the package.
- `_pkgdown.yml` — site config; **new exported functions must be added to a
  `reference:` section there or `pkgdown` fails on undocumented topics.**
- `.Rbuildignore` — anything added at the top level that isn't part of the
  package (including `CLAUDE.md` and `.claude/`) needs an entry here.

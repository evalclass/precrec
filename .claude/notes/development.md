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
`object_usage_linter` reports every package function as undefined - hundreds
of them, all false. `devtools::install()` first, then lint; the package is
expected to report **zero** lints. styler owns
formatting, so `indentation_linter` is switched off in `.lintr` where the two
disagree.

Regenerate `README.md` from `README.Rmd` with
`rmarkdown::render("README.Rmd")` — never edit `README.md` directly.

### The website

`vignettes/introduction.Rmd` is the only shipped vignette — the short
`Get started` page. Everything else lives in `vignettes/articles/`, which
`pkgdown` builds for the site and `R CMD build` ignores (the directory is in
`.Rbuildignore`), so the tarball and the check time do not grow with the
documentation.

Adding a page means three edits: the `.Rmd` in `vignettes/articles/`, an
entry under the right `articles:` section of `_pkgdown.yml`, and an entry in
the matching `navbar:` menu. **Article entries in `articles:` are quoted and
carry the directory — `"articles/howto-prepare-data"`.** Unquoted, `pkgdown`
parses the hyphens as arithmetic and fails with "failed to evaluate"; without
the prefix, it does not find the file.

`pkgdown::build_site()` runs every page and takes several minutes. To find a
broken chunk faster, `knitr::knit()` each `.Rmd` in a subprocess first — same
errors, no HTML. It leaves a `figure/` directory behind in
`vignettes/articles/`; delete it.

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
`tests/testthat/setup.R`. It records what the plot *is* - its panels, their
titles and axis labels, and the x/y/group data behind every layer - as text,
and compares that against a named baseline in `tests/testthat/_snaps/`.

**Those baselines are committed**, because the digest is identical on every
machine. Rendering is not involved, so nothing depends on the local fonts or
on the ggplot2 layout engine, and CI runs the same comparison everyone else
does.

When a snapshot fails, read the diff - it is plain text and names what moved:

```r
testthat::snapshot_review("g_autoplot3_points/")  # or diff the .new.txt
```

Accept with `testthat::snapshot_accept()` **once you have confirmed the
change is the one you meant to make**, and commit the updated baseline with
the change that caused it.

This replaced vdiffr in 0.15.0. vdiffr compared the rendered SVG byte for
byte, and svglite bakes the measured width of the local "sans" font into
every text element, so a baseline belonged to the machine that made it. The
files could not be committed, every developer had to regenerate them, the
comparison had to be skipped on CI, and a stale baseline was reported in the
same words as a real regression - which is how the `autoplot()` panel-title
bug got accepted into a baseline during 0.15.0 development. Deliberate visual
inspection is still just `plot(x)` at the console.

`tests/testthat/Rplots.pdf` is base-graphics test output, and is gitignored.

Machine-independent assertions about plot structure also live in
`test_etc_utils_autoplot.R` - panel counts, which measure each panel draws,
titles and axis labels. Prefer those for anything you can state directly;
the snapshots are for catching what you did not think to assert.

### Benchmarks and the ROCR parity script

`bench/` holds development scripts, is `.Rbuildignore`d, and has its own
README. Two things about it that have bitten:

- **`bench/run_bench.R --compare` is a screen, not a verdict.** On a machine
  that has been running `R CMD check` and benchmark sweeps back to back, two
  runs of identical code against the same baseline can disagree by 2x. Check
  what unchanged code says against the same baseline before believing a
  number, and never compare a `--quick` run against a full baseline.
- **Regenerate `bench/baseline/develop.json` whenever a change alters what
  the package computes.** A stale baseline reports the documented cost of a
  new measure as a regression, which looks exactly like a real one.

`bench/run_rocr_parity.R` cross-checks the basic evaluation measures against
ROCR, which is the reference implementation for the ones added in 0.16.0.
**ROCR is deliberately not a dependency, not even in `Suggests`** - install
it yourself and the script runs, or it exits quietly. Where the two packages
disagree on purpose - `odds` and `chisq` are NA rather than infinite at the
ends, `mi` is 0 rather than NaN there - the script asserts that the
difference is *only* there.

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
2. Bump `Version:` in `DESCRIPTION` (there is no `Date:` field).
3. Add a `# precrec <version>` section to `NEWS.md` with a bullet per
   user-visible change.
4. Confirm `cran-comments.md`. It is kept current as work lands rather than
   written from scratch at release, so it should already summarize the version
   in a handful of grouped bullets — keep it that way and let `NEWS.md` carry
   the detail. What still has to be checked at submission time is that the
   test environments listed are the ones actually used and that the R CMD
   check results match this run. The known-and-accepted NOTE is the >1Mb
   `libs` sub-directory (`precrec.so` is ~4 MB).
5. `devtools::document()`, `devtools::check()`, `devtools::spell_check()`,
   `devtools::install()` then `lintr::lint_package()`, and
   `rmarkdown::render("README.Rmd")` if anything user-facing changed.
6. `devtools::check_win_devel()` / `check_win_release()`, and
   `devtools::revdep_check()` for reverse dependencies.
7. `release/<version>` branch, merge to `main` + `develop`, tag `v<version>`.
8. `devtools::release()` for the CRAN submission.

## Repo layout notes

- `data/` — the shipped datasets (`P10N10`, `B500`, `B1000`, `IB500`,
  `IB1000`, `M2N50F5`, `C3N150`), regenerated by the scripts in `data-raw/`.
  Documented in `R/precrec.R`.
- `inst/CITATION` — the Bioinformatics 2017 citation.
- `scripts/html_update/` — helpers for the external classeval website, not
  part of the package.
- `_pkgdown.yml` — site config; **new exported functions must be added to a
  `reference:` section there or `pkgdown` fails on undocumented topics.**
- `.Rbuildignore` — anything added at the top level that isn't part of the
  package (including `CLAUDE.md` and `.claude/`) needs an entry here.

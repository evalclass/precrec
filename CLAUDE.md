# precrec

R package (CRAN) for accurate ROC and precision-recall curve calculation and
plotting for binary classifiers. Hot paths are C++ via Rcpp.

- Version/deps: `DESCRIPTION` · Exports: `NAMESPACE` (roxygen2-generated)
- Public API: `evalmod`, `mmdata`, `join_scores`, `join_labels`,
  `create_sim_samples`, `format_nfold` + S3 generics `print`, `as.data.frame`,
  `plot`, `autoplot`, `fortify`, `auc`, `part`, `pauc`, `auc_ci`

## Extra docs

- [Architecture](.claude/notes/architecture.md) — pipeline stages, S3 class
  naming scheme, file-prefix convention, R↔C++ boundary. **Read before
  touching anything in `R/pl*`, `R/mm*`, or `src/`.**
- [Development](.claude/notes/development.md) — build/test/check commands,
  git-flow branching, release + CRAN checklist, vdiffr snapshot handling.

## Hard rules

- **Never hand-edit `NAMESPACE`, `man/*.Rd`, or `R/RcppExports.R` /
  `src/RcppExports.cpp`.** They are generated. Edit the roxygen comments above
  the function, or the `// [[Rcpp::export]]` C++ function, then regenerate
  (see Development doc).
- **Every user-visible change needs a `NEWS.md` bullet** under a version
  heading, and the CRAN-facing summary in `cran-comments.md` at release time.
- Keep `Language: en-US` spelling in docs and messages.
- Errors use `stop(msg, call. = FALSE)`; argument checks live in
  `R/etc_utils_validate_args.R`, object checks in the `.validate.<class>`
  S3 methods next to the code that builds the object.
- Don't add hard dependencies. `Imports` is deliberately small; anything
  optional (`patchwork`, `vdiffr`, `data.table` at some call sites) is loaded
  through a `requireNamespace` helper in `R/etc_utils*.R` and belongs in
  `Suggests`.
- `src/*.o` and `src/precrec.so` are local build artifacts — never commit them.

## Conventions

- Exported functions and their args use `snake_case`; internal helpers are
  prefixed with a dot (`.calc_pauc`). Dot-prefixed = not exported, no `.Rd`.
- Source files are grouped by prefix — see the Architecture doc; put new code
  in the file matching its pipeline stage rather than starting a new file.
- Tests mirror source files: `R/pl5_create_curves.R` →
  `tests/testthat/test_pl5_*.R`. Add tests in the mirroring file.
- Indent 2 spaces, ~80 col lines, `<-` for assignment (matches the existing
  `styler` tidyverse formatting throughout).

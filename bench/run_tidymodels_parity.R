#!/usr/bin/env Rscript
#
# Cross-check the tidymodels how-to page against a real tidymodels fit.
#
#   Rscript bench/run_tidymodels_parity.R
#
# vignettes/articles/howto-tidymodels.Rmd shows the calls that need
# tidymodels without evaluating them, so that the website builds on a
# machine that does not have it. tidymodels is deliberately NOT a dependency
# of this package, not even in Suggests - it pulls in a large tree, and
# nothing in `precrec` uses it. Install it yourself to run this, and skip the
# script when it is absent.
#
#   install.packages(c("tidymodels", "workflowsets", "modeldata"))
#
# What this asserts is the page, not the package: that the column names it
# names are the ones tidymodels produces, that its recipes run, and that the
# numbers agree with yardstick where both packages compute the same thing.
# Two of the page's recipes were wrong until this script ran them:
#
#   1. collect_predictions() on a workflow_set averages over the folds
#      unless `summarize = FALSE`, and the summarized frame has no `id`
#      column - the pivot the page showed could not widen on it.
#   2. `.pred_class` also starts with ".pred_" and is a factor, so selecting
#      the score columns by pattern coerced the matrix to character. The
#      page now selects them by level.
#
# Exits non-zero if any check fails.

.bench_dir <- "bench"
source(file.path(.bench_dir, "harness.R"))

.needed <- c("tidymodels", "workflowsets", "modeldata", "tidyr")
.missing <- .needed[!vapply(.needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(.missing) > 0L) {
  cat("not installed:", paste(.missing, collapse = ", "), "\n")
  cat("  install.packages(c(\"tidymodels\", \"workflowsets\", \"modeldata\"))\n")
  quit(status = 0)
}

suppressMessages({
  library(tidymodels)
  library(workflowsets)
})
bench_load_precrec()

.checks <- new.env(parent = emptyenv())
.checks$pass <- 0L
.checks$fail <- character()

check <- function(label, ok) {
  ok <- isTRUE(ok)
  cat(if (ok) "  ok   " else "  FAIL ", label, "\n", sep = "")
  if (ok) {
    .checks$pass <- .checks$pass + 1L
  } else {
    .checks$fail <- c(.checks$fail, label)
  }
  invisible(ok)
}

roc_of <- function(mdat) {
  subset(auc(evalmod(mdat)), curvetypes == "ROC")$aucs
}

data(two_class_dat, package = "modeldata")
set.seed(1)
folds <- vfold_cv(two_class_dat, v = 4)
wf <- workflow() |>
  add_formula(Class ~ .) |>
  add_model(logistic_reg())
preds <- fit_resamples(
  wf, folds,
  control = control_resamples(save_pred = TRUE)
) |>
  collect_predictions()

cat("\ncollect_predictions(), one model\n")
check(
  "carries the columns the page names",
  all(c("id", ".row", ".pred_class", ".pred_Class1", ".pred_Class2") %in%
    names(preds))
)

cat("\nevent level\n")
yd_first <- roc_auc(preds, truth = Class, .pred_Class1)$.estimate
yd_second <- roc_auc(
  preds,
  truth = Class, .pred_Class2, event_level = "second"
)$.estimate
pr_default <- roc_of(mmdata(preds$.pred_Class2, preds$Class))
pr_first <- roc_of(
  mmdata(preds$.pred_Class1, preds$Class, posclass = "Class1")
)
pr_trap <- roc_of(mmdata(preds$.pred_Class1, preds$Class))

check(
  "yardstick event=first equals precrec posclass=first level",
  isTRUE(all.equal(yd_first, pr_first))
)
check(
  "yardstick event=second equals the precrec default",
  isTRUE(all.equal(yd_second, pr_default))
)
check(
  "the mismatched pairing is the complement, not an error",
  isTRUE(all.equal(pr_trap, 1 - pr_default))
)

cat("\none model over folds\n")
curves <- evalmod(
  nfold_df = as.data.frame(preds), score_cols = ".pred_Class2",
  lab_col = "Class", fold_col = "id",
  modnames = "glm", dsids = seq_along(folds$id)
)
per_fold <- subset(auc(curves), curvetypes == "ROC" & modnames == "glm")
check("nfold_df takes the frame as it comes", nrow(per_fold) == nrow(folds))

cat("\nseveral models\n")
wset <- workflow_set(
  preproc = list(plain = Class ~ .),
  models = list(glm = logistic_reg(), tree = decision_tree(mode = "classification"))
)
wres <- workflow_map(
  wset, "fit_resamples",
  resamples = folds, verbose = FALSE,
  control = control_resamples(save_pred = TRUE)
)

check(
  "the summarized frame has no fold column, as the page warns",
  !("id" %in% names(collect_predictions(wres)))
)

wide <- wres |>
  collect_predictions(summarize = FALSE) |>
  tidyr::pivot_wider(
    id_cols = c(id, .row, Class),
    names_from = wflow_id, values_from = .pred_Class2
  )
check(
  "summarize = FALSE widens on the folds",
  all(c("id", ".row", "Class", "plain_glm", "plain_tree") %in% names(wide))
)

cv <- evalmod(
  nfold_df = as.data.frame(wide),
  score_cols = c("plain_glm", "plain_tree"),
  lab_col = "Class", fold_col = "id",
  modnames = c("plain_glm", "plain_tree"), dsids = seq_along(folds$id)
)
glm_wset <- subset(
  auc(cv), curvetypes == "ROC" & modnames == "plain_glm"
)$aucs
check(
  "the widened frame reproduces the single-model AUCs",
  isTRUE(all.equal(glm_wset, per_fold$aucs))
)

cat("\nmore than two classes\n")
pg <- na.omit(modeldata::penguins)
set.seed(1)
mfit <- workflow() |>
  add_formula(species ~ .) |>
  add_model(set_engine(multinom_reg(penalty = 0.01), "glmnet")) |>
  fit(pg)
mp <- augment(mfit, pg)

loose <- as.matrix(mp[grep("^\\.pred_", names(mp))])
check(
  "selecting the score columns by pattern is not safe",
  is.character(loose)
)

sc <- as.matrix(mp[paste0(".pred_", levels(mp$species))])
colnames(sc) <- levels(mp$species)
ovr <- auc(evalmod(mmdata(sc, mp$species)))
check("selecting them by level is", is.numeric(sc))

pr_macro <- subset(
  ovr, curvetypes == "ROC" & modnames == "macro-average"
)$aucs
yd_macro <- roc_auc(
  mp,
  truth = species, .pred_Adelie, .pred_Chinstrap, .pred_Gentoo,
  estimator = "macro"
)$.estimate
check(
  "the macro-average row equals yardstick's macro estimator",
  isTRUE(all.equal(pr_macro, yd_macro))
)

cat("\nthe report\n")
bfit <- workflow() |>
  add_formula(Class ~ .) |>
  add_model(logistic_reg()) |>
  fit(two_class_dat)
bp <- augment(bfit, two_class_dat)
check(
  ".pred_class is the 0.5 cut the report takes",
  all((bp$.pred_Class2 >= 0.5) == (bp$.pred_class == "Class2"))
)

rep <- classification_report(mmdata(bp$.pred_Class2, bp$Class), at = 0.5)
pos <- rep[rep$class == "positive", ]
same <- function(a, b) isTRUE(all.equal(a, b, tolerance = 1e-6))
check(
  "report precision equals yardstick precision",
  same(pos$precision, precision(bp, Class, .pred_class,
    event_level = "second"
  )$.estimate)
)
check(
  "report recall equals yardstick recall",
  same(pos$recall, recall(bp, Class, .pred_class,
    event_level = "second"
  )$.estimate)
)
check(
  "report f1-score equals yardstick f_meas",
  same(pos$fscore, f_meas(bp, Class, .pred_class,
    event_level = "second"
  )$.estimate)
)

cat("\n")
if (length(.checks$fail) > 0L) {
  cat(.checks$pass, "checks passed,", length(.checks$fail), "failed:\n")
  cat(paste0("  ", .checks$fail, collapse = "\n"), "\n")
  quit(status = 1)
}
cat(.checks$pass, "checks passed, none failed.\n")

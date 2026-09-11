# Package index

## Package

- [`precrec-package`](https://evalclass.github.io/precrec/reference/precrec.md)
  [`precrec`](https://evalclass.github.io/precrec/reference/precrec.md)
  : precrec: A package for computing accurate ROC and Precision-Recall
  curves

## Main

- [`evalmod()`](https://evalclass.github.io/precrec/reference/evalmod.md)
  : Evaluate models and calculate performance evaluation metrics
- [`metric_curve()`](https://evalclass.github.io/precrec/reference/metric_curve.md)
  : Draw one evaluation metric against another

## Data preparation

- [`mmdata()`](https://evalclass.github.io/precrec/reference/mmdata.md)
  : Reformat input data for performance evaluation calculation
- [`join_scores()`](https://evalclass.github.io/precrec/reference/join_scores.md)
  : Join scores of multiple models into a list
- [`join_labels()`](https://evalclass.github.io/precrec/reference/join_labels.md)
  : Join observed labels of multiple test datasets into a list
- [`format_nfold()`](https://evalclass.github.io/precrec/reference/format_nfold.md)
  : Create n-fold cross validation dataset from data frame
- [`create_sim_samples()`](https://evalclass.github.io/precrec/reference/create_sim_samples.md)
  : Create random samples for simulations

## Visualization

- [`plot(`*`<sscurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<mscurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<smcurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<mmcurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<sspoints>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<mspoints>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<smpoints>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<mmpoints>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<ssxycurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<msxycurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<smxycurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  [`plot(`*`<mmxycurves>`*`)`](https://evalclass.github.io/precrec/reference/plot.md)
  : Plot performance evaluation metrics
- [`autoplot(`*`<sscurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<mscurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<smcurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<mmcurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<sspoints>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<mspoints>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<smpoints>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<mmpoints>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<ssxycurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<msxycurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<smxycurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  [`autoplot(`*`<mmxycurves>`*`)`](https://evalclass.github.io/precrec/reference/autoplot.md)
  : Plot performance evaluation metrics with ggplot2
- [`fortify(`*`<sscurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<mscurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<smcurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<mmcurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<sspoints>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<mspoints>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<smpoints>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<mmpoints>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<ssxycurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<msxycurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<smxycurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  [`fortify(`*`<mmxycurves>`*`)`](https://evalclass.github.io/precrec/reference/fortify.md)
  : Convert a curves and points object to a data frame for ggplot2

## Data retrieval

- [`print(`*`<mdat>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  [`print(`*`<curve_info>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  [`print(`*`<beval_info>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  [`print(`*`<aucroc>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  [`print(`*`<xycurve_info>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  [`print(`*`<classification_report>`*`)`](https://evalclass.github.io/precrec/reference/print.md)
  : Print the summary of a precrec object
- [`as.data.frame(`*`<sscurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<mscurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<smcurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<mmcurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<sspoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<mspoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<smpoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<mmpoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<aucroc>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<ssxycurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<msxycurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<smxycurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  [`as.data.frame(`*`<mmxycurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.frame.md)
  : Convert a curves and points object to a data frame
- [`as.data.table(`*`<sscurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<mscurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<smcurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<mmcurves>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<sspoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<mspoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<smpoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<mmpoints>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  [`as.data.table(`*`<aucroc>`*`)`](https://evalclass.github.io/precrec/reference/as.data.table.md)
  : Convert a curves and points object to a data.table
- [`metric_table()`](https://evalclass.github.io/precrec/reference/metric_table.md)
  : Get every evaluation metric at every cutoff
- [`best_cutoff()`](https://evalclass.github.io/precrec/reference/best_cutoff.md)
  : Choose an operating point
- [`auc()`](https://evalclass.github.io/precrec/reference/auc.md) :
  Retrieve a data frame of AUC scores
- [`pauc()`](https://evalclass.github.io/precrec/reference/pauc.md) :
  Retrieve a data frame of pAUC scores

## Partial AUC and partial curve

- [`part()`](https://evalclass.github.io/precrec/reference/part.md) :
  Calculate partial AUCs

## Confidence interval of AUC scores

- [`auc_ci()`](https://evalclass.github.io/precrec/reference/auc_ci.md)
  : Calculate CIs of ROC and precision-recall AUCs
- [`auc_boot()`](https://evalclass.github.io/precrec/reference/auc_boot.md)
  : Bootstrap AUCs from one test set
- [`auc_delong()`](https://evalclass.github.io/precrec/reference/auc_delong.md)
  : DeLong's standard error for the ROC AUC
- [`auc_diff()`](https://evalclass.github.io/precrec/reference/auc_diff.md)
  : Compare AUCs between models

## Precision-recall break-even point

- [`prbe()`](https://evalclass.github.io/precrec/reference/prbe.md) :
  Calculate the precision-recall break-even point

## Average precision

- [`average_precision()`](https://evalclass.github.io/precrec/reference/average_precision.md)
  : Calculate the average precision

## Probability-based metrics

- [`prob_metrics()`](https://evalclass.github.io/precrec/reference/prob_metrics.md)
  : Calculate the Brier score, the RMSE and the log loss of predicted
  probabilities
- [`prob_metrics_ci()`](https://evalclass.github.io/precrec/reference/prob_metrics_ci.md)
  : Calculate CIs of the Brier score and the log loss

## Classification report

- [`classification_report()`](https://evalclass.github.io/precrec/reference/classification_report.md)
  : Classification report

## Datasets

P10N10, B500, B1000, IB500, IB1000, M2N50F5 and C3N150

- [`P10N10`](https://evalclass.github.io/precrec/reference/P10N10.md) :
  A small example dataset with several tied scores.
- [`B500`](https://evalclass.github.io/precrec/reference/B500.md) :
  Balanced data with 500 positives and 500 negatives.
- [`B1000`](https://evalclass.github.io/precrec/reference/B1000.md) :
  Balanced data with 1000 positives and 1000 negatives.
- [`IB500`](https://evalclass.github.io/precrec/reference/IB500.md) :
  Imbalanced data with 500 positives and 5000 negatives.
- [`IB1000`](https://evalclass.github.io/precrec/reference/IB1000.md) :
  Imbalanced data with 1000 positives and 10000 negatives.
- [`M2N50F5`](https://evalclass.github.io/precrec/reference/M2N50F5.md)
  : 5-fold cross validation sample.
- [`C3N150`](https://evalclass.github.io/precrec/reference/C3N150.md) :
  Multiclass sample with three classes.

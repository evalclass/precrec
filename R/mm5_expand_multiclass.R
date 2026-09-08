#
# Expand a multiclass dataset into one-vs-rest binary datasets
#
# Each of the K classes becomes its own binary problem - that class against
# all the others - so the whole binary pipeline is reused as it stands,
# including the precision-recall interpolation the package exists for.
#
# The classes ride on the model axis rather than an axis of their own. K
# decompositions of one dataset look to everything downstream like K models
# evaluated on one dataset, which means `plot`, `autoplot`, `as.data.frame`
# and the averaging all work without knowing about classes at all.
#
.expand_multiclass <- function(scores, labels, modnames, dsids) {
  lmats <- .as_score_matrices(scores)
  llabels <- .as_label_list(labels)

  # One set of labels shared by every score matrix is the common case
  if (length(lmats) != 1 && length(llabels) == 1) {
    llabels <- replicate(length(lmats), llabels[[1]], simplify = FALSE)
  }
  if (length(lmats) != length(llabels)) {
    .stop_invalid_arg(
      paste(
        "{.arg scores} and {.arg labels} must hold the same number of",
        "datasets, or {.arg labels} must hold one."
      ),
      arg = "labels"
    )
  }

  classnames <- .get_classnames(llabels)
  if (length(classnames) < 3) {
    .stop_invalid_arg(
      paste(
        "{.arg labels} must hold more than two classes for",
        "{.code multiclass = \"ovr\"}, not {length(classnames)}."
      ),
      arg = "labels", .envir = environment()
    )
  }

  # Model names and dataset IDs describe the matrices, not yet the
  # decompositions. Several matrices with nothing to tell them apart are
  # taken as several datasets, because the model axis is about to be spent
  # on the classes.
  n_mat <- length(lmats)
  if (is.null(modnames) && is.null(dsids)) {
    modnames <- rep("m1", n_mat)
    dsids <- seq_len(n_mat)
  } else if (is.null(modnames)) {
    modnames <- rep("m1", n_mat)
  } else if (is.null(dsids)) {
    dsids <- rep(1, n_mat)
  }
  .validate_modnames(modnames, n_mat)
  .validate_dsids(dsids, n_mat)

  # A single model has nothing to add to a class name; several models do
  single_model <- length(unique(modnames)) == 1

  n_out <- n_mat * length(classnames)
  out_scores <- vector("list", n_out)
  out_labels <- vector("list", n_out)
  out_modnames <- character(n_out)
  out_dsids <- vector("list", n_out)
  out_classes <- character(n_out)

  idx <- 0L
  for (i in seq_len(n_mat)) {
    col_idx <- .match_class_columns(lmats[[i]], classnames)
    for (k in seq_along(classnames)) {
      idx <- idx + 1L
      out_scores[[idx]] <- as.numeric(lmats[[i]][, col_idx[k]])

      # 1 for the class, 0 for the rest. reformat_data() reads the larger
      # of the two values as the positive class, so this needs no posclass.
      out_labels[[idx]] <- as.numeric(llabels[[i]] == classnames[k])
      out_classes[idx] <- classnames[k]
      out_dsids[[idx]] <- dsids[[i]]
      if (single_model) {
        out_modnames[idx] <- classnames[k]
      } else {
        out_modnames[idx] <- paste0(classnames[k], ":", modnames[i])
      }
    }
  }

  list(
    scores = out_scores,
    labels = out_labels,
    modnames = out_modnames,
    dsids = unlist(out_dsids),
    classes = out_classes,
    classnames = classnames
  )
}

#
# Take the score matrices out of the 'scores' argument
#
# One matrix or data frame is one dataset; a list holds several. Anything
# else cannot carry one column per class.
#
.as_score_matrices <- function(scores) {
  if (is.matrix(scores) || is.data.frame(scores)) {
    return(list(as.matrix(scores)))
  }

  if (is.list(scores) &&
    all(.map_lgl(scores, function(s) {
      is.matrix(s) || is.data.frame(s)
    }))) {
    return(.map(scores, as.matrix))
  }

  .stop_invalid_arg(
    paste(
      "{.arg scores} must be a matrix or a data frame with one column per",
      "class, or a list of them, for {.code multiclass = \"ovr\"}."
    ),
    arg = "scores"
  )
}

#
# Take the label vectors out of the 'labels' argument
#
.as_label_list <- function(labels) {
  if (is.list(labels) && !is.data.frame(labels)) {
    return(labels)
  }

  if (is.matrix(labels) || is.data.frame(labels)) {
    return(.map(seq_len(ncol(labels)), function(j) labels[, j]))
  }

  list(labels)
}

#
# Get the class names of a multiclass dataset
#
# A factor states its own classes, which keeps a class that is missing from
# one fold of a cross validation from disappearing along with it. Anything
# else is read off the values, in sorted order.
#
.get_classnames <- function(llabels) {
  if (all(.map_lgl(llabels, is.factor))) {
    lvs <- unique(unlist(.map(llabels, levels)))
    if (length(lvs) > 0L) {
      return(lvs)
    }
  }

  vals <- unique(unlist(.map(llabels, function(l) as.character(unique(l)))))
  sort(vals)
}

#
# Line the columns of a score matrix up with the classes
#
# Column names that name the classes are used whatever their order; without
# them the columns are taken to be in the order of the class names.
#
.match_class_columns <- function(mat, classnames) {
  if (ncol(mat) != length(classnames)) {
    .stop_invalid_arg(
      paste(
        "{.arg scores} must have one column per class:",
        "{length(classnames)} expected, {ncol(mat)} found."
      ),
      arg = "scores", .envir = environment()
    )
  }

  cnames <- colnames(mat)
  if (!is.null(cnames) && setequal(cnames, classnames)) {
    return(match(classnames, cnames))
  }

  seq_along(classnames)
}

#
# Check partial match - multiclass
#
.pmatch_multiclass <- function(val) {
  if (.is_string(val)) {
    if (val == "none" || val == "ovr") {
      return(val)
    }

    if (!is.na(pmatch(val, "none"))) {
      return("none")
    }

    if (!is.na(pmatch(val, "ovr"))) {
      return("ovr")
    }
  }

  val
}

#
# Decide whether the input is a multiclass dataset
#
# An explicit choice is honored. Otherwise the input is multiclass when the
# labels hold more than two classes and the scores hold one column per class
# - which is input the package used to reject outright, so nothing that
# worked before is read differently now.
#
.get_new_multiclass <- function(multiclass, scores, labels) {
  if (!is.null(multiclass)) {
    new_multiclass <- .pmatch_multiclass(multiclass)
    .assert_string(new_multiclass, "multiclass", c("none", "ovr"))
    return(new_multiclass)
  }

  n_class <- tryCatch(
    length(.get_classnames(.as_label_list(labels))),
    error = function(e) 0L
  )
  if (n_class < 3) {
    return("none")
  }

  is_matrix <- is.matrix(scores) || is.data.frame(scores) ||
    (is.list(scores) && length(scores) > 0L &&
      all(.map_lgl(scores, function(s) {
        is.matrix(s) || is.data.frame(s)
      })))
  if (!is_matrix) {
    return("none")
  }

  ncols <- .map_int(.as_score_matrices(scores), ncol)
  if (all(ncols == n_class)) {
    "ovr"
  } else {
    "none"
  }
}

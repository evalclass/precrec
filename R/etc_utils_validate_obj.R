#
# A generic to validate data
#
.validate <- function(x) UseMethod(".validate", x)

#
# Default .validate
#
.validate.default <- function(x) {
  stop(paste0("Unrecognized class for .validate(): '", class(x), "'"),
       call. = FALSE)
}

#
# Validate class items and attributes
#
.validate_basic <- function(obj, class_name, func_name, item_names, attr_names,
                            arg_names) {
  # Check class
  if (!methods::is(obj, class_name)) {
    stop(paste0("Expected ", class_name, " created by ", func_name, "(): ",
                class(obj)),
         call. = FALSE)
  }

  # Check class items
  ditems <- setdiff(item_names, names(obj))
  if (!is.null(item_names) && length(ditems) > 0) {
    stop(paste0("Invalid list items in ", class_name, ": ",
                paste(ditems, collapse = ", ")),
         call. = FALSE)
  }

  # Check attributes
  ditems <- setdiff(attr_names, names(attributes(obj)))
  if (!is.null(attr_names) && length(ditems) > 0) {
    stop(paste0("Invalid attributes in ", class_name, ": ",
                paste(ditems, collapse = ", ")),
         call. = FALSE)
  }

  # Check args
  ditems <- setdiff(names(attr(obj, "args")), arg_names)
  if (!is.null(arg_names) && length(ditems) > 0) {
    stop(paste0("Invalid args attribute in ", class_name, ": ",
                paste(ditems, collapse = ", ")),
         call. = FALSE)
  }
}

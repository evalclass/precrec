#
# A generic to validate data
#
.validate <- function(x) UseMethod(".validate")

#
# Default .validate
#
.validate.default <- function(x) {
  stop(paste0("Unrecognized class for .validate(): '", class(x), "'"))
}

#
# Validate class items and attributes
#
.validate_basic <- function(obj, class_name, func_name, item_names, attr_names,
                            arg_names) {
  # Check class
  if (class(obj) != class_name) {
    stop(paste0("Expected '", class_name, "' created by ", func_name, "(): ",
                class(obj)))
  }

  # Check class items
  ditems <- setdiff(item_names, names(obj))
  if (!is.null(item_names) && length(ditems) > 0) {
    stop(paste0("Missing items in '", class_name, "': ",
                paste(ditems, collapse = ", ")))
  }

  # Check attributes
  ditems <- setdiff(attr_names, names(attributes(obj)))
  if (!is.null(attr_names) && length(ditems) > 0) {
    stop(paste0("Missing items in '", class_name, "' attributes: ",
                paste(ditems, collapse = ", ")))
  }

  # Check args
  ditems <- setdiff(names(attr(obj, "args")), arg_names)
  if (!is.null(arg_names) && length(ditems) > 0) {
    stop(paste0("Invalid items in 'args' attribute of '", class_name, "': ",
                paste(ditems, collapse = ", ")))
  }
}

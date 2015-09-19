# A generic to validate data
.validate <- function(x) UseMethod(".validate")

# Default .validate
.validate.default <- function(x) {
  stop("An object of unknown class is specified")
}

# Validate class items and attributes
.validate_basic <- function(obj, class_name, func_name, item_names, attr_names,
                            arg_names) {
  # Check class
  if (class(obj) != class_name) {
    stop(paste0("Expected '", class_name, "' created by ", func_name, "()"))
  }

  # Check class items
  if (!is.null(item_names) && length(setdiff(item_names, names(obj))) > 0) {
    print(setdiff(item_names, names(obj)))
    stop(paste0("Missing items in '", class_name, "'"))
  }

  # Check attributes
  if (!is.null(attr_names)
      && length(setdiff(attr_names, names(attributes(obj)))) > 0) {
    stop(paste0("Missing items in '", class_name, "' attributes"))
  }

  # Check args
  if (!is.null(arg_names)
      && length(setdiff(names(attr(obj, "args")), arg_names) > 0)) {
    stop(paste0("Invalid items in 'args' attribute of '", class_name, "'"))
  }
}

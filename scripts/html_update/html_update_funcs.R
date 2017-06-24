update_html <- function(file_name, targets, tabs) {
  print(paste("Update", basename(file_name)))

  lines <- readLines(file_name)
  lines <- update_lines(lines, targets, tabs)

  conn <- file(file_name)
  writeLines(lines, conn)
  close(conn)
}

update_lines <- function(lines, targets, tabs) {

  n_targers <- length(targets)
  n_processed <- 0
  new_lines <- NULL
  in_target <- FALSE
  in_table <- FALSE
  for (i in seq_along(lines)) {
    if (n_targers == n_processed) {
      new_lines <- c(new_lines, lines[i])
      next
    } else if (!in_target && start_target(lines[i], n_processed, targets)) {
      tline <- trim_start(lines[i])
      if (tline != lines[i]) {
        new_lines <- c(new_lines, tline)
        in_table <- TRUE
      }
      in_target <- TRUE
    }

    if (in_target) {
      if (!in_table && start_table(lines[i])) {
        tline <- trim_start(lines[i])
        if (tline != "") {
          new_lines <- c(new_lines, tline)
        }
        in_table <- TRUE
      } else if (in_table && end_table(lines[i])) {
        n_processed <- n_processed + 1
        new_lines <- c(new_lines, get_html_table(tabs[n_processed]))
        tline <- trim_end(lines[i])
        if (tline != "") {
          new_lines <- c(new_lines, tline)
        }
        in_table <- FALSE
        in_target <- FALSE
      } else if (!in_table) {
        new_lines <- c(new_lines, lines[i])
      }
    } else {
      new_lines <- c(new_lines, lines[i])
    }
  }

  new_lines
}

start_target <- function(line, n_processed, targets) {
#  print(paste(as.character(regexpr(targets[n_processed+1], line)[1]), line))
  if (regexpr(targets[n_processed+1], line) > 0) {
    TRUE
  } else {
    FALSE
  }
}

start_table <- function(line) {
#  print(paste(as.character(regexpr("<table", line)), line))
  if (regexpr("<table", line) > 0) {
    TRUE
  } else {
    FALSE
  }
}

trim_start <- function(line) {
  sub("<table.*", "", line)
}

end_table <- function(line) {
#  print(paste(as.character(regexpr("</table>", line)), line))
  if (regexpr("</table>", line) > 0) {
    TRUE
  } else {
    FALSE
  }
}

trim_end <- function(line) {
  sub(".*</table>", "", line)
}

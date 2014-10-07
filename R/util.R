is_directory <- function(path) {
  file.info(path)$isdir
}

file_remove_if_exists <- function(filename, dir=FALSE) {
  if (file.exists(filename)) {
    if (is_directory(filename)) {
      unlink(filename, recursive=TRUE)
    } else {
      file.remove(filename)
    }
  }
}

backup <- function(filename, verbose=TRUE, move=FALSE) {
  if (file.exists(filename)) {
    pat <- sprintf("%s\\.([0-9]+)", basename(filename))
    found <- dir(dirname(filename), pattern=pat)
    if (length(found) > 0) {
      n <- max(as.integer(sub(pat, "\\1", found))) + 1
    } else {
      n <- 1
    }
    dest <- sprintf("%s.%d", filename, n)
    if (verbose) {
      action <- if (move) "Moving" else "Copying"
      message(sprintf("%s %s -> %s", action, filename, basename(dest)))
    }
    if (move) {
      file.rename(filename, dest)
    } else {
      file.copy(filename, dest)
    }
  }
}

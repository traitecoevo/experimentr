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

backup <- function(filename, verbose=TRUE) {
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
      message(sprintf("Backing up %s -> %s", filename, basename(dest)))
    }
    file.copy(filename, dest)
  }
}

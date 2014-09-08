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

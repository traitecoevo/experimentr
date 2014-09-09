target_fn <- function(a, b) {
  list(a=a, b=b)
}

reprocess_fn <- function(dat) {
  rep(dat, 2)
}

metadata_hook <- function() {
  list(version="1.0")
}

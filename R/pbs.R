make_pbs_file <- function(experiment, task, id,
                          email=getOption("experimentr.email"),
                          walltime="48:00", template=NULL) {
  if (is.null(template)) {
    template <- readLines(system.file("pbs.whisker",
                                      package="experimentr",
                                      mustWork=TRUE))
  }
  str <- whisker.render(template)
  filename <- file.path(path, pbs_filename(experiment, task, id))
  writeLines(str, filename)
  invisible(filename)
}

copy_runner <- function(dest="run_experiment.R") {
  src <- system.file("run_experiment.R", package="experimentr",
                     mustWork=TRUE)
  file.copy(src, dest, overwrite=TRUE)
}

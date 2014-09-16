##' Generate a pbs file from a template.  This is extremely unlikely
##' to be useful unless you are me.
##' @title Generate pbs files from a template
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param id Id number
##' @param email Email to send updates to
##' @param walltime Requested walltime
##' @param queue Name of the queue to use
##' @param template Contents of a whisker template, or NULL to use the
##' default.
##' @param path Path to write the pbs file to.
##' @export
make_pbs_file <- function(experiment, task, id,
                          email=getOption("experimentr.email"),
                          walltime="48:00", queue="normal",
                          template=NULL,
                          path=".") {
  if (is.null(template)) {
    template <- readLines(system.file("pbs.whisker",
                                      package="experimentr",
                                      mustWork=TRUE))
  }
  if (length(experiment) != 1 || length(task) != 1) {
    stop("experiment and task must be scalar")
  }

  f <- function(experiment, task, id, walltime, email, queue) {
    str <- whisker::whisker.render(template)
    filename <- file.path(path, pbs_filename(experiment, task, id))
    writeLines(str, filename)
    filename
  }
  invisible(sapply(id, function(x) f(experiment, task, x, walltime, email, queue)))
}

##' Copy a helper script that runs a parameter set from the experiment.
##' @title Copy helper script
##' @param dest Filename of the script
##' @export
copy_runner <- function(dest="run_experiment.R") {
  src <- system.file("run_experiment.R", package="experimentr",
                     mustWork=TRUE)
  file.copy(src, dest, overwrite=TRUE)
}

qsub <- function(pbs_filenames, echo_only=TRUE) {
  if (echo_only) {
    system2 <- function(command, args) {
      message(paste(command, args))
    }
  }
  for (i in pbs_filenames) {
    system2("qsub", i)
  }
}

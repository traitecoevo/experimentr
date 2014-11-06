##' @importFrom optparse make_option
experimentr_options <- function() {
  ## TODO: make help the default option.
  option_list <- list(
    make_option(c("-e", "--experiment"), type="character",
                help="name experiment"),
    make_option(c("-t", "--task"), type="character",
                help="Task within experiment to run"),
    make_option(c("-i", "--id"), type="integer",
                help="ID of the job to run"),
    make_option(c("-n", "--dry-run"), type="logical",
                default=FALSE, dest="dry_run",
                help="Dry run (don't actually run anything)"),
   make_option(c("-b", "--backup"), type="logical",
                default=FALSE, dest="backup",
                help="Backup any existing output"))

}

##' Run an experiment, from the command line.
##'
##' Don't use this in interactive mode this won't work
##' @title Run an experiment, from the command line
##' @importFrom optparse parse_args
##' @param args Arguments (or NULL to read from R's \code{commandArgs})
##' @export
##' @importFrom optparse OptionParser
main <- function(args=NULL) {
  if (is.null(args)) {
    args <- parse_args(OptionParser(option_list=experimentr_options()))
  }
  experiment <- args$experiment
  task       <- args$task
  id         <- args$id
  dry_run    <- args$dry_run
  backup    <- args$backup
  if (length(id) != 1L) {
    stop("Expected single id")
  }
  if (is.null(getOption("error"))) {
    options(error=traceback)
  }

  run_task(experiment, task, id, dry_run=dry_run, backup=backup)
}

##' Run a task.  This is designed to run locally, rather than being
##' run from a cluster.
##' @title Run a task
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param id Id of the jobs to run.  If omitted, all ids for this
##' experiment will be run.
##' @param parallel Run in parallel using \code{parallel::mclapply}?
##' @param ... Arguments passed through to \code{mclapply}.  In
##' particular, it will probably be useful to set
##' \code{mc.preschedule=FALSE} and specify \code{mc.cores}.
##' @param dry_run Logical, indicating if we should \emph{actually}
##' run things.
##' @param backup Logical, backyp any existing output
##' @export
run_task <- function(experiment, task, id=NULL, parallel=TRUE, ...,
                     dry_run=FALSE, backup=FALSE) {
  message("Experiment: ", experiment)
  message("Task:       ", task)

  dat <- yaml::yaml.load_file(experiments_filename())
  if (is.null(id)) {
    id <- ids(experiment)
  }
  message("Id:         ", paste(id, collapse=", "))

  f <- function(id) {
    pars <- load_task_info(experiment, task, id, dat)
    env <- create_environment(experiment, task, dat)

    if (!dry_run) {

      if(backup) {
      ## NOTE: These can easily get out-of-sync
        backup(pars$filename, move=TRUE)
        backup(pars$metaname, move=TRUE)
      }

      ## TODO :overwrite option currently does nothing
      save_metadata(pars$metaname, experiment, dat, env)
      run(pars, env)
    }
  }
  if (parallel && length(id) > 1L) {
    parallel::mclapply(id, f, ...)
  } else {
    lapply(id, f)
  }
  invisible(NULL)
}

run <- function(pars, env, capture_all_output=!interactive()) {
  if (capture_all_output) {
    message("Diverting messages to ", pars$logfile)
    dir.create(dirname(pars$logfile), showWarnings=FALSE, recursive=TRUE)
    con <- file(pars$logfile, open="wt")
    sink(con, type="message") # Dangerous!
    sink(con, type="output")
    on.exit(sink(NULL, type="message"))
    on.exit(sink(NULL, type="output"), add=TRUE)
  }
  message("--- Starting at ", Sys.time())
  f <- get(pars$function_name, env, mode="function")
  nms <- names(formals(f))
  ## TODO: Process prerequisites here to allow using filenames from
  ## previous versions.
  ret <- do.call(f, pars[names(pars) %in% nms], envir=env)
  saveRDS(ret, pars$filename)
  message("--- Finishing at ", Sys.time())
}

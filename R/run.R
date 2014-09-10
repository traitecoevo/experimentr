##' @importFrom optparse make_option
experimentr_options <- function() {
  ## TODO: make help the default option.
  option_list <- list(
    make_option(c("-e", "--experiment"), type="character",
                help="name experiment"),
    make_option(c("-t", "--task"), type="character",
                help="Task within experiment to run"),
    make_option(c("-i", "--id"), type="integer",
                help="ID of the job to run"))
}

##' Run an experiment, from the command line.
##'
##' Don't use this in interactive mode this won't work
##' @title Run an experiment, from the command line
##' @importFrom optparse parse_args
##' @param args Arguments (or NULL to read from R's \code{commandArgs})
##' @param dry_run Just print information but don't run anything
##' @export
##' @importFrom optparse OptionParser
main <- function(args=NULL, dry_run=FALSE) {
  if (is.null(args)) {
    args <- parse_args(OptionParser(option_list=experimentr_options()))
  }
  experiment <- args$experiment
  task       <- args$task
  id         <- args$id

  message("Experiment: ", experiment)
  message("Task:       ", task)
  message("Id:         ", id)

  dat <- yaml::yaml.load_file(experiments_filename())
  pars <- load_task_info(experiment, task, id, dat)
  env <- create_environment(experiment, task, dat)

  if (!dry_run) {
    save_metadata(experiment, task, id, dat, env)
    run(pars, env)
  }
}

run <- function(pars, env) {
  message("--- Starting at ", Sys.time())
  f <- get(pars$function_name, env, mode="function")
  nms <- names(formals(f))
  ## TODO: Process prerequisites here to allow using filenames from
  ## previous versions.
  backup(pars$filename, TRUE)
  ret <- do.call(f, pars[names(pars) %in% nms], envir=env)
  saveRDS(ret, pars$filename)
  message("--- Finishing at ", Sys.time())
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
##' @export
run_task <- function(experiment, task, id=NULL, parallel=TRUE, ...) {
  dat <- yaml::yaml.load_file(experiments_filename())
  if (is.null(id)) {
    id <- ids(experiment)
  }
  f <- function(id) {
    pars <- load_task_info(experiment, task, id, dat)
    env <- create_environment(experiment, task, dat)
    save_metadata(experiment, task, id, dat, env)
    run(pars, env)
  }
  if (parallel && length(id) > 1L) {
    parallel::mclapply(id, f, ...)
  } else {
    lapply(id, f)
  }
  invisible(NULL)
}

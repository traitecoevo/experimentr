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

  dat_all <- yaml::yaml.load_file(experiments_filename())
  dat <- get_experiment(experiment, dat_all)
  dat_task <- get_task(task, dat)

  path <- dat$path
  pars <- load_parameters(path, id, dat_task)
  env <- create_environment(dat, dat_task)
  if (!dry_run) {
    save_metadata(path, task, id)
    run(dat_task, pars, env)
  }
}

run <- function(task, pars, env) {
  message("--- Starting at ", Sys.time())
  f <- get(task[["function"]], env, mode="function")
  nms <- names(formals(f))
  ## TODO: Process prerequisites here to allow using filenames from
  ## previous versions.
  backup(pars$filename, TRUE)
  ret <- do.call(f, pars[names(pars) %in% nms], envir=env)
  saveRDS(ret, pars$filename)
  message("--- Finishing at ", Sys.time())
}
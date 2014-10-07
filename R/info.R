##' Get names of experiments
##' @title Names of known experiments
##' @export
experiments <- function() {
  dat <- yaml::yaml.load_file(experiments_filename())
  names(dat)
}

##' Get names of tasks
##' @title Names of known tasks
##' @param experiment Name of the experiment
##' @export
tasks <- function(experiment) {
  dat <- yaml::yaml.load_file(experiments_filename())
  exp <- get_experiment(experiment, dat)
  names(exp$tasks)
}
##' Load parameters
##' @title Load parameters
##' @param experiment Name of experiment
##' @export
load_parameters <- function(experiment) {
  parameters_csv <- parameters_csv_name(experiment)
  if (!file.exists(parameters_csv)) {
    stop("Did not find parameters file at ", parameters_csv)
  }
  read.csv(parameters_csv, stringsAsFactors=FALSE)
}

##' Get valid ids
##' @title Get valid ids
##' @param experiment Name of the experiment
##' @export
ids <- function(experiment) {
  load_parameters(experiment)$id
}

##' Look for jobs that have not completed.  Very simple minded and
##' only looks for existance of the output file.  Eventually this will
##' support running a custom function over the output.
##' @title Identify incomplete ids
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param test Function to apply to each output to determine
##' "done-ness" (not yet implemented)
##' @export
ids_incomplete <- function(experiment, task, test=NULL) {
  if (!is.null(test)) {
    .NotYetImplemented()
  }
  yml <- yaml::yaml.load_file(experiments_filename())
  get_task(experiment, task, yml) # check things exist
  id <- ids(experiment)
  files <- output_filename(experiment, task, id)
  id[!file.exists(files)]
}

##' Print information on how many jobs have started (or possibly how
##' many have completed)
##' @title Print job information
##' @param experiment Name of the experiment
##' @param task Name of the task within experiment
##' @export
print_progress <- function(experiment, task) {
  info <- progress(experiment, task)
  message(sprintf("%d / %d started", sum(info$started), nrow(info)))
}

##' @rdname print_progress
progress <- function(experiment, task) {
  yml <- yaml::yaml.load_file(experiments_filename())
  get_task(experiment, task, yml) # check things exist
  id <- ids(experiment)
  files <- output_filename(experiment, task, id)
  started <- file.exists(files)
  data.frame(id=id, started=started)
}

##' Load output for a given task
##' @title Load output for a given task
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param id Optional vector of ids
##' @param f Optional Function to apply to each output
##' @param missing_ok Logical indicating if missing ids are OK: if so
##' a \code{NULL} will be given for this set.
##' @export
##' @author Rich FitzJohn
load_output <- function(experiment, task, id=NULL, f=identity,
                        missing_ok=TRUE) {
  if (is.null(id)) {
    id <- ids(experiment)
  }

  yml <- yaml::yaml.load_file(experiments_filename())
  get_task(experiment, task, yml) # check things exist
  files <- output_filename(experiment, task, id)

  ok <- file.exists(files)
  if (any(!ok) && !missing_ok) {
    stop("Missing output files: ", paste(id[ok], collapse=", "))
  }

  pars <- load_parameters(experiment)
  i <- match(id, pars$id)
  if (any(is.na(i))) {
    stop("Invalid ids: ", paste(id[is.na(i)], collapse=", "))
  }
  pars <- pars[i,]
  ret <- vector("list", length(ids))
  ret[ok] <- lapply(files[ok], function(x) f(readRDS(x)))
  attr(ret, "pars") <- pars
  ret
}

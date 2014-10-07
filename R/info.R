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

##' Get valid ids
##' @title Get valid ids
##' @param experiment Name of the experiment
##' @export
ids <- function(experiment) {
  p <- read.csv(parameters_csv_name(experiment),
                stringsAsFactors=FALSE)
  p$id
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

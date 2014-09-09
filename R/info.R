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

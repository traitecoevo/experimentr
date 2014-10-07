setup_experiment_pars <- function(pars=NULL) {
  if (!is.data.frame(pars)) {
    stop("pars must be a data.frame")
  }
  if ("id" %in% names(pars)) {
    stop("'id' cannot be used in the parameters data.frame")
  }
  id <- seq_len(nrow(pars))
  data.frame(id=id, pars, stringsAsFactors=FALSE)
}

##' Set up an experiment
##' @title Set up an experiment
##' @param path Path within \code{output/} that things will be stored
##' in.  Also the name of the key in the yml.
##' @param pars data.frame of parameters
##' @param packages Optional character vector of packages to load for
##' every task in this experiment
##' @param scripts Optional characte vector of scripts to source for
##' every task in this experiment
##' @param metadata Metadata hook
##' @param overwrite Overwrite an existing experiment?
##' @param purge If overwriting, also delete all the output from that
##' experiment?
##' @author Rich FitzJohn
##' @export
setup_experiment <- function(path, pars, packages=NULL, scripts=NULL,
                             metadata=NULL,
                             overwrite=FALSE, purge=FALSE) {
  experiments_file <- experiments_filename()
  output_path <- output_path(path)
  parameters_file <- parameters_csv_name(path)

  if (overwrite) {
    remove_experiment(path, purge)
  }

  if (file.exists(experiments_file)) {
    yml <- yaml::yaml.load_file(experiments_file)
    if (path %in% names(yml)) {
      stop("experiment already within file")
    }
  }

  if (!overwrite && file.exists(output_path)) {
    stop("path already exists")
  }
  dir.create(output_path, FALSE)
  write.csv(setup_experiment_pars(pars), parameters_file, row.names=FALSE)

  ret <- list(list(packages=packages,
                   scripts=scripts,
                   metadata=metadata,
                   tasks=NULL))
  names(ret) <- path

  if (file.exists(experiments_file)) {
    str <- readLines(experiments_file)
  } else {
    str <- character(0)
  }
  str_new <- strsplit(yaml::as.yaml(ret), "\n")[[1]]
  writeLines(c(str, "\n", str_new), experiments_file)
}


##' Add parameters to an existing experiment
##' @title Add parameters to an experiment
##' @param experiment Name of the experiment
##' @param pars data.frame of the parameters.  Column names must match
##' the existing parameters
##' @export
add_parameters <- function(experiment, pars) {
  experiments_file <- experiments_filename()
  yml <- yaml::yaml.load_file(experiments_file)
  exp <- get_experiment(experiment, yml)

  parameters_file <- parameters_csv_name(experiment)
  pars_existing <- read.csv(parameters_file, stringsAsFactors=FALSE)
  cols <- setdiff(names(pars_existing), "id")
  if (!identical(names(pars), cols)) {
    stop("New parameters must have column names: ",
         paste(sprintf('"%s"', cols), collapse=", "))
  }
  id_start <- pars_existing$id[nrow(pars_existing)] + 1
  id_new <- seq(id_start, by=1, length=nrow(pars))
  pars_with_id <- cbind(id=id_new, pars)
  res <- rbind(pars_existing, pars_with_id)
  write.csv(res, parameters_file, row.names=FALSE)
  invisible(id_new)
}

remove_experiment <- function(experiment, purge=FALSE) {
  experiments_file <- experiments_filename()
  if (file.exists(experiments_file)) {
    yml <- yaml::yaml.load_file(experiments_file)
    if (experiment %in% names(yml)) {
      yml <- yml[names(yml) != experiment]
      if (length(yml) == 0) {
        file.remove(experiments_file)
      } else {
        writeLines(yaml::as.yaml(yml), experiments_file)
      }
    }
  }
  if (purge) {
    file_remove_if_exists(output_path(experiment))
    file_remove_if_exists(parameters_csv_name(experiment))
  }
}

#' Add a task to an experiment
#'
#' Adds a task to an existing experiment
#' @param experiment name
#' @param task task name
#' @param function_name name of function to call
#' @param common_parameters Parameters that are constant for all runs within this task
#' @param packages Optional character vector of packages to load for
#' every task in this experiment
#' @param scripts Optional characte vector of scripts to source for
#' every task in this experiment
#' @param overwrite Replace existing specifiction, if it exists
#' @param purge If overwriting, delete files associated with previous
#' task?
#' @param depends Does this task depend on other tasks existing
#' @export
add_task <- function(experiment, task, function_name,
                     common_parameters=NULL,
                     packages=NULL, scripts=NULL,
                     overwrite=FALSE, purge=FALSE,
                     depends=NULL) {
  experiments_file <- experiments_filename()
  yml <- yaml::yaml.load_file(experiments_file)
  exp <- get_experiment(experiment, yml)
  path_output <- output_task_path(experiment, task)
  path_log <- log_task_path(experiment, task)
  if (task %in% names(exp$tasks)) {
    if (!overwrite) {
      stop("task already exists")
    }
    if (purge) {
      file_remove_if_exists(path_output)
      file_remove_if_exists(path_log)
    }
  }
  dir.create(path_output, FALSE)
  dir.create(path_log, FALSE)

  ret <- list(function_name=function_name)
  ## No checking here:
  ret$common_parameters <- common_parameters # named
  ret$packages <- packages # character
  ret$scripts <- scripts # character
  ret$depends <- depends # needs care
  yml[[experiment]]$tasks[[task]] <- ret
  writeLines(yaml::as.yaml(yml), experiments_file)
}

##' Create directories needed for experimentr
##' @title Create directories needed for experimentr
##' @export
create_dirs <- function() {
  dir.create("experiments", FALSE)
  dir.create("experiments/parameters", FALSE)
  dir.create("experiments/output", FALSE)
}

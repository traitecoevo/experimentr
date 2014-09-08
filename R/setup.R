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
##' @author Rich FitzJohn
##' @export
setup_experiment <- function(path, pars, packages=NULL, scripts=NULL,
                             overwrite=FALSE, purge=FALSE) {
  experiments_file <- experiments_filename()
  output_path <- output_path(path)
  parameters_file <- parameters_csv_name(path)

  if (overwrite) {
    remove_experiment(path, purge)
  }

  if (file.exists(experiments_file)) {
    yml <- yaml::yaml.load_file(experiments_file)
    stop("experiment already within file")
  }

  if (!overwrite && file.exists(output_path)) {
    stop("path already exists")
  }
  dir.create(output_path, FALSE)
  write.csv(setup_experiment_pars(pars), parameters_file, row.names=FALSE)

  ret <- list(list(packages=packages,
                   scripts=scripts,
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

add_task <- function(experiment, task, function_name,
                     common_parameters=NULL,
                     packages=NULL, scripts=NULL,
                     overwrite=FALSE, purge=FALSE,
                     depends=NULL) {
  experiments_file <- experiments_filename()
  yml <- yaml::yaml.load_file(experiments_file)
  exp <- get_experiment(experiment, yml)
  path <- output_task_path(experiment, task)
  if (task %in% names(exp$tasks)) {
    if (!overwrite) {
      stop("task already exists")
    }
    if (purge) {
      file_remove_if_exists(path)
    }
  }
  dir.create(path, FALSE)

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

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
setup_experiment <- function(path, pars, packages=NULL, scripts=NULL) {
  experiments_file <- experiments_filename()
  output_path <- output_path(path)
  parameters_file <- parameters_csv_name(path)

  if (file.exists(output_path)) {
    stop("path already exists")
  }

  if (file.exists(experiments_file)) {
    yml <- yaml::yaml.load_file(experiments_file)
    if (path %in% names(yml)) {
      stop("experiment already within file")
    }
  }

  dir.create(output_path)
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

##' Create directories needed for experimentr
##' @title Create directories needed for experimentr
##' @export
create_dirs <- function() {
  dir.create("experiments", FALSE)
  dir.create("experiments/parameters", FALSE)
  dir.create("experiments/output", FALSE)
}

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

setup_experiment <- function(path, pars, name=basename(path),
                             packages=NULL, scripts=NULL) {
  experiments_file <- experiments_filename()
  output_path <- output_path(path)
  parameters_file <- parameters_csv_name(path)

  if (file.exists(output_path)) {
    stop("path already exists")
  }

  if (file.exists(experiments_file)) {
    yml <- yaml::yaml.load_file(experiments_file)
    if (name %in% names(yml)) {
      stop("experiment already within file")
    }
  }

  dir.create(output_path)
  write.csv(setup_experiment_pars(pars), parameters_file, row.names=FALSE)

  ret <- list(list(path=path,
                   packages=packages,
                   scripts=scripts,
                   tasks=NULL))
  names(ret) <- name

  if (file.exists(experiments_file)) {
    str <- readLines(experiments_file)
  } else {
    str <- character(0)
  }
  str_new <- strsplit(yaml::as.yaml(ret), "\n")[[1]]
  writeLines(c(str, "\n", str_new), experiments_file)
}

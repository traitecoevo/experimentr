option_list <- function() {
  option_list <- list(
    make_option(c("-e", "--experiment"), type="character",
                help="name experiment"),
    make_option(c("-t", "--task"), type="character",
                help="Task within experiment to run"),
    make_option(c("-i", "--id"), type="integer",
                help="ID of the job to run"))
}

load_parameters <- function(path, id, yml) {
  parameters_csv <- file.path(path, "parameters.csv")
  if (!file.exists(parameters_csv)) {
    stop("Did not find parameters file at ", parameters_csv)
  }
  p <- read.csv(parameters_csv, stringsAsFactors=FALSE)
  i <- match(id, p$id)
  if (length(i) != 1) {
    stop("Corrupt file -- more than one case found")
  } else if (is.na(i)) {
    stop(sprintf("id %d not found in parameters", id))
  }
  p <- modifyList(as.list(yml$common_parameters),
                  as.list(p[i,]))
  p$output_directory <- file.path(path, yml$path)
  p$filename <- file.path(p$output_directory,
                          paste0(p$id, ".rds"))
  p
}

get_task <- function(task, yml) {
  if (!(task %in% names(yml$tasks))) {
    stop("Did not find task within experiment")
  }
  yml$tasks[[task]]
}

get_experiment <- function(experiment, yml) {
  if (!(experiment %in% names(yml))) {
    stop("Did not find experiment within overall data")
  }
  yml[[experiment]]
}

git_info <- function() {
  sha <- system("git rev-parse HEAD", intern=TRUE)
  status <- system("git status -s", intern=TRUE)
  if (length(status) > 0) {
    attr(sha, "status") <- status
  }
  sha
}

save_metadata <- function(pars, path, task) {
  filename <- file.path(pars$output_directory,
                        sprintf("%s_meta_%s.rds", pars$id, task))
  dat <- list(sessionInfo=sessionInfo(),
              Sys.info=Sys.info(),
              git=git_info())
  if ("tree" %in% loadedNamespaces()) {
    dat$tree=tree:::git_sha()
  }
  dir.create(pars$output_directory, FALSE, recursive=TRUE)  
  saveRDS(dat, filename)
}

create_environment <- function(dat, dat_task) {
  packages <- c(dat$packages, dat_task$packages)
  scripts  <- c(dat$scripts, dat_task$scripts)
  e <- new.env(parent=.GlobalEnv)
  for (p in packages) {
    library(p, character.only=TRUE)
  }
  for (s in scripts) {
    source(s, e, chdir=TRUE)
  }
  e
}

backup <- function(filename, verbose=TRUE) {
  if (file.exists(filename)) {
    pat <- sprintf("%s\\.([0-9]+)", basename(filename))
    found <- dir(dirname(filename), pattern=pat)
    if (length(found) > 0) {
      n <- max(as.integer(sub(pat, "\\1", found))) + 1
    } else {
      n <- 1
    }
    dest <- sprintf("%s.%d", filename, n)
    if (verbose) {
      message(sprintf("Backing up %s -> %s", filename, basename(dest)))
    }
    file.copy(filename, dest)
  }
}

run <- function(task, pars, env) {
  message("--- Starting at ", Sys.time())
  f <- get(task[["function"]], env, mode="function")
  nms <- names(formals(f))
  if (!("filename" %in% nms)) {
    stop("Function must take 'filename' as an argument")
  }
  backup(pars$filename, TRUE)
  do.call(f, pars[names(pars) %in% nms], envir=env)
  message("--- Finishing at ", Sys.time())
}

setup_experiment_pars <- function(pars=NULL) {
  if (!is.data.frame(pars)) {
    stop("pars must be a data.frame")
  }
  id <- seq_len(nrow(pars))
  data.frame(id=id, pars, stringsAsFactors=FALSE)
}

setup_experiment <- function(path, pars, name=basename(path),
                             packages=NULL, scripts=NULL) {
  if (file.exists(path)) {
    stop("path already exists")
  }
  yml <- yaml::yaml.load_file("experiments.yml")
  if (name %in% names(yml)) {
    stop("experiment already within file")
  }
  pars <- setup_experiment_pars(pars)

  dir.create(path)
  write.csv(pars, file.path(path, "parameters.csv"), row.names=FALSE)
  
  ret <- list(list(path=path,
                   packages=packages,
                   scripts=scripts,
                   tasks=NULL))
  names(ret) <- name

  str <- readLines("experiments.yml")
  str_new <- strsplit(yaml::as.yaml(ret), "\n")[[1]]
  writeLines(c(str, "\n", str_new), "experiments.yml")
}

main <- function(args) {
  experiment <- args$experiment
  task       <- args$task
  id         <- args$id

  message("Experiment: ", experiment)
  message("Task:       ", task)
  message("Id:         ", id)
  
  dat_all <- yaml::yaml.load_file("experiments.yml")
  dat <- get_experiment(experiment, dat_all)
  dat_task <- get_task(task, dat)

  path <- dat$path
  pars <- load_parameters(path, id, dat_task)
  env <- create_environment(dat, dat_task)
  save_metadata(pars, path, task)
  run(dat_task, pars, env)
}

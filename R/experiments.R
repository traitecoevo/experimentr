load_task_info <- function(experiment, task, id, yml) {
  p <- load_parameters(experiment)

  dat_task <- get_task(experiment, task, yml)

  i <- match(id, p$id)
  if (length(i) != 1) {
    stop("Corrupt file -- more than one case found")
  } else if (is.na(i)) {
    stop(sprintf("id %d not found in parameters", id))
  }
  p <- modifyList(as.list(dat_task$common_parameters),
                  as.list(p[i,]))
  p$output_directory <- output_task_path(experiment, task)
  p$log_directory <- log_task_path(experiment, task)
  p$filename <- output_filename(experiment, task, p$id)
  p$metaname <- metadata_filename(experiment, task, p$id)
  p$logfile <- log_filename(experiment, task, p$id)
  p$function_name <- dat_task[["function_name"]]

  ## TODO: Validation so that we don't overwrite names we use elsewhere?
  for (i in names(dat_task$depends)) {
    p[[i]] <- load_output1(experiment, dat_task$depends[[i]], id)
  }

  p
}

load_output1 <- function(experiment, task, id) {
  readRDS(output_filename(experiment, task, id))
}

get_task <- function(experiment, task, yml) {
  yml_exp <- get_experiment(experiment, yml)
  if (!(task %in% names(yml_exp$tasks))) {
    stop("Did not find task within experiment")
  }
  ret <- yml_exp$tasks[[task]]
  ret$path <- task
  ret$experiment <- yml_exp
  ret
}

get_experiment <- function(experiment, yml) {
  if (!(experiment %in% names(yml))) {
    stop("Did not find experiment within overall data")
  }
  ret <- yml[[experiment]]
  ret$path <- experiment
  ret
}

## NOTE: this will behave badly if git is missing, or if not in a git
## repo.
##
## If git is not installed, we can get the version by reading
##   .git/HEAD -- grab the filename, pr refs/heads/master
##   refs/heads/master: SHA
git_info <- function() {
  sha <- system("git rev-parse HEAD", intern=TRUE)
  status <- system("git status -s", intern=TRUE)
  if (length(status) > 0) {
    attr(sha, "status") <- status
  }
  sha
}

save_metadata <- function(filename, experiment, dat, env) {
  dir.create(dirname(filename), showWarnings=FALSE, recursive=TRUE)
  metadata <- list(sessionInfo=sessionInfo(),
                   Sys.info=Sys.info(),
                   git=git_info())

  hook <- get_experiment(experiment, dat)$metadata
  if (!is.null(hook)) {
    metadata_exp <- get(hook, env, mode="function")()
    if (!is.list(metadata_exp)) {
      stop("Metadata hook must return a list")
    }
    metadata <- c(metadata, metadata_exp)
  }
  saveRDS(metadata, filename)
}

create_environment <- function(experiment, task, yml) {
  dat_task <- get_task(experiment, task, yml)
  dat_exp  <- dat_task$experiment

  packages <- c(dat_exp$packages, dat_task$packages)
  scripts  <- c(dat_exp$scripts, dat_task$scripts)

  e <- new.env(parent=.GlobalEnv)
  for (p in packages) {
    library(p, character.only=TRUE)
  }
  for (s in scripts) {
    source(s, e, chdir=TRUE)
  }
  e
}

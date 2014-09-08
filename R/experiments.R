load_parameters <- function(experiment, id, yml) {
  parameters_csv <- parameters_csv_name(experiment)
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
  p$output_directory <- output_task_path(experiment, yml$path)
  p$filename <- output_filename(experiment, yml$path, p$id)
  p
}

get_task <- function(task, yml) {
  if (!(task %in% names(yml$tasks))) {
    stop("Did not find task within experiment")
  }
  ret <- yml$tasks[[task]]
  ret$path <- task
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

git_info <- function() {
  sha <- system("git rev-parse HEAD", intern=TRUE)
  status <- system("git status -s", intern=TRUE)
  if (length(status) > 0) {
    attr(sha, "status") <- status
  }
  sha
}

save_metadata <- function(experiment, task, id) {
  filename <- metadata_filename(experiment, task, id)
  dat <- list(sessionInfo=sessionInfo(),
              Sys.info=Sys.info(),
              git=git_info())
  if ("tree" %in% loadedNamespaces()) {
    dat$tree=tree:::git_sha()
  }
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

experiments_filename <- function() {
  file.path("experiments", "experiments.yml")
}
parameters_csv_name <- function(experiment) {
  file.path("experiments/parameters", paste0(experiment, ".csv"))
}
output_path <- function(experiment) {
  file.path("experiments/output", experiment)
}
output_task_path <- function(experiment, task) {
  output_path(file.path(experiment, task))
}
output_filename <- function(experiment, task, id) {
  file.path(output_task_path(experiment, task),
            paste0(id, ".rds"))
}
metadata_filename <- function(experiment, task, id) {
  file.path(output_task_path(experiment, task),
            paste0(id, "_meta.rds"))
}
pbs_filename <- function(experiment, task, id) {
  sprintf("%s__%s__%d.pbs", experiment, task, id)
}

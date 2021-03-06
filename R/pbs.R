##' Generate a pbs file from a template.  This is extremely unlikely
##' to be useful unless you are me.
##' @title Generate pbs files from a template
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param id Id number
##' @param email Email to send updates to
##' @param walltime Requested walltime
##' @param queue Name of the queue to use
##' @param memory Memory requested
##' @param ncpus Number of cpus requested
##' @param template Contents of a whisker template, or NULL to use the
##' default.
##' @param path Path to write the pbs file to.
##' @export
make_pbs_file <- function(experiment, task, id=NULL,
                          email=getOption("experimentr.email"),
                          walltime="48:00:00", queue="normal",
                          memory="1GB", ncpus="1",
                          template=NULL,
                          path=".") {
  if (is.null(template)) {
    template <- readLines(system.file("pbs.whisker",
                                      package="experimentr",
                                      mustWork=TRUE))
  }
  if (length(experiment) != 1 || length(task) != 1) {
    stop("experiment and task must be scalar")
  }
  if (is.null(id)) {
    id <- ids(experiment)
  }

  f <- function(experiment, task, id, walltime, email, queue, memory, ncpus) {
    str <- whisker::whisker.render(template)
    filename <- file.path(path, pbs_filename(experiment, task, id))
    writeLines(str, filename)
    filename
  }
  invisible(sapply(id, function(x)
                   f(experiment, task, x, walltime, email, queue, memory, ncpus)))
}

##' Copy a helper script that runs a parameter set from the experiment.
##' @title Copy helper script
##' @param dest Filename of the script
##' @export
copy_runner <- function(dest="run_experiment.R") {
  src <- system.file("run_experiment.R", package="experimentr",
                     mustWork=TRUE)
  file.copy(src, dest, overwrite=TRUE)
}

qsub <- function(pbs_filenames, echo_only=TRUE, verbose=TRUE) {
  if (echo_only) {
    system2 <- function(command, args, ...) {
      message(paste(command, args, ...))
    }
  }
  pbs_ids <- vector("list", length.out=length(pbs_filenames))
  for (i in seq_along(pbs_filenames)) {
    if (verbose) {
      message("Launching ", pbs_filenames[[i]])
    }
    pbs_ids[[i]] <- system2("qsub", pbs_filenames[[i]], stdout=TRUE)
  }
  ## TODO: Throw an error if the job was refused.
  invisible(pbs_ids)
}

##' Launch jobs via qsub, saving information about PBS job numbers
##' @title Launch jobs via qsub
##' @param experiment Name of the experiment
##' @param task Name of the task
##' @param id Vector of ids
##' @param jobfile Name of the file to store information associating
##' pbs job id with experimentrs information.  This is used by
##' \code{\link{move_pbs_logs}}.
##' @param email Email address for PBS to use
##' @param walltime Requested walltime (default two days)
##' @param queue Name of the queue (default is "normal")
##' @param memory Memory requested
##' @param ncpus Number of cpus requested
##' @param template Template to build job file from (or NULL)
##' @param path Path to make/submit PBS files from
##' @param verbose Print name of PBS file during submission?
##' @export
launch_pbs <- function(experiment, task, id=NULL, jobfile="pbs_jobs.csv",
                      email=getOption("experimentr.email"),
                      walltime="48:00:00", queue="normal",
                      memory="1GB", ncpus="1",
                      template=NULL,
                      path=".", verbose=TRUE) {
  if (is.null(id)) {
    id <- ids(experiment)
  }
  ## NOTE: this will happily overshoot on the id.
  files <- make_pbs_file(experiment,
                         task,
                         id=id,
                         email=email,
                         walltime=walltime,
                         queue=queue,
                         memory=memory,
                         ncpus=ncpus,
                         template=template,
                         path=path)
  res <- sapply(files, qsub, echo_only=FALSE, verbose=verbose)
  dat <- process_pbs(experiment, task, id, res)
  append_jobfile(dat, jobfile)
  invisible(res)
}

append_jobfile <- function(dat, jobfile="pbs_jobs.csv") {
  if (file.exists(jobfile)) {
    prev <- read.csv(jobfile, stringsAsFactors=FALSE)
    ## *Replace* existing ids.
    v <- c("experiment", "task", "id")
    hash_prev <- apply(prev[v], 1, paste, collapse="\r")
    hash_dat <- apply(dat[v], 1, paste, collapse="\r")
    prev <- prev[!(hash_prev %in% hash_dat),]
  } else {
    prev <- NULL
  }
  write.csv(rbind(prev, dat), jobfile, row.names=FALSE)
}

process_pbs <- function(experiment, task, id, pbs) {
  data.frame(experiment=experiment, task=task, id=id,
             pbs_id=as.integer(sub("\\..+$", "", pbs)),
             stringsAsFactors=FALSE)
}

##' Move PBS output into the approprate place.
##' @title Move PBS output into the approprate place
##' @param path Path to the PBS files.  Assumed to be current
##' directory if omitted.
##' @param jobfile Name of the csv file that associates PBS id numbers
##' with experimentr.  The default here is the same as
##' \code{\link{launch_pbs}}.
##' @param verbose Print information about what is being moved?
##' @export
move_pbs_logs <- function(path=".", jobfile="pbs_jobs.csv",
                          verbose=TRUE) {
  if (!file.exists(jobfile)) {
    stop(sprintf("jobfile %s does not exist: launch jobs with launch_pbs",
                 jobfile))
  }
  jobs <- read.csv(jobfile, stringsAsFactors=FALSE)

  re <- ".*\\.e([0-9]+)$"
  pos <- dir(path, pattern=re)
  pos_pbs_id <- as.integer(sub(re, "\\1", pos))

  i <- match(pos_pbs_id, jobs$pbs_id)
  to_move <- pos[!is.na(i)]
  to_move_info <- jobs[i[!is.na(i)],]
  to_move_out <- sub("\\.e([0-9]+)$", ".o\\1", to_move, perl=TRUE)

  ## Quick check:
  if (!isTRUE(all.equal(as.integer(sub(re, "\\1", to_move)),
                        to_move_info$pbs_id))) {
    stop("alignment failed")
  }
  if (!isTRUE(all(file.exists(file.path(path, to_move_out))))) {
    stop("output files do not exist failed")
  }

  to_move_info$dest <- file.path("experiments", "output",
                                 to_move_info$experiment,
                                 to_move_info$task, "pbs")
  to_move_info$f_err <- file.path(to_move_info$dest,
                                  paste0(to_move_info$id, ".err"))
  to_move_info$f_out <- file.path(to_move_info$dest,
                                  paste0(to_move_info$id, ".out"))
  to_move_info$f_pbs <- pbs_filename(to_move_info$experiment,
                                     to_move_info$task,
                                     to_move_info$id)

  invisible(lapply(unique(to_move_info$dest), dir.create, FALSE, TRUE))
  for (i in seq_along(to_move)) {
    if (verbose) {
      message(sprintf("moving %s : %s : %d (%d)",
                      to_move_info$experiment[i], to_move_info$task[i],
                      to_move_info$id[i], to_move_info$pbs_id[i]))
    }
    file.rename(file.path(path, to_move[i]),     to_move_info$f_err[i])
    file.rename(file.path(path, to_move_out[i]), to_move_info$f_out[i])
    file.remove(file.path(path, to_move_info$f_pbs[i]))
  }
}


##' Generate a suitable time string for pbs
##' @title Generate pbs time string
##' @param days Number of days
##' @param hours Number of hours
##' @param minutes Number of minutes (less than 60)
##' @author Rich FitzJohn
##' @export
pbs_time <- function(days=1, hours=0, minutes=0) {
  hours <- hours + round(days * 24)
  if (minutes > 59) {
    stop("Invalid value for minutes")
  }
  sprintf("%02d:%02d:00", hours, minutes)
}

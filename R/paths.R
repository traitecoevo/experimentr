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
log_task_path <- function(experiment, task) {
  file.path("experiments/logs", experiment, task)
}
log_filename <- function(experiment, task, id) {
  file.path(log_task_path(experiment, task),
            paste0(id, ".txt"))
}

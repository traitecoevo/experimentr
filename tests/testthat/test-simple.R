if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

test_that("Simple dry run", {
  unlink("experiments", recursive=TRUE)
  experimentr::create_dirs()

  pars <- expand.grid(a=1:10, b=runif(10))
  setup_experiment("trial", pars, scripts="simulation.R",
                   packages=c("ape", "MASS"))

  ## Manually add a task:
  yml <- yaml::yaml.load_file(experiments_filename())
  yml$trial$tasks <- list(testing=list("function"="target_fn"))
  writeLines(yaml::as.yaml(yml), experiments_filename())

  for (i in 1:10) {
    main(list(experiment="trial", task="testing", id=i))
    expect_that(file.exists(output_filename("trial", "testing", i)), is_true())
  }
})

test_that("pbs workflow", {
  unlink("pbs", recursive=TRUE)
  copy_runner()
  options(experimentr.email="rich.fitzjohn@gmail.com")
  devtools::load_all("../../")
  dir.create("pbs")
  files <- make_pbs_file("trial", "testing", 1:100, path="pbs")
  qsub(files)
})

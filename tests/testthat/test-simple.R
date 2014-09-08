if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

test_that("Simple dry run", {
  unlink("experiments", recursive=TRUE)
  experimentr::create_dirs()

  pars <- expand.grid(a=1:10, b=runif(10))
  setup_experiment("trial", pars, scripts="simulation.R",
                   packages=c("ape", "MASS"), overwrite=TRUE)
  add_task("trial", "testing", "target_fn")
  add_task("trial", "reprocess", "reprocess_fn",
           depends=list(dat="testing"))

  for (i in 1:10) {
    main(list(experiment="trial", task="testing", id=i))
    expect_that(file.exists(output_filename("trial", "testing", i)), is_true())
  }

  for (i in 1:10) {
    main(list(experiment="trial", task="reprocess", id=i))
    expect_that(file.exists(output_filename("trial", "reprocess", i)), is_true())
  }
})

test_that("pbs workflow", {
  unlink("pbs", recursive=TRUE)
  copy_runner()
  options(experimentr.email="rich.fitzjohn@gmail.com")
  devtools::load_all("../../")
  dir.create("pbs")
  files <- make_pbs_file("trial", "testing", 1:100, path="pbs", queue="express")
  qsub(files)
})

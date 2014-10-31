if (interactive()) {
  devtools::load_all("../../")
  library(testthat)
}

context("experimentr")

test_that("Simple dry run", {
  unlink("experiments", recursive=TRUE)
  experimentr::create_dirs()

  pars <- expand.grid(a=1:10, b=runif(10))
  setup_experiment("trial", pars, scripts="simulation.R",
                   packages=c("utils", "MASS"),
                   metadata="metadata_hook",
                   overwrite=TRUE)
  add_task("trial", "testing", "target_fn")
  add_task("trial", "reprocess", "reprocess_fn",
           depends=list(dat="testing"))

  for (i in 1:10) {
    main(list(experiment="trial", task="testing", id=i, dry_run=FALSE))
    expect_that(file.exists(output_filename("trial", "testing", i)), is_true())
  }

  for (i in 1:10) {
    main(list(experiment="trial", task="reprocess", id=i, dry_run=FALSE))
    expect_that(file.exists(output_filename("trial", "reprocess", i)), is_true())
  }
})

test_that("Add parameters to experiment", {
  pars <- expand.grid(a=11:20, b=runif(10))
  id <- add_parameters("trial", pars)
  expect_that(id, equals(101:200))

  pars <- expand.grid(a=11:20, c=runif(10))
  expect_that(add_parameters("trial", pars),
              throws_error("New parameters must have column names:"))
})

test_that("pbs workflow", {
  unlink("pbs", recursive=TRUE)
  copy_runner()
  options(experimentr.email="rich.fitzjohn@gmail.com")
  dir.create("pbs")
  files <- make_pbs_file("trial", "testing", 1:100, path="pbs", queue="express")
  qsub(files)
})

test_that("misc regression tests", {
  ## Same mock as before:
  unlink("experiments", recursive=TRUE)
  experimentr::create_dirs()
  pars <- expand.grid(a=1:10, b=runif(10))
  setup_experiment("trial", pars, scripts="simulation.R",
                   packages=c("utils", "MASS"),
                   metadata="metadata_hook",
                   overwrite=TRUE)
  add_task("trial", "testing", "target_fn")
  add_task("trial", "reprocess", "reprocess_fn",
           depends=list(dat="testing"))


  expect_that(experiments(), equals("trial"))
  expect_that(sort(tasks("trial")),
              equals(sort(c("testing", "reprocess"))))
  expect_that(ids("trial"), equals(1:100))
  expect_that(ids_incomplete("trial", "testing"), equals(1:100))

  yml <- yaml::yaml.load_file(experiments_filename())
  expect_that(get_experiment("foo", yml),
              throws_error("Did not find experiment within overall data"))
  expect_that(get_task("trial", "foo", yml),
              throws_error("Did not find task within experiment"))

  expect_that(ids_incomplete("asdf", "testing"),
              throws_error("Did not find experiment"))
  expect_that(ids_incomplete("trial", "asdfa"),
              throws_error("Did not find task"))
})

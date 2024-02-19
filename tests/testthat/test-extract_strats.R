test_that("extract_strats works", {
  skip_on_ci()
  skip_on_cran()

  # setup
  fitme <- coxme::coxme(
    survival::Surv(tte, status_loa) ~ (1|center),
    covariate_file  # see test_path("setup.R")
  )

  db_wrong_gt2 <- covariate_file
  fit_wrong_gt2 <- fitme

  db_wrong_2_multiple <- covariate_file
  fit_wrong_2_multiple <- fitme

  # tests

  expect_numeric(
    extract_strats(fitme, covariate_file)
  )

  expect_error(
    extract_strats(fit_wrong_gt2, db_wrong_gt2),
    "do not include stratum/covariates with name strata"
  )

  expect_error(
    extract_strats(fit_wrong_2_multiple, db_wrong_2_multiple),
    "please include the strata once in the data frame"
  )



})

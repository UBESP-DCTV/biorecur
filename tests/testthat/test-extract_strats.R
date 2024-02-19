test_that("extract_strats works", {
  skip_on_ci()
  skip_on_cran()

  # setup
  fitme <- coxme::coxme(
    survival::Surv(tte, status_loa) ~ (1|center),
    covariate_file  # see test_path("setup.R")
  )

  wrong_db1 <- cbind(
    covariate_file,
    strata = rep(1:2, nrow(covariate_file))
  )
})

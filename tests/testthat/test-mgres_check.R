test_that("mgres_check works", {

  # setup
  data <- tibble::tibble(
    time = c(1,2,3,4),
    event =  c(1,0,1,0),
    subject = c(1,2,3,4),
    strata = c(1,1,2,2)
  )

  fitme <- coxme(
    Surv(time, event) ~ (1|subject) + strata(strata),
    data = data
  )

  # Exectue
  fine_coxme <- mgres_check(fitme, data)

  # Test
  expect_true(fine_coxme)
})

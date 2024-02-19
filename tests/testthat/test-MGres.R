test_that("mgres works for coxme and coxph", {
  # setup
  db <- coxme::eortc
  db[["subject"]] <- seq_len(nrow(db))

  fitme_coxph <- survival::coxph(
    survival::Surv(y, uncens) ~ trt,
    db
  )

  fitme_coxme <- coxme::coxme(
    survival::Surv(y, uncens) ~ trt + (1|center),
    db
  )

  # test
  mgres(fitme_coxme, db) |>
    expect_snapshot()

  mgres(fitme_coxph, db) |>
    expect_snapshot()
})

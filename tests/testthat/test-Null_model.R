test_that("null_model works", {
  # setup
  db <- coxme::eortc
  db[["subject"]] <- seq_len(nrow(db))

  fitme <- coxme::coxme(
    survival::Surv(y, uncens) ~ trt + (1|center),
    db
  )

  # test
  null_model(
    fitme = fitme,
    data = db,
    IDs = db[["subject"]],
    range = c(-1, 1),
    length.out = 50
  ) |>
    expect_snapshot()
})

test_that("spare.bed works", {
  skip_on_ci()
  skip_on_cran()

  # setup
  fitme <- coxme::coxme(
    survival::Surv(tte, status_loa) ~ (1|center),
    covariate_file  # see test_path("setup.R")
  )

  nm <- null_model(
    fitme = fitme,
    data = covariate_file,
    IDs = covariate_file[["subject"]],
    range = c(-1, 1),
    length.out = 50,
    verbose = FALSE
  )

  out_file <- fs::file_temp("spare_test", ext = ".txt")

  # execute
  SPARE.bed(
    test_path("data_test/omni_mind_geno_clean3_ridotto_prova_07_02_2024"),
    covariate_file[["subject"]],
    nm,
    out_file,
    verbose = FALSE
  )


  # test
  paste0(out_file, ".txt") |>
    rlang::hash_file() |>
    expect_snapshot()

})

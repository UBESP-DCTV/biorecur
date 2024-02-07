# the following code is running at the beginning of every tests.
library(checkmate)

covariate_file <- test_path("data_test/dataset_vedovelli_besta_bosisio.txt") |>
  readr::read_tsv(show_col_types = FALSE) |>
  dplyr::transmute(
    subject = as.character(IID),
    tte = as.numeric(Time_event),
    status_loa = as.numeric(status_LoA) - 1,
    center = forcats::fct(Centro) |> as.integer()
  ) |>
  dplyr::filter(
    !is.na(tte),
    !is.na(status_loa)
  )

extract_strats <- function(fitme, data) {

  strats <- rep(0, dim(data)[1])

  check_strat <- strsplit(
    as.character(fitme$formulaList$fixed)[3],
    "strata"
  )[[1]]

  if (length(check_strat) > 2) {
    stop("do not include stratum/covariates with name strata")
  }

  # In this case, strata have been specified..
  if (length(check_strat) == 2) {
    strata_names <- substring(strsplit(check_strat[2], ")")[[1]][1], 2)

    strats <- data |>
      dplyr::select(dplyr::all_of(strata_names))

    try(
      # questo presuppone che in base R se seleziono una sola colonna di
      # un data frame NON ottengo un data frame ma un vettore e quindi
      # la sua lunghezza è pari al numero di righe del dataset. Se non
      # è così allora vuol dire che ho selezionato più di una colonna
      # e quindi ho messo più di uno strato e non gli piace
      if (length(strats) != dim(data)[1]) {
        stop("please include the strata once in the data frame")
      }
    )
  }

  strats
}

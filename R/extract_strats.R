extract_strats <- function(fitme, data) {

  check_strat <- strsplit(
    as.character(fitme$formulaList$fixed)[3],
    "strata"
  )[[1]]

  if (length(check_strat) > 2) {
    stop("do not include stratum/covariates with name 'strata'.")
  }

  if (length(check_strat) < 2) {
    stop("Strange unexpected things is happening here..")
  }

  strata_names <- substring(strsplit(check_strat[2], ")")[[1]][1], 2)
  checkmate::check_string(strata_names)

  data[[strata_names]]
}

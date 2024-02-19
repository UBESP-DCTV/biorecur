check_input_SPARE <- function(obj.null, Geno.mtx, par.list) {
  if (!inherits(obj.null, "null_model")) {
    stop("obj.null should be a returned outcome from 'null_model'")
  }

  if (is.null(rownames(Geno.mtx))) {
    stop("Row names of 'Geno.mtx' should be given.")
  }
  if (is.null(colnames(Geno.mtx))) {
    stop("Column names of 'Geno.mtx' should be given.")
  }
  if (!is.numeric(Geno.mtx) | !is.matrix(Geno.mtx)) {
    stop("Input 'Geno.mtx' should be a numeric matrix.")
  }

  if (length(intersect(obj.null$IDs, rownames(Geno.mtx))) == 0) {
    stop("None of 'IDs' are included in rownames(Geno.mtx).")
  }
  print(paste0(
    "In total, ",
    length(intersect(obj.null$IDs, rownames(Geno.mtx))),
    " samples with phenotype and genotype information"
  ))

  if (
    !is.numeric(par.list$min.maf) |
    par.list$min.maf < 0 |
    par.list$min.maf > 0.5
  ) {
    stop(
      "Argument 'min.maf' should be a numeric value >= 0 and <= 0.5."
    )
  }
  if (
    !is.numeric(par.list$missing.cutoff) |
    par.list$missing.cutoff < 0 |
    par.list$missing.cutoff > 1
  ) {
    stop(paste0(
      "Argument 'missing.cutoff' should be a numeric value ",
      "between 0 and 1."
    ))
  }
}




check_input <- function(data, IDs, mresid, range) {
  if (is.null(IDs)) {
    stop(paste0(
      "Argument 'IDs' is required in case of potential errors. ",
      "For more information, please refer to 'Details'."
    ))
  }
  IDs <- as.character(IDs)

  if (any(!is.element(IDs, data$subject))) {
    warning("All elements in IDs should be also in data as 'subject'")
  }

  if (anyDuplicated(IDs) != 0) {
    stop("Argument 'IDs' should not have a duplicated element.")
  }

  if (range[2] != -1 * range[1]) {
    stop("range[2] should be -1*range[1]")
  }

  if (length(mresid) != length(IDs)) {
    warning(paste0(
      "length(mresid)!=length(IDs) where mresid are the martingale ",
      "residuals."
    ))
  }
}




mgres_check <- function(fitme, data) {
  if (is.null(fitme)) {
    stop("no coxme object included")
  }
  if (!inherits(fitme, "coxme")) {
    stop("object not of class coxme")
  }
  if (is.null(data)) {
    stop("no data object included")
  }
  if (!inherits(data, "data.frame")) {
    stop("data is not a data frame object")
  }
  if (!"subject" %in% colnames(data)) {
    stop('please include individuals as "subject" in dataframe')
  }

  check_strat <- strsplit(
    as.character(fitme$formulaList$fixed)[3],
    "strata"
  )[[1]]
  if (length(check_strat) > 2) {
    stop("do not include stratum/covariates with name strata")
  }

  # In this case, strata have been specified..
  if (length(check_strat) > 1) {
    name <- substring(strsplit(check_strat[2], ")")[[1]][1], 2)

    strats <- data[, which(colnames(data) == name)]
    try(
      if (length(strats) != dim(data)[1]) {
        stop("please include the strata once in the data frame")
      }
    )
  } else {
    strats <- rep(0, dim(data)[1])
  }
}

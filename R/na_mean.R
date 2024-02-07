na_mean <- function(x, option = "mean", maxgap = Inf) {
  data <- x
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    for (i in seq_len(dim(data)[2])) {
      if (!anyNA(data[, i])) {
        next
      }
      tryCatch(data[, i] <- na_mean(
        data[, i], option,
        maxgap
      ), error = function(cond) {
        warning(paste(
          "imputeTS: No imputation performed for column",
          i, "because of this", cond
        ), call. = FALSE)
      })
    }
    return(data)
  } else {
    missindx <- is.na(data)
    if (!anyNA(data)) {
      return(data)
    }
    if (any(class(data) == "tbl")) {
      data <- as.vector(as.data.frame(data)[, 1])
    }
    if (all(missindx)) {
      stop(paste0(
        "Input data has only NAs. ",
        "Input data needs at least 1 non-NA data point ",
        "for applying na_mean"
      ))
    }
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {
      stop("Wrong input type for parameter x")
    }
    if (!is.null(dim(data)[2])) {
      data <- data[, 1]
    } else if (option == "mean") {
      mean <- mean(data, na.rm = TRUE)
      data[missindx] <- mean
    }
    if (is.finite(maxgap) && maxgap >= 0) {
      rlencoding <- rle(is.na(x))
      rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE
      en <- inverse.rle(rlencoding)
      data[en == TRUE] <- NA
    }
    if (!is.null(dim(x)[2])) {
      x[, 1] <- data
      return(x)
    }
    return(data)
  }
}

MGres_ph <- function(fitph = NULL, data = NULL) {
  if (is.null(fitph)) stop("no coxph object included")
  if (!"coxph" %in% class(fitph)) stop("object not of class coxph")
  if (is.null(data)) stop("no data object included")
  if (!"subject" %in% colnames(data)) {
    stop("please include individuals as \"subject\" in dataframe")
  }
  if (length(grep("subject", attr(fitph$terms, "term.labels"))) < 1) {
    warning("subject not included as frailty")
  }

  if (!is.null(fitph$na.action)) data <- data[-fitph$na.action, ]
  mgres <- rowsum(fitph$residuals, data$subject)
  mgres <- mgres[match(unique(data$subject), rownames(mgres)), ]

  count <- rep(0, length(unique(data$subject)))
  for (i in seq_along(count)) {
    count[i] <- sum(
      as.data.frame(as.matrix(fitph$y))$status[
        which(data$subject == unique(data$subject)[i])
      ]
    )
  }
  cumhaz <- count - mgres
  names(mgres) <- unique(data$subject)
  names(cumhaz) <- unique(data$subject)
  list(
    resids = mgres,
    cumhaz = cumhaz,
    frail = as.numeric(unlist(fitph$history)[1])
  )
}

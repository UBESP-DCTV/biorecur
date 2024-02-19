#' @export
mgres_check <- function(fitme, data) {
  UseMethod("mgres_check", fitme)
}


#' @export
mgres_check.coxme <- function(fitme, data) {
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
}


#' @export
mgres_check.coxph <- function(fitme, data) {
  if (is.null(fitme)) stop("no coxph object included")
  if (!"coxph" %in% class(fitme)) stop("object not of class coxph")
  if (is.null(data)) stop("no data object included")
  if (!"subject" %in% colnames(data)) {
    stop("please include individuals as \"subject\" in dataframe")
  }
  if (length(grep("subject", attr(fitme$terms, "term.labels"))) < 1) {
    warning("subject not included as frailty")
  }

}

mgres_check.default <- function(fitme, data) {
  usethis::ui_stop(stringr::str_c(
    "{usethis::ui_code('mgres_check')} not yet implemented for model ",
    "of class:
    {usethis::ui_value(stringr::str_c(class(fitme), collapse = '/'))}."
  ))
}

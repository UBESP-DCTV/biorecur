#' @export
mgres_check <- function(fitme, data) {
  UseMethod("mgres_check", fitme)
}


#' @export
mgres_check.coxme <- function(fitme, data) {
  checkmate::check_data_frame(data)
  if (!"subject" %in% colnames(data)) {
    stop('please include individuals as "subject" in dataframe')
  }
  invisible(TRUE)
}


#' @export
mgres_check.coxph <- function(fitme, data) {
  if (!"subject" %in% colnames(data)) {
    stop('please include individuals as "subject" in dataframe')
  }
  if (length(grep("subject", attr(fitme$terms, "term.labels"))) < 1) {
    warning("subject not included as frailty")
  }
  invisible(TRUE)
}

mgres_check.default <- function(fitme, data) {
  usethis::ui_stop(stringr::str_c(
    "{usethis::ui_code('mgres_check')} not yet implemented for model ",
    "of class:
    {usethis::ui_value(stringr::str_c(class(fitme), collapse = '/'))}."
  ))
}

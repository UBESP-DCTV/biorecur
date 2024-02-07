#' null_model
#'
#' @param fitme (coxme) frailty survival model
#' @param data (data.frame) clinical characteristics, with a mandatory
#'   column `subject` reporting subjects' IDs.
#' @param IDs (chr, default = NULL) character vector of unique IDs
#'   reported in the column `subject` of `data`. If NULL (default) an
#'   error will be thrown.
#' @param range (num, default = c(-20, 20)) reference symmetric interval
#'   for martingale in the form `c(-n, n)`.
#' @param length.out (int, default = 50000) number of iterations for the
#'   martingale.
#' @param seed (int, default = 42) seed for the simulation
#'
#' @return (null_model) the null model containing the formatted
#'   parameters coming from the `{coxme}` model.
#' @export
#'
#' @examples
#' library(coxme)
#'
#' db <- eortc
#' db[["subject"]] <- seq_len(nrow(db))
#'
#' fitme <- coxme(
#'   Surv(y, uncens) ~ trt + (1|center),
#'   db
#' )
#' nm <- null_model(
#'   fitme = fitme,
#'   data = db,
#'   IDs = db[["subject"]],
#'   range = c(-10, 10),
#'   length.out = 5000
#' )
#' str(nm, 1)
null_model <- function(
    fitme,
    data,
    IDs = NULL,
    range = c(-20, 20),
    length.out = 50000,
    seed = 42
) {
  set.seed(seed)
  Call <- match.call()

  # Compute martingale residuals and cumulative hazards
  if ("coxme" %in% class(fitme)) {
    MG <- MGres(fitme, data)

    mresid <- MG$resids
    cumhaz <- MG$cumhaz
    frail <- MG$frail
  }
  if ("coxph" %in% class(fitme)) {
    MG <- MGres_ph(fitme, data)

    mresid <- MG$resids
    cumhaz <- MG$cumhaz
    frail <- MG$frail
  }

  ### Check input arguments
  obj.check <- check_input(data, IDs, mresid, range)

  ### Calculate empirical CGF for martingale residuals
  idx0 <- stats::qcauchy(seq_len(length.out) / (length.out + 1))
  idx1 <- idx0 * max(range) / max(idx0)

  cumul <- NULL
  print("Compute empirical CGF for martingale residuals...")
  c <- 0
  for (i in idx1) {
    c <- c + 1
    t <- i
    e_resid <- exp(mresid * t)
    M0 <- mean(e_resid)
    M1 <- mean(mresid * e_resid)
    M2 <- mean(mresid^2 * e_resid)
    K0 <- log(M0)
    K1 <- M1 / M0
    K2 <- (M0 * M2 - M1^2) / M0^2
    cumul <- rbind(cumul, c(t, K0, K1, K2))
    if (c %% 5000 == 0) {
      print(paste0("Complete ", c, "/", length.out, "."))
    }
  }

  structure(
    list(
      resid = mresid,
      cumhaz = cumhaz,
      frail = frail,
      cumul = cumul,
      Call = Call,
      IDs = IDs
    ),
    class = "null_model"
  )
}

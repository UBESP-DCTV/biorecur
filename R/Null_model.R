Null_model <- function(fitme,
                       data,
                       IDs = NULL,
                       range = c(-20, 20),
                       length.out = 50000) {
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

  K_org_emp <- stats::approxfun(cumul[, 1], cumul[, 2], rule = 2)
  K_1_emp <- stats::approxfun(cumul[, 1], cumul[, 3], rule = 2)
  K_2_emp <- stats::approxfun(cumul[, 1], cumul[, 4], rule = 2)

  structure(
    list(
      resid = mresid,
      cumhaz = cumhaz,
      frail = frail,
      K_org_emp = K_org_emp,
      K_1_emp = K_1_emp,
      K_2_emp = K_2_emp,
      Call = Call,
      IDs = IDs
    ),
    class = "NULL_Model"
  )
}

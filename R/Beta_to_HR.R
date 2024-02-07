Beta_to_HR <- function(beta, g, resids, cumhaz, frail) {
  constant <- sum(g^2 * cumhaz / (frail * cumhaz + 1)) / sum(g^2)
  HR <- beta / constant
  return(HR)
}

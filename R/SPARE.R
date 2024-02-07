SPARE <- function(
  obj.null,
  Geno.mtx,
  missing.cutoff = 0.05,
  min.maf = 0.05,
  p.cutoff = 0.001
) {
  par.list <- list(
    pwd = getwd(),
    sessionInfo = utils::sessionInfo(),
    missing.cutoff = missing.cutoff,
    min.maf = min.maf
  )

  ### Check input
  check_input_SPARE(obj.null, Geno.mtx, par.list)

  print(paste0("Sample size is ", nrow(Geno.mtx), "."))
  print(paste0("Number of variants is ", ncol(Geno.mtx), "."))

  IDs <- rownames(Geno.mtx)
  SNPs <- colnames(Geno.mtx)

  #
  ### -------------- Quality Control ---------------
  #

  ### Only select genotypes that have a phenotype
  mresid <- obj.null$resid
  Complete <- intersect(names(mresid), IDs)
  Geno.mtx <- Geno.mtx[which(rownames(Geno.mtx) %in% Complete), ]

  ### Filter on call rate, minor allele frequency, and match entries of
  ### genotype and phenotype
  # for plink input
  if (any(Geno.mtx == -9)) Geno.mtx[Geno.mtx == -9] <- NA

  SNP.callrate <- colMeans(is.na(Geno.mtx))
  if (any(SNP.callrate > missing.cutoff)) {
    Geno.mtx <- Geno.mtx[, -which(SNP.callrate > missing.cutoff)]
  }

  No_Geno <- is.na(Geno.mtx)
  # Impute missing values to mean for estimating MAF
  Geno.mtx <- na_mean(Geno.mtx)

  print(paste0(
    dim(Geno.mtx)[2],
    "SNPs remaining after call threshold ",
    Sys.time()
  ))

  G <- Geno.mtx[, which(
    colMeans(Geno.mtx) > 2 * min.maf &
      colMeans(Geno.mtx) < 2 * (1 - min.maf)
  )]
  No_Geno <- No_Geno[, which(
    colMeans(Geno.mtx) > 2 * min.maf &
      colMeans(Geno.mtx) < 2 * (1 - min.maf)
  )] # Also update the No_geno matrix

  if (is.null(dim(G))) {
    return(NULL)
  } # No SNPs left after QC
  if (dim(G)[2] == 0) {
    return(NULL)
  } # No SNPs left after QC

  mresid <- mresid[which(names(mresid) %in% Complete)]
  MG <- mresid[match(rownames(G), names(mresid))]

  ### Perform Linear regression for all SNPs simultaneously using matrix
  ### multiplication

  print(paste0("SPARE started at ", Sys.time()))

  ### Define outcome Y and intercept X
  n <- dim(G)[1]
  X <- as.matrix(rep(1, dim(G)[1]), nrow = dim(G)[1], ncol = 1)
  Y <- as.matrix(MG)

  ### Scale genotypes to have mean 0 and compute slopes
  Str <- G - as.matrix(X %*% colMeans(G))
  if (sum(No_Geno) > 0) Str[No_Geno] <- 0
  b <- as.numeric(crossprod(Y, Str)) / as.numeric(colSums(Str^2))

  ### Compute P values in SPARE
  S_sq <- colSums(Str^2)
  RSS <- crossprod(Y^2, !No_Geno) - b^2 * S_sq
  sigma_hat <- RSS / (n - 2)
  error <- sqrt(sigma_hat / S_sq)
  pval.mg <- as.numeric(2 * stats::pnorm(-abs(b / error)))

  ### Prepare the output file
  b <- as.numeric(b)
  error <- as.numeric(error)

  # Compute approximate Hazard Ratio
  HR <- rep(NA, length(b))
  for (i in seq_along(HR)) {
    HR[i] <- Beta_to_HR(
      b[i], G[, i], mresid, obj.null$cumhaz, obj.null$frail
    )
  }

  outcome <- cbind(
    SNP = colnames(G),
    MAF = colMeans(G) / 2,
    Missing = colSums(is.na(G)),
    pSPA = pval.mg,
    pMG = pval.mg,
    log_HR_approx = HR,
    Beta = b,
    SE = error,
    Z = b / error
  )
  outcome <- as.data.frame(outcome, stringsAsFactors = FALSE)
  outcome <- transform(
    outcome,
    Beta = as.numeric(Beta),
    log_HR_approx = as.numeric(log_HR_approx),
    pMG = as.numeric(pMG),
    pSPA = as.numeric(pSPA),
    SE = as.numeric(SE),
    Z = as.numeric(Z),
    MAF = as.numeric(MAF),
    Missing = as.integer(Missing)
  )
  rownames(outcome) <- NULL
  low.p <- which(pval.mg < p.cutoff)

  # Perform SPA for SNPs with low P values
  for (i in low.p) {
    g <- G[, i]
    g <- g - mean(g)
    s <- sum(g * MG) / sum(g^2)
    k0 <- function(x) {
      sum(k_emp(obj.null, g, x, "org"))
    }
    k1 <- function(x) {
      sum(g / sum(g^2) * k_emp(obj.null, g, x, "1"))
    }
    k2 <- function(x) {
      sum((g / sum(g^2))^2 * k_emp(obj.null, g, x, "2"))
    }
    get_p <- function(s, tail) {
      k1_root <- function(x) k1(x) - s

      zeta <- tryCatch(
        # First try domain (-2000,2000), then (-20000,20000), otherwise
        # return p values NA
        stats::uniroot(k1_root, c(-2000, 2000))$root,
        error = function(e) {
          result <- tryCatch(
            stats::uniroot(k1_root, c(-20000, 20000))$root,
            error = function(e2) {
              # Set zeta to NA
              NA
            }
          )
          # Check if an error occurred in the second attempt
          if (!inherits(result, "error")) result
        }
      )

      w <- sign(zeta) * sqrt(2 * (zeta * s - k0(zeta)))
      v <- zeta * sqrt(k2(zeta))
      pval <- stats::pnorm(w + 1 / w * log(v / w), lower.tail = tail)
      return(pval)
    }
    p_SPA <- get_p(abs(s), tail = FALSE) + get_p(-abs(s), tail = TRUE)
    outcome$pSPA[i] <- p_SPA
  }

  # Compute SPA - corrected standard errors. Effects have to be slightly
  # way from the null to avoid dividing by 0
  SNP_set <- which(outcome$pMG < 0.75)
  SE2 <- outcome$SE
  SE2[SNP_set] <- sqrt(
    outcome$Beta[SNP_set]^2 /
      stats::qchisq(outcome$pSPA[SNP_set], df = 1, lower.tail = FALSE)
  )

  # Compute (SPA - corrected) standard errors for the approximate HRs
  log_HR_approx_SE <- outcome$SE * (
    outcome$log_HR_approx / outcome$Beta
  )
  log_HR_approx_SE2 <- SE2 * (outcome$log_HR_approx / outcome$Beta)

  outcome <- cbind(outcome, SE2, log_HR_approx_SE, log_HR_approx_SE2)

  print(paste0("SPARE completed at ", Sys.time()))
  outcome
}


k_emp <- function(null_model, g, x, y = c("org", "1", "2")) {
  y <- match.arg(y)
  y <- switch(y,
    "org" = 2,
    "1" = 3,
    "2" = 4
  )
  cumul <- null_model[["cumul"]]
  f <- stats::approxfun(cumul[, 1], cumul[, y], rule = 2)
  f(g / sum(g^2) * x)

}

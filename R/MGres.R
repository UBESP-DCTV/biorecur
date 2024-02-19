#' @export
mgres <- function(fitme = NULL, data = NULL) {
  UseMethod("mgres", fitme)
}



#' @export
mgres.coxme <- function(fitme = NULL, data = NULL) {
  mgres_check(fitme, data)

  ### Find the strata included in the coxme fit
  check_strat <- strsplit(
    as.character(fitme$formulaList$fixed)[3],
    "strata"
  )[[1]]

  if (length(check_strat) > 1) {
    name <- strsplit(check_strat[2], ")")[[1]][1] |>
      substring(2)

    strats <- data[, colnames(data) == name]
  } else {
    strats <- rep(0, dim(data)[1])
  }
  strat_list <- unique(strats)

  cumhaz <- rep(0, length(unique(data$subject)))

  ### Compute baseline hazards for all strata
  events <- as.data.frame(as.matrix(fitme$y))

  for (strat in strat_list) {

    ### Distinguish models where 'start' time of risk interval is
    ### specified
    if (dim(events)[2] > 2) {
      basehaz <- cbind(
        rep(0, length = length(unique(events$stop))),
        unique(events$stop)
      ) |>
        as.data.frame()

      colnames(basehaz) <- c("Haz", "Time")
      eventtimes <- events$stop

      for (i in seq_along(unique(events$stop))) {
        time <- basehaz$Time[i]
        risk.set <- which(
          events$start < time &
            events$stop >= time &
            strats == strat
        )
        denom <- sum(exp(fitme$linear.predictor[risk.set]))
        nom <- sum(
          events$stop == time & events$status == 1 & strats == strat
        )
        basehaz$Haz[i] <- nom / denom
      }
    } else {
      basehaz <- as.data.frame(cbind(
        rep(0, length = length(unique(events$time))),
        unique(events$time)
      ))
      colnames(basehaz) <- c("Haz", "Time")
      eventtimes <- events$time
      for (i in seq_along(unique(events$time))) {
        time <- basehaz$Time[i]
        risk.set <- which(events$time >= time & strats == strat)
        denom <- sum(exp(fitme$linear.predictor[risk.set]))
        nom <- sum(
          events$time == time &
            events$status == 1 &
            strats == strat
        )
        basehaz$Haz[i] <- nom / denom
      }
    }

    ### Compute individual cumulative hazards from baseline hazard and
    ### linear predictor
    for (i in seq_along(unique(data$subject))) {
      rows <- which(
        data$subject == unique(data$subject)[i] &
          strats == strat
      )
      if (length(rows) > 0) {
        for (r in seq_along(rows)) {
          if (dim(events)[2] > 2) {
            haz <- sum(basehaz$Haz[which(
              events$start[rows[r]] < basehaz$Time &
                basehaz$Time <= events$stop[rows[r]]
            )])
            cumhaz[i] <- cumhaz[i] +
              haz * exp(fitme$linear.predictor[rows[r]])
          } else {
            haz <- sum(
              basehaz$Haz[which(basehaz$Time <= events$time[rows[r]])]
            )
            cumhaz[i] <- cumhaz[i] +
              haz * exp(fitme$linear.predictor[rows[r]])
          }
        }
      }
    }
  }
  individual_prop_haz <- rep(0, length(unique(data$subject)))
  count <- rep(0, length(unique(data$subject)))
  for (i in seq_along(unique(data$subject))) {
    set <- which(data$subject == unique(data$subject)[i])
    count[i] <- sum(events$status[set] == 1)
  }
  resids <- count - cumhaz

  names(resids) <- unique(data$subject)
  names(cumhaz) <- unique(data$subject)

  list(
    resids = resids,
    cumhaz = cumhaz,
    frail = as.numeric(fitme$vcoef)
  )
}



#' @export
mgres.coxph <- function(fitph = NULL, data = NULL) {

  mgres_check(fitph, data)

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


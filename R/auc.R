#' Calculate the AUC under the curve
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#'
#' @return Returns the calculated AUC
#' @export
#'
#' @examples
#' 2+2
auc <- function(value, time) {
  order <- order(time)

  time  <- time[order]
  value <- value[order]

  diffs <- diff(time) / 2
  pairs <- value[1:(length(value) - 1)] + value[2:(length(value))]

  auc <- sum(diffs * pairs)
  return(auc)
}

#' Calculate the AUC under the moment curve
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#'
#' @return Returns the calculated Area under the curve
#' @export
#'
#' @examples
#' 2+2
aumc <- function(value, time) {
  order <- order(time)

  time  <- time[order]
  value <- value[order]

  diffs <- diff(time) / 2

  pairs <- value[1:(length(value) - 1)] *  time[1:(length(time) - 1)] +
           value[2:(length(value))] * time[2:(length(time))]

  aumc <- sum(diffs * pairs)
  return(aumc)
}


#' Predic the concentration based on a tail fil
#'
#' @param fit A tail fit based on either observed data or censored data
#' @param time the time for which the estimation is made.
#'
#' @return The estimated concentration
#' @export
#'
#' @examples
#' 2+2
predictConc <- function(fit, time){
  l_val <- predict(fit,
                   new = data.frame(time = time))

  return(exp(l_val))
}


#' Area under the curve based on a tail fit
#'
#' @param fit A tail fit based on either observed data or censored data
#' @param from The time from which the area is calculated
#' @param to The end of an interval where the area is calculated
#'
#' @return The fitted area
#' @export
#'
#' @examples
#' 2+2
aucFit <- function(fit, from, to = Inf){

  #calculate t_half
  t_half <- tHalf(fit)

  # Get lambda_z
  lambda_z <- t_half$lambda_z

  # Get the predicted concentration on log scale
  predicted_conc_1  <- predictConc(fit, from)

  # calculate the predicted area
  auc_tz_t <- predicted_conc_1/lambda_z

  # subtract the area specified by to
  if (to != Inf) {
    predicted_conc_2 <- predictConc(fit, to)
    auc_tz_t <- auc_tz_t - predicted_conc_2/lambda_z
  }

  # return the AUC
  return(auc_tz_t)
}

#' Area under the moment curve based on a tail fit
#'
#' @param fit A tail fit based on either observed data or censored data
#' @param from The start of the interval
#' @param to The end the interval, deafults to Inf
#'
#' @return The fitted area under the moment curve
#' @export
#'
#' @examples
#' 2+2
aumcFit <- function(fit, from, to = Inf){

  #calculate t_half
  t_half   <- tHalf(fit)

  # Get lambda_z
  lambda_z <- t_half$lambda_z

  # Get the predicted concentration on log scale
  predicted_conc_1  <- predictConc(fit, from)

  # calculate the predicted area
  aumc_tz_t <-
    from * (predicted_conc_1 / lambda_z) + predicted_conc_1 / (lambda_z^2)


  # subtract the area specified by to
  if (to != Inf) {
    predicted_conc_2 <- predictConc(fit, to)
    aumc_tz_t <- aumc_tz_t <-
      from * (predicted_conc_2 / lambda_z) + predicted_conc_2 / (lambda_z^2)
  }

  # return the AUC
  return(aumc_tz_t)
}


#' Area under a curve for specified interval
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#' @param from The start of the interval
#' @param to The end of the interval, defaults to Inf
#' @param type The type of area to be calculated auc or aumc
#' @param fit A tail fit. if not provided it will be fit on the entire interval
#' @param predict If the end of the are is after Tz, the last quantifiable dose,
#'   should the area be calculated based on the tail fit
#' @param na.rm Should NA values be removed.
#' @param linear_interpol Should linear interpolation be used when the points
#'   specified in from and to is not included in time
#'
#' @return The fitted area
#' @export
#'
#' @examples
#' 2+2
aucInt <- function(value, time, from = 0, to = Inf, type = c("auc", "aumc"),
                   fit = tailFit(value, time), predict = TRUE, na.rm = TRUE,
                   linear_interpol = TRUE) {

  type <- match.arg(type)

  if (na.rm) {
    time  <- time[!is.na(value)]
    value <- value[!is.na(value)]
  }

  # Get first dose

  first_dose <- min(time[!is.na(value)])
  # Get Tz
  tz <- max(time[!is.na(value)])

  if (!from %in% time && from > first_dose & linear_interpol) {
    time <- c(time, from)
    value <- c(value, NaN)
    value <- linearIntPol(value, time)
  }

  if (to != Inf && !to %in% time && to < tz & linear_interpol) {
    time <- c(time, to)
    value <- c(value, NaN)
    value <- linearIntPol(value, time)
  }

  tz_points <- from <= time & time <= tz & time <= to


  auc_0_tz <- switch(type,
                     auc   = auc(value = value[tz_points], time = time[tz_points]),
                     aumc  = aumc(value = value[tz_points], time = time[tz_points]))


  if (to > tz) {
    if (predict) {
      auc_tz_t <- switch(type,
                        auc  = aucFit(fit = fit, from = tz, to = to),
                        aumc = aumcFit(fit = fit, from = tz, to = to))
    } else {
      warning(paste0("tz is equal to ", round(tz),  " < ", to, ", however predict is FALSE"))
      auc_tz_t = 0
    }
  } else {
    auc_tz_t = 0
  }

  auc <- auc_0_tz + auc_tz_t

  return(auc)
}


#' Do linear interpolation between points
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#'
#' @return The variable given to value with missing points filled in by linear interpolation
#' @export
#'
#' @examples
#' 2+2
#' @importFrom stats approx
linearIntPol <- function(value, time){
  missing <- time[is.na(value)]

  int <- approx(time[!is.na(value)], value[!is.na(value)], xout = missing)

  value[is.na(value)] <- int$y
  value
}



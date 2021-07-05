#' Fit the tail for uncensored data
#'
#' Fit a linear model for the log-linear part of the PK-curve
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#'
#' @return The fit by lm
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tail_fit <- tailFit(aval, atpt1n)
#' @importFrom stats lm
tailFit <- function(value, time) {
  l_value <- log(value)
  l_value[value <= 0] <- NaN
  #time    <- time[value > 0]

  fit <- stats::lm(l_value ~ time)

  attr(fit, "fitted_data") <- data.frame(time, value, l_value)

  class(fit) <- c("tailFit", class(fit))
  return(fit)
}

#' Fit the tail for uncensored data
#'
#' Fit a linear model for the log-linear part of the PK-curve
#'
#' @param lower The lower limit of the PK measurements on original scale
#' @param upper The upper limit of the PK measurements on original scale
#' @param time The time variable
#' @param ... additional arguments to survreg
#' @param maxiter The maximum number of iterations
#'
#' @return A fit by survreg
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tail_fit_cens <- tailFitCens(lower, upper, atpt1n)
#' @importFrom survival Surv survreg
tailFitCens <- function(lower, upper, time, maxiter = 1000, ...) {

  l_lower <- log(lower)
  l_lower[lower == 0] <- NA

  l_upper <- log(upper)
  l_upper[upper == 0] <- NA
  
  
  
  
  fit <- 
    tryCatch({
      survival::survreg(survival::Surv(time = l_lower, time2=l_upper, type = "interval2") ~ time,
                        dist = "gaussian", maxiter = maxiter, x = TRUE)
    }, warning = function(w) {
      
      if (any(upper != lower) && sum(upper == lower) >= 3) {
        init_fit <- lm(l_upper[upper == lower] ~ time[upper == lower])
        init <- coef(init_fit)
        tryCatch({
          survival::survreg(survival::Surv(time = l_lower, time2=l_upper, type = "interval2") ~ time,
                            dist = "gaussian", maxiter = maxiter, init = init, x = TRUE)
        }, 
        warning = function(w) {
          fit <- survival::survreg(survival::Surv(time = l_lower, time2=l_upper, type = "interval2") ~ time,
                            dist = "gaussian", maxiter = maxiter, init = init, x = TRUE)
          attr(fit, "status") <- w$message
          fit
        },
        error = function(e) {stop(e$message)}) 
      } else {
          fit <- survival::survreg(survival::Surv(time = l_lower, time2=l_upper, type = "interval2") ~ time,
                                   dist = "gaussian", maxiter = maxiter, x = TRUE)
          attr(fit, "status") <- w$message
          fit
      }
    }, 
    error = function(e) { stop(e$message) }
  ) 

  if (!"status" %in% names(attributes(fit))) attr(fit, "status") <- "succes"
  
  attr(fit, "fitted_data") <- data.frame(time, lower, l_lower, upper, l_upper)
  
  class(fit) <- c("tailFitCens", class(fit))
  return(fit)
}


#' Calculate t-half based on a fit
#'
#' @param fit A tail fit based on either observed data or censored data
#'
#' @return t-half based on a tail fit
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tail_fit <- tailFit(lower, atpt1n)
#' tHalf(tail_fit)
#'
#' best_points <- bestTailFit(lower, atpt1n)$points
#' best_tail_fit <- tailFit(lower[best_points], atpt1n[best_points])
#'
#' tHalf(best_tail_fit)
#'
#' # Fit the tail based on the censored data
#' tail_fit_cens <- tailFitCens(lower, upper, atpt1n)
#' tHalf(tail_fit_cens)
#' @importFrom stats coef
tHalf <- function(fit) {
  lambda_z <- unname(-1*stats::coef(fit)[2])
  t_half   <- log(2)/(lambda_z)
  out      <- tibble(lambda_z = lambda_z, t_half = t_half)

  return(out)
}



#' Calculate R-squared for a tail fit
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#'
#' @return The R-squared and adjusted R-sqared
#' @export
#'
#' @examples
#'
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tailFitRsq(aval, atpt1n)
tailFitRsq <- function(value, time){

  l_value <- log(value)
  l_value[value == 0] <- NaN

  n <- sum(!is.na(l_value))

  m_time <- mean(time)
  m_valu <- mean(l_value)
  Sxx <- sum((time - m_time) * (time - m_time))
  Sxy <- sum((time - m_time) * (l_value - m_valu))
  Syy <- sum((l_value - m_valu) * (l_value - m_valu))
  b1  <- Sxy/Sxx

  r2     <- b1 * Sxy/Syy
  r2_adj <- 1 - (1 - r2) * (n - 1)/(n - 2)

  out <- c(r2 = r2, r2_adj = r2_adj)
  return(out)
}

#' Get R2 from a fitted model
#'
#' @param fit A fitted linear model
#'
#' @return R-squared for the fitted model
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tail_fit <- tailFit(aval, atpt1n)
#' tailFitRsquare.fit(tail_fit)
tailFitRsquare.fit <- function(fit){
  r   <- fit$residuals
  f   <- fit$fitted.values
  n   <- length(r)

  mss <- sum((f - mean(f))^2)
  rss <- sum(r^2)

  r2 <- mss/(mss + rss)
  adj.r2 <-  1 - (1 - r2) * (n - 1)/(n - 2)

  c(R2 = r2, R2_adj = adj.r2)
}


#' Get R2 for censored survival times
#'
#' @param lower The lower limit of the PK measurements on original scale
#' @param upper The upper limit of the PK measurements on original scale
#' @param time The time variable
#'
#' @return The R-squared for censored data
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 1500)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' tailFitRsq.cens(lower, upper, atpt1n)
#' @importFrom survival survreg Surv survfit
#' @importFrom stats predict lm
tailFitRsq.cens <- function(lower, upper, time){

  l_lower <- log(lower)
  l_lower[lower == 0] <- NA

  l_upper <- log(upper)
  l_upper[upper == 0] <- NA

  n <- length(l_lower)

  y <- survival::Surv(time = l_lower, time2 = l_upper, type = "interval2")

  fit <- survival::survreg(y ~ time, dist = "gaussian")

  y.unsorted <- y[, 1]
  censor.unsorted <- y[, 3]
  censor.unsorted[censor.unsorted == 2] <- 0

  delta <- censor.unsorted[order(y.unsorted)]

  fit.km.censoring <- survival::survfit(y ~ 1)
  sum.km.censoring <- summary(fit.km.censoring, times = l_upper, extend = TRUE)

  km.censoring <- sum.km.censoring$surv
  km.censoring.minus <- c(1, km.censoring[-length(km.censoring)])

  ratio.km <- delta/km.censoring.minus
  ratio.km[is.nan(ratio.km)] <- 0
  weight.km <- ratio.km/(sum(ratio.km))

  t.predicted <- predict(fit, newdata = data.frame(l_lower, l_upper, time),
                         type = "response")

  wls.fitted <- lm(l_upper ~ t.predicted, weights = weight.km)

  calibrate.fitted <- predict(wls.fitted)
  num.rho2 <- sum(weight.km * (calibrate.fitted - sum(weight.km *  l_upper))^2)
  denom.rho2 <- sum(weight.km *
                      (l_upper - sum(weight.km * l_upper))^2)

  R2 <- num.rho2/denom.rho2
  r2_adj <-  1 - (1 - R2) * (n - 1)/(n - 2)
  out <- c(r2 = R2, r2_adj = r2_adj)

  return(out)
}


#' Get the points used for best tail fit
#'
#' @param value The PK measurements on original scale
#' @param time The time variable
#' @param min_points The minimum number of used points
#' @param tol The tolerence for best fit. The fit based on most points that
#'   gives an adjusted R2 above the highest observed R2 minus the tolerence is
#'   used
#' @return Returns a list with the number of points used, R-squared, adjusted
#'   R-squared, and the used points
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, NaN)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # Fit the tail
#' bestTailFit(aval[!is.na(aval)], atpt1n[!is.na(aval)])
#'
#' # For censored data the convention in SAS has been to use the lower limits for the value
#' best_tail <- bestTailFit(lower, atpt1n)
#'
#' best_tail
#'
#' @importFrom tibble tibble
bestTailFit <- function(value, time,
                        min_points = 3, tol = 1e-04) {


  first <- which.max(value)
  last  <- sum(value > 0)

  n_points <- rev(min_points:(last - first + 1))

  r2_data <- tibble::tibble(n_points = n_points , r2 = NaN, r2_adj = NaN,
                    points = lapply(first:(last - (min_points - 1)), function(x) x:last))

  for (i in first:(last - (min_points - 1))) {

    j <- i - first + 1

    points   <- i:last
    sub_time <- time[points]
    sub_valu <- value[points]

    r2 <- tailFitRsq(sub_valu, sub_time)
    
    r2_data[j, "r2"] <- r2["r2"]
    r2_data[j, "r2_adj"] <- r2["r2_adj"]
  }

  best_row <- r2_data[r2_data$r2_adj > max(r2_data$r2_adj) - tol, ][1, ]

  tolerance = max(r2_data$r2_adj) - tol

  best_fit <- tailFit(value[unlist(best_row$points)], time[unlist(best_row$points)])

  out <- list(best_fit = best_fit,
              n_points = best_row$n_points,
              points = unlist(best_row$points),
              time_points = time[unlist(best_row$points)],
              r2 = best_row$r2, r2_adj = best_row$r2_adj,
              all_r2 = r2_data, tolerance = tolerance)

  class(out) <- "bestTail"
  return(out)
}

#' Title
#'
#' @param lower The lower limit of the PK measurements on original scale
#' @param upper The upper limit of the PK measurements on original scale
#' @param time The time variable
#' @param min_points The minimum number of used points
#' @param tol The tolerence for best fit. The fit based on most points that
#'
#' @return  A list with the number of points used, R-squared, adjusted
#'   R-squared, and the used points Any censored observation occuring after first
#' # value used in the tail fit is also given in the list of points
#' @export
#'
#' @examples
#' require(ggplot2)
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, NaN)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # For censored data the convention in SAS has been to use the lower
#' # limits for the concentration. Any censored observation occuring after first
#' # value used in the tail fit is also given in the list of points
#' best_tail <- bestTailFitCens(lower, upper, atpt1n)
#'
#' best_tail
#'
#' p <- ggplot(data = best_tail$all_r2, aes(n_points, r2_adj)) + geom_point() +
#'        geom_hline(yintercept = best_tail$tolerance) +
#'        labs(y = "Adjusted R-squared", x = "Number of points")
#' p + geom_point(aes(6, 0.99), colour = 2)
bestTailFitCens <- function(lower, upper, time, min_points = 3, tol = 1e-04) {

  not_na <- !is.na(lower) & lower != 0

  best_tail <- bestTailFit(lower[not_na], time[not_na], min_points = min_points, tol = tol)

  points_adj <- which(time %in% time[not_na][best_tail$points])

  min_time <- time[min(points_adj)]

  best_tail$added_points <- setdiff(which(time > min_time & lower != upper), points_adj)

  best_tail$points <- c(points_adj, which(time > min_time & lower != upper))

  best_tail$time_points <- time[best_tail$points]

  best_tail$best_fit <-
    tailFitCens(lower[best_tail$points], upper[best_tail$points], time[best_tail$points])

  return(best_tail)
}


#' Plot the adjusted R-square for the different tail fits
#'
#' @param x An objects returned from \code{bestTailFitCens} of \code{bestTailFit}
#' @param ... Included to be in compliance with the plot generic
#'
#' @return An ggplot objects
#' @export
#'
#' @examples
#' # Establish the data
#' atpt1n <- c(5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400)
#' aval   <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, NaN)
#' lower  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 0)
#' upper  <- c(59630, 49690, 50130, 45410, 29980, 19650, 7488, 3332, 3000)
#'
#' # For censored data the convention in SAS has been to use the lower
#' # limits for the concentration. Any censored observation occuring after first
#' # value used in the tail fit is also given in the list of points
#' best_tail <- bestTailFitCens(lower, upper, atpt1n)
#'
#' plot(best_tail)
#' @importFrom ggplot2 ggplot geom_point geom_hline labs aes_string
plot.bestTail <- function(x, ...) {
  ggplot(data = x$all_r2, aes_string("n_points", "r2_adj")) + geom_point() +
         geom_hline(yintercept = x$tolerance) +
         labs(y = "Adjusted R-squared", x = "Number of points")
}

globalVariables(c("n_points", "r2_adj"))

#' Plot of pk data
#'
#' @param data Optinal, data set that includes the data to plot
#' @param fit The fitted tail fit
#' @param time The time variable to plot
#' @param value the value to plot
#' @param lower optional, lower limit
#' @param upper optional, upper limin
#' @param log_trans Should the y-axis be log-transformed
#' @param colour colour variable for ggplot
#' @param highlight_points highlighted point
#' @param fit_point_adj should the tail fit be drawn from the first point before
#'   to the first point after the points given in fitted points
#' @param fitted_points optional, The timepoints which the tail fit is based on.
#' @param ... Additional arguments passed on to ggplot aes
#' @param plot_tail Should the tail be plotted
#' @param connect_the_dots should linear interpolation be shown between the dots
#'
#' @return A ggplot with data and tail fit
#' @export
#'
#' @examples
#' require(tibble)
#' adpc_sub <-
#'    tibble(
#'        conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                  66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                  50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'        atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                 3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                 61920),
#'        lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                 69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                 7488, 3332, 0, 0),
#'        upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                  66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                  50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'        tailfl= c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "")          )
#'
#'
#' # Example using bestTailFit
#' best_tail <- bestTailFitCens(adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#'                              adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#'                              adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#' plotTailFit(value = conc, time = atpt1n, data = adpc_sub[!is.na(adpc_sub$conc), ],
#'             fit = best_tail$best_fit, fitted_points = best_tail$time_points,
#'             log_trans = FALSE)
#'
#' plotTailFit(value = conc, time = atpt1n, data = adpc_sub[!is.na(adpc_sub$conc), ],
#'             fit = best_tail$best_fit, fitted_points = best_tail$time_points,
#'             log_trans = TRUE)
#'
#' #plotTailFit(value = conc, time = "atpt1n", lower = lower_conc, upper = upper_conc,
#' #           data = adpc_sub,
#' #             fit = best_tail$best_fit, fitted_points = best_tail$time_points,
#' #             colour = tailfl,
#' #             log_trans = FALSE)
#'
#' #plotTailFit(value = conc, time = atpt1n, lower = lower_conc, upper = upper_conc,
#' #             data = adpc_sub,
#' #           fit = best_tail$best_fit, fitted_points = best_tail$time_points,
#' #           colour = tailfl,
#' #           log_trans = TRUE)
#' #
#'
#' #plotTailFit(value = data$conc, time = data$atpt1n, lower = data$lower_conc,
#' #            upper = data$upper_conc,
#' #            fit = best_tail$best_fit, fitted_points = best_tail$time_points,
#' #             colour = data$tailfl,
#' #             log_trans = TRUE)
#'
#' @importFrom rlang enquo
#' @importFrom ggplot2 ggplot geom_point geom_linerange geom_abline geom_line
#'   stat_function aes aes_string
#' @importFrom glue glue glue_collapse
#' @importFrom utils capture.output
plotTailFit <- function(data = NULL, fit, time, value,
                        lower = NULL, upper = NULL, colour = NULL, ...,
                        log_trans = TRUE, plot_tail = TRUE, connect_the_dots = TRUE,
                        fitted_points = NULL, highlight_points = fitted_points,
                        fit_point_adj = FALSE) {

  # Get slope and intercept from fit
  slope     <- fit$coefficients[2]
  intercept <- fit$coefficients[1]

  # if data is not supplied a dataset is created
  if (is.null(data)) {
    if (!is.null(lower)) {
      data <- tibble::tibble(!! rlang::enquo(time),
                             !! rlang::enquo(value),
                             !! rlang::enquo(lower),
                             !! rlang::enquo(upper))
    } else {

      data <- tibble::tibble(!! rlang::enquo(time),
                             !! rlang::enquo(value),
                             lower = !! rlang::enquo(value),
                             upper = !! rlang::enquo(value))
    }

    if (!is.null(colour)) {
      data[[sub("^.*\\$", "", capture.output(substitute(colour)))]] <- colour
      colour <- as.name(names(data)[5])
    }

    time  <- as.name(names(data)[1])
    value <- as.name(names(data)[2])
    lower <- as.name(names(data)[3])
    upper <- as.name(names(data)[4])
  } else {

    time  <- as.name(substitute(time))
    value <- as.name(substitute(value)) # as.name(value)
    if (!is.null(substitute(lower))) {
      lower <- as.name(substitute(lower))
      upper <- as.name(substitute(upper))
    } else {
      lower <- value
      upper <- value
    }

    needed_vars <- c(time, value, lower, upper)
    if (any(!needed_vars %in% colnames(data))) {
      data_name <- substitute(data)
      missing   <- unique(needed_vars[!needed_vars %in% colnames(data)])
      stop(paste("columns",
             glue::glue_collapse(missing, sep = ", ", last = " and "),
             ifelse(length(missing) > 1, "are", "is"),
             "not available in", capture.output(data_name)))
    }
  }

  min <- min(data[[time]], na.rm = TRUE)
  max <- max(data[[time]], na.rm = TRUE)

  if (!is.null(fitted_points)) {
   # sub_data <- data[data[[time]] %in% fitted_points, ]

    if (fit_point_adj) {
      if (min < min(fitted_points)) {
        fit_min <- max(data[[time]][data[[time]] < min(fitted_points)])
      } else {
        fit_min <- min
      }

      if (max > max(fitted_points)) {
        fit_max <- min(data[[time]][data[[time]] > max(fitted_points)])
      } else {
        fit_max <- max
      }
    } else {
      fit_min <- min(fitted_points)
      fit_max <- max(fitted_points)
    }

  } else {
    fit_min <- min(data[[time]], na.rm = TRUE)
    fit_max <- max(data[[time]], na.rm = TRUE)
  }

  if (!is.null(highlight_points)) {
    sub_data <- data[data[[time]] %in% highlight_points, ]
  }

  if (log_trans) {
    # Get the prediced concentration for the last observation
    c_last <- predictConc(fit, time = max(data[[time]]))

    #
    if (!is.null(upper)) {
      ymin <- log(c_last) - (log(min(data[[upper]])) - log(c_last))

      if (log(min(data[[value]]) < ymin)) {
        ymin <- log(min(data[[value]])) -
          (log(min(data[[upper]])) - log(min(data[[value]])))
      }
    } else {
      ymin <- min(log(c_last), log(min(data[[value]])))
    }

    data$l_lower_lim <- log(data[[lower]])
    data$l_lower_lim[data[[lower]] != data[[upper]]] <- ymin
    data$l_upper <- log(data[[upper]])

    fun.lin <- function(x) slope * x + intercept

    # The actual plot on log scale
    p <- ggplot(data = data, aes(!! rlang::enquo(time), log(!! rlang::enquo(value)), colour = !! rlang::enquo(colour), ...))
    if (connect_the_dots) p <- p + geom_line(size = 1, show.legend = FALSE)
    if (plot_tail)
      p <- p +  stat_function(fun = fun.lin, size = 1, xlim = c(fit_min, fit_max), na.rm = TRUE, show.legend = FALSE)
    if (!is.null(highlight_points)) {
      p <- p + geom_point(mapping = aes(!! rlang::enquo(time), log(!! rlang::enquo(value))),
                 data = sub_data, colour = 2, na.rm = TRUE, show.legend = FALSE)
    }
    p <- p +
      geom_point(na.rm = TRUE)  +
      geom_linerange(aes_string(ymin = "l_lower_lim", ymax = "l_upper"), size = 1, show.legend = FALSE, na.rm = TRUE)
  } else {
    # Get the fitted tail fit on original scale
    fun.1 <- function(x) exp(slope * x + intercept)

    # The actual plot on original scale
    p <- ggplot(data = data, aes(!! rlang::enquo(time), !! rlang::enquo(value), colour = !! rlang::enquo(colour), ...))
    if (connect_the_dots) p <- p + geom_line(size = 1, show.legend = FALSE)
    if (plot_tail)
      p <- p + stat_function(fun = fun.1, size = 1, xlim = c(fit_min, fit_max), na.rm = TRUE)
    if (!is.null(highlight_points)) {
      p <- p + geom_point(mapping = aes(!! rlang::enquo(time), !! rlang::enquo(value)),
                 data = sub_data, colour = 2, na.rm = TRUE, show.legend = FALSE)
    }
    p <- p +
      geom_point(na.rm = TRUE)  +
      geom_linerange(aes(ymin = !! rlang::enquo(lower), ymax = !! rlang::enquo(upper)), size = 1, show.legend = FALSE, na.rm = TRUE)
  }

  p
}




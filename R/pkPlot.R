#' Inititiate a PK plot
#'
#' @param data \code{data.frame} or tibble containing the data
#' @param time \code{character} with the name of the time column
#' @param value \code{character} with the name of the value column
#' @param log \code{logical} indicating if the dat should be log-transformed
#' @param ... Additional parameters passed to \code{aes_string}
#'
#' @return Initiates the pk plot and returns an object of class ggplot
#' @export
#'
#' @examples
#' require(ggplot2)
#'
#' adpc_sub <-
#'   data.frame(
#'     conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'              66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'              50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'     atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                  3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                  61920),
#'     lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                     69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                     7488, 3332, 0, 0),
#'     upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                    66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                    50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'     tailfl = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""))
#' # Create a tail fit
#' best_tail <- bestTailFitCens(lower = adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#'                              upper = adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#'                              time  = adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#' # Plot the data on original scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc")
#'
#'
#' # Plot the data on log scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", log = TRUE, colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, log = TRUE, from = 96, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n", 
#'               lower = "lower_conc", upper = "upper_conc", log = TRUE)
#'
#' @importFrom ggplot2 ggplot aes_string
pkPlot <- function(data, time, value, log = FALSE, ...) {
  if (log)
    data[[value]] <- log(data[[value]])

  ggplot(data = data, aes_string(time, value, ...))
}

#' Add dots to a pk plot
#'
#' Add dots to a pk plot initiated by \code{pkPlot}
#'
#' @param ... Params supplied to geom_point
#' @param na.rm \code{logical} indicating if NAs should be removed without warning
#'
#' @return An object of class ggplot with dots added
#' @export
#'
#' @examples
#' #' require(ggplot2)
#'
#' adpc_sub <-
#'   data.frame(
#'     conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'              66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'              50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'     atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                  3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                  61920),
#'     lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                     69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                     7488, 3332, 0, 0),
#'     upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                    66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                    50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'     tailfl = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""))
#' # Create a tail fit
#' best_tail <- bestTailFitCens(lower = adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#' upper = adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#' time  = adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#'
#' # Plot the data on original scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc")
#'
#
#  # Plot the data on log scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", log = TRUE, colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, log = TRUE, from = 96, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc", log = TRUE)
#'
#' @importFrom ggplot2 geom_point
pkPlot_dot <- function(..., na.rm = TRUE) {
  geom_point(..., na.rm = na.rm)
}

#' Add tail fit to an existing PK plot
#'
#' @param fit The fitted tail fit
#' @param from \code{numeric} indicating where the curve should be drawn from
#' @param to \code{numeric} indicating where the curve should be drawn to
#' @param log \code{logical} indicating if data should be log transformed
#' @param size \code{numeric} indicating the size of the line is passed on the function stat_function
#' @param na.rm  \code{logical} indicating if it NAs should be removed without warning
#' @param ... Additinal parameters pass on to stat_function
#'
#' @return The ggplot including the tail plot
#' @export
#'
#' @examples
#' #' require(ggplot2)
#'
#' adpc_sub <-
#'   data.frame(
#'     conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'              66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'              50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'     atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                  3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                  61920),
#'     lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                     69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                     7488, 3332, 0, 0),
#'     upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                    66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                    50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'     tailfl = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""))
#' # Create a tail fit
#' best_tail <- bestTailFitCens(lower = adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#' upper = adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#' time  = adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#'
#' # Plot the data on original scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc")
#'
#
#  # Plot the data on log scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", log = TRUE, colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, log = TRUE, from = 96, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc", log = TRUE)
#'
#' @importFrom ggplot2 stat_function
pkPlot_tailFit <- function(fit, from = NULL, to = NULL, log = FALSE,
                           size = 1, na.rm = TRUE, ...) {

  fitted_data <- attr(fit, "fitted_data")

  # Get the min and max from the fit
  if (is.null(from)) from <- min(fitted_data$time)
  if (is.null(to)) to <- max(fitted_data$time)

  # Get slope and intercept from fit
  slope     <- fit$coefficients[2]
  intercept <- fit$coefficients[1]

  # Get the fitted tail fit on original scale
  if (log) {
    fun.1 <- function(x) slope * x + intercept
  } else {
    fun.1 <- function(x) exp(slope * x + intercept)
  }
  p <- stat_function(fun = fun.1, size = size, xlim = c(from, to), na.rm = na.rm, ...)
  
  if (!is.null(fit) && attr(fit, "status") != "succes") {
    p <-  ggplot2::annotate(geom = 'text', label = paste("Tail fit:", attr(fit, "status")),
                                x = -Inf, y = Inf, hjust = 0, vjust = 1, colour = "red")
  } 
  
  return(p)
}

#' Add lloq values to an existing PK plot
#'
#' @param data The data also passed to the function \code{pkPlot}
#' @param time \code{character} supplying the column name of the time variable
#' @param conc \code{character} supplying the column name of the concentration variable
#' @param lower \code{character} supplying the column name of the lower concentration variable
#' @param upper \code{character} supplying the column name of the upper concentration variable
#' @param fit The fitted tail fit
#' @param log \code{logical} indicating if data should be log transformed
#' @param size \code{numeric} indicating the size of the line
#' @param linetype \code{character} indicating the type of the line
#' @param show.legend \code{logical} indicating if legend should be shown
#' @param na.rm  \code{logical} indicating if it NAs should be removed without warning
#' @param ... Additional arguments passe on to aes_string
#'
#' @return The ggplot with segements for the lloq values
#' @export
#'
#' @examples
#' #' require(ggplot2)
#'
#' adpc_sub <-
#'   data.frame(
#'     conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'              66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'              50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'     atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                  3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                  61920),
#'     lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                     69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                     7488, 3332, 0, 0),
#'     upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                    66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                    50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'     tailfl = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""))
#' # Create a tail fit
#' best_tail <- bestTailFitCens(lower = adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#' upper = adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#' time  = adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#'
#' # Plot the data on original scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc")
#'
#
#  # Plot the data on log scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", log = TRUE, colour = "tailfl") +
#'   pkPlot_dot() +
#'   pkPlot_tailFit(best_tail$best_fit, log = TRUE, from = 96, colour = "#3F9C35") +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc", log = TRUE)
#'
#' @importFrom ggplot2 geom_linerange aes_string
pkPlot_lloq <- function(data, time = "atpt1n", conc = "aval" ,lower = "lower_conc",
                        upper = "upper_conc", fit = NULL, log = FALSE,
                        size = 1, linetype = "dashed", show.legend = FALSE, na.rm = TRUE,
                        ...) {

  lloq <- data[(data[[lower]] != data[[upper]] & !is.na(data[[lower]]) & !is.na(data[[upper]]) & !is.na(data[[time]])), ]

  # if no lloq values are observed return NULL
  if (nrow(lloq) == 0)
    return(NULL)

  if (log) {
    lloq$l_upper <- log(lloq[[upper]])
    lloq$l_lower <- ifelse(lloq[[lower]] <= 0, -Inf, log(lloq[[lower]]))

    lloq[, "aval"] <- log(lloq[, "aval"])
    
    # if (!is.null(fit)) {
    #   c_last <- predictConc(fit, time = max(data[[time]]))
    #   lloq$l_lower <- min(lloq$l_lower, log(c_last) - (lloq$l_upper - log(c_last)))
    # }
    #lloq[[conc]] <- log(lloq[[conc]])
    #lloq[[conc]][is.na(lloq[[conc]])] <- lloq$l_lower
  } else {
    lloq$l_lower <- lloq[[lower]]
    lloq$l_upper <- lloq[[upper]]
  }
  
    ggplot2::geom_segment(aes_string(x = time, xend = time, y = "l_upper", yend = "l_lower", ...),
                            data = lloq,
                            size = size, linetype = linetype, show.legend = show.legend, na.rm = na.rm)
}


#' PK line plot
#'
#' Add a line plot to the PK plot
#'
#' @param size \code{numeric} indicating the line size
#' @param show.legend \code{logical} Should the legend be shown
#' @param na.rm  \code{logical} Should NAs be removed without warning
#' @param ... Arguments passed on to geom_line
#'
#' @return The PK pl
#' @export
#' @examples
#' require(ggplot2)
#'
#' adpc_sub <-
#'   data.frame(
#'     conc = c(NA, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'              66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'              50130, 45410, 29980, 19650, 7488, 3332, NA, NA),
#'     atpt1n  =  c(-15, 60, 180, 360, 720, 900, 1080, 1260, 1440, 1620, 1800, 2160, 2520, 2880,
#'                  3600, 4440, 5760, 7200, 8640, 10080, 14400, 20160, 30360, 40320, 50400,
#'                  61920),
#'     lower_conc =  c(0, 13290, 48790, 62740, 81510, 89140, 69410, 71490, 66720, 63170, 65890,
#'                     69520, 66660, 76340, 63220, 69020, 59630, 49690, 50130, 45410, 29980, 19650,
#'                     7488, 3332, 0, 0),
#'     upper_conc = c(3000, 13290, 48790, 62740, 81510, 89140, 69410, 71490,
#'                    66720, 63170, 65890, 69520, 66660, 76340, 63220, 69020, 59630, 49690,
#'                    50130, 45410, 29980, 19650, 7488, 3332, 3000, 3000),
#'     tailfl = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
#'                "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", ""))
#' # Create a tail fit
#' best_tail <- bestTailFitCens(lower = adpc_sub$lower_conc[adpc_sub$tailfl == "Y"],
#' upper = adpc_sub$upper_conc[adpc_sub$tailfl == "Y"],
#' time  = adpc_sub$atpt1n[adpc_sub$tailfl == "Y"])
#'
#'
#' # Plot the data on original scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", colour = "tailfl") +
#'   pkPlot_line() +
#'   pkPlot_tailFit(best_tail$best_fit, colour = "#3F9C35") +
#'   pkPlot_dot() +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc")
#'
#
#'  # Plot the data on log scale
#' pkPlot(adpc_sub, time = "atpt1n", value = "conc", log = TRUE, colour = "tailfl") +
#'   pkPlot_line() +
#'   pkPlot_tailFit(best_tail$best_fit, log = TRUE, from = 96, colour = "#3F9C35") +
#'   pkPlot_dot() +
#'   pkPlot_lloq(data = adpc_sub,  time = "atpt1n",
#'               lower = "lower_conc", upper = "upper_conc", log = TRUE)
#'
#' @importFrom ggplot2 geom_line
pkPlot_line <- function(size = 1, show.legend = FALSE, na.rm = TRUE, ...){
  geom_line(size = size, show.legend = show.legend, na.rm = na.rm, ...)
}



# To avoid NOTEs the R CMD check
utils::globalVariables(".")

#' Create the base plot for the review app
#'
#' @param profile_data The data used
#' @param best_fit should the best fit be found automatically
#' @param best_fit_n The minimum number of points to use
#' @param log should the data be log transformed
#' @param comments data set with comments, created using createPlotComment
#' scale_size_manual geom_point
#' @param text_size The size of the label text
#' @param interpolation Should points be interpolated
#'
#' @return A ggplot
#' @export
#' @importFrom ggplot2 labs theme guides element_blank element_text scale_color_manual scale_shape_manual scale_size_manual geom_point aes_string scale_y_continuous coord_cartesian
#' @importFrom ggthemes theme_tufte
#' @importFrom dplyr filter
#' @importFrom plyr round_any
#' @importFrom scales trans_format number_format
create_base_plot <- function(profile_data, best_fit = FALSE, best_fit_n = 3, log = FALSE,
                             comments = NULL, text_size = 12,
                             interpolation = TRUE) {

  # hard coded color scheme
  NNcol <- list(dark_blue = "#001965", black = "#231F20",
                forest_green = "#3F9C35", spring_green = "#C1D82F",
                lava_red = "#E64A0E", golden_yellow = "#EAAB00")
  
  labels <- c("Tail", "In-tail", "Non-tail", "Excluded")
  
  if ("highlight" %in% colnames(profile_data)) {
    high_label <- unique(profile_data$highlight[profile_data$highlight != ""])
    
    high_col <- nncol$company[ match(c( "sunset_orange", "golden_yellow",  
                                        "light_blue", "granite_grey", "lava_red", 
                                        "forest_green", "ocean_blue",
                                        "dark_blue", "sky_blue",    "misty_blue", 
                                        "grass_green", "lime_green"), names(nncol$company))][
                                          seq_along(high_label)]
    
    high_shape <- rep(1, length(high_label))
    
    high_sizes <- rep(4, length(high_label))
  } else {
    high_label <- high_col <- high_shape <- high_sizes <- c()
  }
  
  colors <- unlist(c(NNcol$spring_green, NNcol$forest_green, NNcol$dark_blue, NNcol$lava_red))
  
  colors <- c(colors, high_col, NNcol$lava_red, NNcol$golden_yellow)
  shapes <- c(rep(19, length(labels)), high_shape, 1, 4)
  sizes  <- c(rep(4, length(labels)), high_sizes, 6, 5)
  
  names(colors) <- c(labels, high_label, "Review comment","Lab comment")
  names(shapes) <- c(labels, high_label, "Review comment","Lab comment")
  names(sizes)  <- c(labels, high_label, "Review comment","Lab comment")
  
  
  # Should we paste log info on title
  log_paste <- ifelse(log, " - log scale", "")

  profile <- paste0(profile_data$profid[1], log_paste)

  # Add tail
  if (sum(profile_data$tailfl == "Y") > 2) {
    if (best_fit) {
      fit_list <- bestTailFitCens(profile_data$lower_conc[profile_data$tailfl == "Y"],
                                  profile_data$upper_conc[profile_data$tailfl == "Y"],
                                  profile_data$plot_time[profile_data$tailfl == "Y"],
                                  min_points = best_fit_n)

    } else {
      fit <- tailFitCens(profile_data$lower_conc[profile_data$tailfl == "Y"],
                              profile_data$upper_conc[profile_data$tailfl == "Y"],
                              profile_data$plot_time[profile_data$tailfl == "Y"])

      time_points <- profile_data$plot_time[profile_data$tailfl == "Y"]

      fit_list <- list(best_fit = fit, time_points = time_points)
    }

    p_tail <- pkPlot_tailFit(fit_list$best_fit, log = log, colour = "#3F9C35")
    profile_data$col_flag[profile_data$plot_time %in% fit_list$time_points]  <- "In-tail"
    
  } else {
    fit_list <- list(best_fit = NULL, time_points = NULL)
    p_tail <- NULL
  }
  
  # profile_data$col_flag[profile_data$tailfl == "Y"] <- "Tail"
  # profile_data$tailfl[profile_data$tailfl == "" | profile_data$tailfl == "N"]  <- "Non-tail"
  # profile_data$tailfl[profile_data$time %in% fit_list$time_points]  <- "In-tail"
  profile_data$line_col <- "Non-tail"

  # The base plot
  p <- pkPlot(profile_data, time = "plot_time", value = "aval", colour = "col_flag", shape = "col_flag", size = "col_flag", log = log)
    
  suppressWarnings(
  if (interpolation)
    p <- p + pkPlot_line(mapping = ggplot2::aes_string(color = "line_col", shape = "line_col", size = "line_col"), data = . %>%  dplyr::filter(.data$anelfl == "Y"))
  )
  p <- p + p_tail + pkPlot_dot()
  
  
  if ("highlight" %in% colnames(profile_data))
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(.data$highlight != ""),
                        ggplot2::aes_string(colour = "highlight", shape = "highlight", size = "highlight"), stroke = 1.5)
  
  # Points below LLOQ
  p <- p + pkPlot_dot(data = . %>%  dplyr::filter(lower_conc != upper_conc), color = "white", size = 1.5, show.legend = FALSE)

  time_unit <- ""
  if ("time_unit" %in% colnames(profile_data)) 
    time_unit <- paste0("(", profile_data$time_unit[1], ")")
  
  conc_unit <- ""
  if ("conc_unit" %in% colnames(profile_data)) 
    conc_unit <- profile_data$conc_unit[1]
  
  # Add theme cols
  p <- p + ggplot2::labs(#title = paste("PK-profile for", profile),
    x = paste("Time", time_unit),
    y = paste("Concentration", conc_unit, log_paste)) +
    ggthemes::theme_tufte(base_size = text_size, base_family = "sans") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5, size = text_size)) +
    ggplot2::scale_color_manual(breaks = names(colors), values = colors) +
    ggplot2::scale_shape_manual(breaks = names(shapes), values = shapes) +
    ggplot2::scale_size_manual(breaks =  names(sizes), values = sizes) +
    ggplot2::guides(color=ggplot2::guide_legend(ncol=3, byrow=TRUE))

  if (!is.null(comments))
    p <- p + ggplot2::geom_point(mapping = ggplot2::aes_string("plot_time", ifelse(log, "log_aval", "aval"),
                                                   color = "type_comment",
                                                   shape = "type_comment",
                                                   size  = "type_comment"),
                            data = comments[comments$profid == profile_data$profid[1],],  stroke = 1.5,
                            show.legend = FALSE, na.rm = TRUE)
  # Ranges and ticks
  min_x <- min(profile_data$plot_time, na.rm = TRUE)
  
  max_x <- max(profile_data$plot_time, na.rm = TRUE)
  
  min_y <- min(profile_data$aval, na.rm = TRUE)
  
  max_y <- max(profile_data$aval, na.rm = TRUE)
  
  min_log_y <- min(ifelse(profile_data$aval <= 0, NA, profile_data$aval),
                   ifelse(is.null(fit_list$best_fit), NA, predictConc(fit_list$best_fit, max_x)),
                   na.rm = TRUE) %>% log()
  
  max_log_y <- max(ifelse(profile_data$aval <= 0, NA, profile_data$aval),
                   ifelse(is.null(fit_list$best_fit), NA, predictConc(fit_list$best_fit, max_x)),
                   na.rm = TRUE) %>% log()
  
  if (log == TRUE) {
    
    org_breaks <- pretty(c(min_y, max_y), n = 4)
    
    min_org_breaks <- org_breaks[org_breaks > 0] %>% min()
    
    second_break <- ifelse(floor(log10(min_org_breaks/4)) == floor(log10(min_org_breaks)) | min_org_breaks/4 <= 2.5*10^(floor(log10(min_org_breaks/4))),
                           plyr::round_any(min_org_breaks/4, 10^(floor(log10(min_org_breaks/4)))),
                           plyr::round_any(min_org_breaks/4, 5*10^(floor(log10(min_org_breaks/4)))))
    
    first_break <- ifelse(floor(log10(min_org_breaks/32)) == floor(log10(min_org_breaks/4)) | min_org_breaks/32 <= 2.5*10^(floor(log10(min_org_breaks/32))),
                          plyr::round_any(min_org_breaks/32, 10^(floor(log10(min_org_breaks/32)))),
                          plyr::round_any(min_org_breaks/32, 5*10^(floor(log10(min_org_breaks/32)))))
    
    breaks <- c(first_break, second_break, org_breaks) %>% 
      unique()
    
    breaks <- breaks[log(breaks) >= min_log_y & log(breaks) <= max_log_y]
    
    log_breaks <- log(breaks)
    
    # log_breaks <- log_breaks[c(1, 1 + which(diff(log_breaks) >= 0.2))]
    
    n_decimals <- nchar(strsplit(paste(min(breaks)),"\\.")[[1]][2])
    
    accuracy <- ifelse(is.na(n_decimals), 1, 10^-n_decimals)
    
    p <- p + ggplot2::scale_y_continuous(breaks = log_breaks,
                                         labels = scales::trans_format("exp", scales::number_format(accuracy = accuracy))) + 
      ggplot2::coord_cartesian(xlim = c(min_x, max_x),
                               ylim = c(min_log_y, max_log_y))
  }
  else {
    breaks <- pretty(c(min_y, max_y), n = 4)
    
    breaks <- breaks[breaks >= min_y & breaks <= max_y]
    
    n_decimals <- nchar(strsplit(paste(min(breaks[breaks > 0])),"\\.")[[1]][2])
    
    accuracy <- ifelse(is.na(n_decimals), 1, 10^-n_decimals)
    
    p <- p + ggplot2::scale_y_continuous(breaks = breaks,
                                         labels = scales::number_format(accuracy = accuracy)) + 
      ggplot2::coord_cartesian(xlim = c(min_x, max_x),
                               ylim = c(min_y, max_y))
  }

  return(p)
}

#' Convert comment data for the pk plot
#'
#' @param data The PK data
#' @param comments The complete comment data
#'
#' @return A data set formatted for use in the
#' @export
#' @importFrom dplyr mutate select left_join select rename bind_rows
#' @importFrom tidyr unnest
#' @importFrom labelled remove_labels
createPlotComment <- function(data, comments) {

  # Create join data for comments to get time and value
  # data[is.na(data$aval), "aval"] <-
  #   data[is.na(data$aval), "upper_conc"] / 2
  all_data <- data %>% dplyr::mutate(log_aval = log(.data$aval), join_var = as.character(.data$time))

  all_data <- labelled::remove_labels(all_data)

  # Create one comment per row
  if (nrow(comments))
    comment_unnest <- comments %>% dplyr::ungroup() %>%
      dplyr::select(profid = .data$profile, .data$user, .data$comment, time_join = .data$time_points) %>%
      tidyr::unnest(.data$time_join) %>% dplyr::mutate(join_var = as.character(.data$time_join)) %>%
      dplyr::left_join(all_data, by = c("profid", "join_var")) %>%
      dplyr::select(.data$profid, .data$user, .data$comment, .data$time, .data$plot_time, .data$aval, .data$log_aval)

  #
  lab_comments <- all_data[!all_data$lab_comment %in% c("", "BLQ"),
                                    c("profid", "lab_comment", "time", "plot_time", "aval")] %>%
    dplyr::mutate(user = "Lab", log_aval = log(.data$aval)) %>% dplyr::rename(comment = .data$lab_comment)

  out <- lab_comments %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))

  if (nrow(lab_comments) && nrow(comments))
    out <- dplyr::bind_rows(comment_unnest, lab_comments) %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))

  if (nrow(lab_comments) == 0 && nrow(comments))
    out <- comment_unnest %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))

  if (nrow(lab_comments)  && nrow(comments) == 0)
    out <- lab_comments %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))

  return(out)
}

#' Convert comment data for the pk table
#'
#' @param data The PK data
#' @param comments The complete comment data
#'
#' @return A data set formatted for use in the
#' @export
#' @importFrom dplyr mutate select filter left_join rename bind_rows group_by ungroup summarise_each
#' @importFrom tidyr unnest
#' @importFrom labelled remove_labels
createTableComment <- function(data, comments){
  
  # Create join data for comments to get time and value
  # data[is.na(data$aval), "aval"] <-
  #   data[is.na(data$aval), "upper_conc"] / 2
  
  all_data <- data %>% dplyr::mutate(log_aval = log(.data$aval), join_var = as.character(.data$time))
  
  all_data <- labelled::remove_labels(all_data)
  
  # Create one comment per row
  if (nrow(comments)){
    comments_point <- comments %>% dplyr::ungroup() %>% 
      dplyr::filter(unlist(lapply(comments$time_points, function(x) {!identical(unlist(x),numeric(0))}))) %>%
      dplyr::select(profid = .data$profile, .data$user, .data$regarding, .data$comment, .data$status, time_join = .data$time_points) %>%
      tidyr::unnest(.data$time_join) %>% dplyr::mutate(join_var = as.character(.data$time_join)) %>%
      dplyr::left_join(all_data, by = c("profid", "join_var")) %>% 
      dplyr::select(.data$profid, .data$user, .data$regarding, .data$comment, .data$time, .data$plot_time, .data$status) %>%
      dplyr::group_by(.data$profid, .data$user, .data$regarding, .data$comment, .data$status) %>% 
      dplyr::summarise_each("list", time_points = .data$time, plot_time_points = .data$plot_time) %>% dplyr::ungroup()
    
    comments_profile <- comments %>% dplyr::ungroup() %>% 
      dplyr::filter(unlist(lapply(comments$time_points, function(x) {identical(unlist(x),numeric(0))}))) %>%
      dplyr::select(profid = .data$profile, .data$user, .data$regarding, .data$comment, .data$time_points, .data$status) %>%
      dplyr::mutate(plot_time_points = list(numeric(0)))
    
    comments_all <- dplyr::bind_rows(comments_profile, comments_point)
  }
    
  #
  lab_comments <- all_data[!all_data$lab_comment %in% c("", "BLQ"),
                           c("profid", "lab_comment", "time", "plot_time")] %>%
    dplyr::mutate(user = "Lab", regarding = "", status = "") %>% dplyr::rename(comment = .data$lab_comment)
  
  out <- lab_comments %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))
  
  if (nrow(lab_comments) && nrow(comments)) {
    lab_comments$time_points <- lapply(lab_comments$time, list)
    lab_comments$plot_time_points <- lapply(lab_comments$plot_time, list)
    out <- dplyr::bind_rows(comments_all, lab_comments) %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))
  }
  if (nrow(lab_comments) == 0 && nrow(comments))
    out <- comments_all %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))
  
  if (nrow(lab_comments)  && nrow(comments) == 0) {
    lab_comments$time_points <- lapply(lab_comments$time, list)
    lab_comments$plot_time_points <- lapply(lab_comments$plot_time, list)
    out <- lab_comments %>%
    dplyr::mutate(type_comment = ifelse(.data$user == "Lab", "Lab comment", "Review comment"))
  }
  
  if(!is.null(out) && !is.na(out) && nrow(out)){
    out <- out %>% dplyr::select(.data$profid, .data$user, .data$regarding, .data$comment,
                                 .data$time_points, .data$plot_time_points, .data$status, .data$type_comment)
    
    colnames(out)[match(c("profid", "user", "regarding", "comment", "time_points",
                          "plot_time_points", "status", "type_comment"), colnames(out))] <- c("Profile", "User", "Regarding",
                                                                                              "Comment", "Nominal time", "Time",
                                                                                              "Status", "Type")
  }
  
  
  return(out)
}


#' Create a combined plot with both original scale and log scale plots
#'
#' @param profile_data The data used
#' @param best_fit should the best fit be found automatically
#' @param best_fit_n The minimum number of points to use
#' @param comments data set with comments, created using createPlotComment
#'
#' @return The combined plot on both original and log scale
#' @export
#' @importFrom ggpubr ggarrange
create_comb_plot <- function(profile_data, best_fit = FALSE, best_fit_n = 3,
                             comments = NULL) {
  p1 <- create_base_plot(profile_data = profile_data, best_fit = best_fit, best_fit_n = best_fit_n,
                         log = FALSE, comments = comments)

  p2 <- create_base_plot(profile_data = profile_data, best_fit = best_fit, best_fit_n = best_fit_n,
                         log = TRUE, comments = comments)

  ggpubr::ggarrange(p1, p2)
}

#' Title
#'
#' @param comb_plots list of plots to combine into a word document
#' @param comments data set with comments, created using createPlotComment
#' @param type The type of document, PK/PD
#' @param title The four titles for the header
#' @param ... additional arguments passed on to header
#'
#' @return create the word file with the plots
#' @export
#' @importFrom officer read_docx cursor_begin body_remove body_add_toc body_add_break body_add_par body_add_gg
#' @importFrom magrittr '%<>%'
wordplot <- function(comb_plots, comments = NULL, type = "PK",
                     title = c("NN1436-4225", "First draft", "PK-profiles", "Not validated"), ...) {
  my_empty <- officer::read_docx(system.file(file.path("word_template",
                  "template_novo_portrait_empty.docx"), package = "NNPKPD"))
  my_land  <- officer::read_docx(system.file(file.path("word_template",
                  "template_novo_landscape.docx"), package = "NNPKPD"))
  #my_port  <- officer::read_docx(system.file(file.path("word_template",
  #                "template_novo_portrait.docx"), package = "NNPKPD"))

  titles <- paste(type, "profile for", names(comb_plots))

  land_ex <- my_land %>%
    officer::cursor_begin() %>% officer::body_remove() %>%
    header(title = title, ...)
  # land_ex %<>% officer::body_add_toc( separator = ";") %>%  officer::body_add_break()

  # after the las plot we do not want page break
  n_plots <- length(comb_plots)
  for (i in seq_len(n_plots - 1 )) {
    land_ex %<>% officer::body_add_par(titles[i], style = "FigureTitle")
    land_ex %<>% officer::body_add_gg(value = comb_plots[[i]], width = 9.5, height = 5.8)
    land_ex %<>% officer::body_add_break()
    if (!is.null(comments)) {
      table <- comments[comments$Profile == names(comb_plots)[i],]
      if (nrow(table)) {
        land_ex %<>% officer::body_add_table(table, style = "table_template")
        land_ex %<>% officer::body_add_break()
      }
    }
  }
  land_ex %<>% officer::body_add_par(titles[n_plots], style = "FigureTitle")
  land_ex %<>% officer::body_add_gg(value = comb_plots[[n_plots]], width = 9.5, height = 5.8)
  if (!is.null(comments)) {
    table <- comments[comments$Profile == names(comb_plots)[n_plots],]
    if (nrow(table)) {
      land_ex %<>% officer::body_add_table(table, style = "table_template")
      land_ex %<>% officer::body_add_break()
    }
  }

  return(land_ex)
}


# profile_data_keep <- profile_data <- pkdata_all
# 
# all_comments
# 
# p <- create_base_plot(profile_data = profile_data_keep[profile_data_keep$profid == "101001 I287 FD",], log = TRUE)
# 
# pk_hover <- createPlotComment(adpc_data, PK_comments_data_all)
# 
# 
# plot_data <- plyr::dlply(adpc_data, "profid", tibble::as_tibble)
# 
# orig_plots <- lapply(plot_data, create_base_plot, log = FALSE,
#                      comments = createPlotComment(adpc_data, PK_comments_data_all))
# 
# log_plots  <- lapply(plot_data, create_base_plot, log = TRUE,
#                      comments = createPlotComment(adpc_data, PK_comments_data_all))
# 
# 
# comb_plots <- lapply(plot_data, create_comb_plot,
#                      comments = createPlotComment(adpc_data, PK_comments_data_all))
# 
# filename <-
# 
# word_object <- wordplot()
# officer::print(word_object, filename)
# system(paste("open", filename))
# 
# 
# 
# unlink(filename)


# To avoid NOTEs the R CMD check
utils::globalVariables(".")

#' Create the base pd plot for the review app
#'
#' @param profile_data The data used
#' @param comments data set with comments
#' @param text_size The size of the label text
#' @param panel_data Dataset explaining panel setup
#' @param plot_data Dataset explaining the type of plot for each paramcd
#' @param col_scheme The color associated with each paramcd
#'
#' @return A ggplot
#' @export
#' @importFrom ggplot2 labs theme element_blank element_text scale_fill_manual scale_color_manual scale_shape_manual scale_alpha_manual scale_size_manual scale_linetype_manual aes_string
#' @importFrom dplyr left_join mutate
#' @importFrom tidyr unnest spread
#' @importFrom rlang sym
create_base_pd_plot <- function(profile_data, comments = NULL, 
                                panel_data, plot_data, col_scheme,
                                text_size = 10) {

  plot_data_out <- profile_data %>% dplyr::filter(!is.na(.data$panel_name))
  
  if ("color_by" %in% colnames(plot_data_out)) { 
    adpdc_sub <- plot_data_out %>%   
      dplyr::left_join(col_scheme[, c("paramcd", "color_by", "label")], by = c("paramcd", "color_by"))
  
    n_legend_rows <- length(unique(plot_data_out$color_by))

    col_scheme <- col_scheme[paste(col_scheme$paramcd, col_scheme$color_by) %in% paste(adpdc_sub$paramcd, adpdc_sub$color_by), ]
    
  } else {
    adpdc_sub <- plot_data_out %>%   
      dplyr::left_join(col_scheme[, c("paramcd", "label")], by = c("paramcd"))
    
    n_legend_rows <- 1

    col_scheme <- col_scheme[col_scheme$paramcd %in% adpdc_sub$paramcd, ]
  }
  
  comments <- comments[comments$profile %in% plot_data_out$profid,]
  
  comments_int <- comments[comments$time_type == "interval" & 
                             !is.na(comments$time_type), ]
  
  comments_point <- comments[comments$time_type == "point" & 
                               !is.na(comments$time_type), ]
  
  p <- ggplot2::ggplot(adpdc_sub, mapping = ggplot2::aes_string("time", "aval"))
  
  
  for (i in seq_len(nrow(col_scheme))) {
    shape <- ifelse(!is.nan(col_scheme$shape[i]), col_scheme$shape[i], NA)
    size <- ifelse(!is.nan(col_scheme$size[i]), col_scheme$size[i], 0)
    lty <- ifelse(!is.na(col_scheme$lty[i]), col_scheme$lty[i], "blank")
    width <- ifelse(!is.nan(col_scheme$width[i]), col_scheme$width[i], 0)
    alpha <- col_scheme$alpha[i]
    fill <- ifelse(!is.na(col_scheme$fill[i]), col_scheme$fill[i], col_scheme$color[i])
    
    if (col_scheme$line[i] == "region") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_ribbon(data = . %>% dplyr::filter(line == "region" & paramcd == param_xx & color_by == color_by_xx),
                                                      mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = label), shape = NaN, linetype = "blank", alpha = alpha, size = NaN, 
                                                      na.rm = TRUE, key_glyph = "point"),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_ribbon(data = . %>% dplyr::filter(line == "region" & paramcd == param_xx),
                                                      mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = label), shape = NaN, linetype = "blank", alpha = alpha, size = NaN,  
                                                      na.rm = TRUE, key_glyph = "point"),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
    
    if (col_scheme$line[i] == "h-line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_hline(data = . %>% dplyr::filter(line == "h-line" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(yintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width,  
                                                     na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_hline(data = . %>% dplyr::filter(line == "h-line" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(yintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, 
                                                     na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
    
    if (col_scheme$line[i] == "v-line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_vline(data = . %>% dplyr::filter(line == "v-line" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(xintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width,
                                                     na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_vline(data = . %>% dplyr::filter(line == "v-line" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(xintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width,  
                                                     na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }

    
    if (col_scheme$line[i] == "line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_line(data = . %>% dplyr::filter(line == "line" & paramcd == param_xx & color_by == color_by_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_line(data = . %>% dplyr::filter(line == "line" & paramcd == param_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
    
    if (col_scheme$line[i] == "step") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_step(data = . %>% dplyr::filter(line == "step" & paramcd == param_xx & color_by == color_by_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_step(data = . %>% dplyr::filter(line == "step" & paramcd == param_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
    if (col_scheme$symbol[i] == "point") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_point(data = . %>% dplyr::filter(symbol == "point" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(colour = label), fill = fill, shape = shape, linetype = "blank", alpha = alpha, size = size, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_point(data = . %>% dplyr::filter(symbol == "point" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(colour = label), fill = fill, shape = shape, linetype = "blank", alpha = alpha, size = size, na.rm = TRUE),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
  }
  
  # Highlight of comment intervals
  if (any(sapply(comments_int$time_points, length))) {
    rows <- sapply(comments_int$time_points, function(x) length(x) > 0)
    
    new_data <- comments_int[rows, ] %>% dplyr::mutate(key = list(c("from", "to"))) %>%
      tidyr::unnest(time_points, key, .preserve = paramcd) %>% 
      tidyr::spread(key = key, value = time_points) %>% 
      tidyr::unnest(paramcd) %>% 
      dplyr::left_join(plot_data[, c("paramcd", "panel_name")], by = "paramcd")
    
    
    new_data$panel_name <- factor(new_data$panel_name, panel_data$panel_name)
    
    new_data$label <- "Review comment "
    
    col_scheme_com_int <- tibble::tibble(label = "Review comment ", line = "region", color = "#009FDA", alpha = 0.2, fill = "#009FDA", lty = "blank", shape = NaN, size = NaN, width = NaN)
    
    col_scheme <- dplyr::bind_rows(col_scheme, col_scheme_com_int)
    
    if (any(!is.na(new_data$panel_name)))
      p <- p + 
      ggplot2::geom_rect(data =  new_data[!is.na(new_data$panel_name), ], 
                         ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = label), 
                         shape = col_scheme_com_int$shape[1],
                         linetype = col_scheme_com_int$lty[1], alpha = col_scheme_com_int$alpha[1],
                         inherit.aes = FALSE, key_glyph = "point")
    
    
    if (any(is.na(new_data$panel_name)))
      p <- p + 
      ggplot2::geom_rect(data =  new_data[is.na(new_data$panel_name), setdiff(colnames(new_data), "panel_name")], 
                         ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = label), 
                         shape = col_scheme_com_int$shape[1],
                         linetype = col_scheme_com_int$lty[1], alpha = col_scheme_com_int$alpha[1],
                         inherit.aes = FALSE, key_glyph = "point")
  }
  
  # Highlight of comment points
  if (!is.null(comments_point) && nrow(comments_point) > 0) {
    
    if (nrow(comments_point) > 0) {
      
      
      # "%pin%" <- function(x, table) pmatch(x, table, nomatch = 0) > 0
      com_data_point <- adpdc_sub %>% 
        dplyr::filter(paste(round(time, 5), paramcd) %in% 
                        paste(round(unlist(comments_point$time_points), 5), unlist(comments_point$time_point_paramcd)))
      
      if (nrow(com_data_point) > 0){
        com_data_point$label <- "Review comment"
        col_scheme_com <- tibble::tibble(label = "Review comment", symbol = "point", shape = 1, color = "#E64A0E", fill = "transparent", lty = "blank", stroke = 1.5, alpha = 1)
        col_scheme_com$size <-  max(col_scheme[col_scheme$symbol == "point", ]$size, na.rm = TRUE) + 5
        
        col_scheme <- dplyr::bind_rows(col_scheme, col_scheme_com)
        
        p <- p + 
          ggplot2::geom_point(data = com_data_point, 
                              mapping = ggplot2::aes(colour = label), fill = col_scheme_com$fill[1], shape = col_scheme_com$shape[1], linetype = col_scheme_com$lty[1], alpha = col_scheme_com$alpha[1], size = col_scheme_com$size[1], stroke = col_scheme_com$stroke[1], na.rm = TRUE)
      }
    }
  }  

  
  # create axes
  
  time_unit <- ""
  if ("time_unit" %in% colnames(adpdc_sub)) 
    time_unit <- paste0("(", adpdc_sub$time_unit[1], ")")
  
  p <- p + 
    ggplot2::labs(#title = paste("PK-profile for", profile),
      x = paste("Time", time_unit),
      y = paste("Value"))
  
  col_scheme$size[is.nan(col_scheme$size)] <- col_scheme$width[is.nan(col_scheme$size)]
  col_scheme$size[is.nan(col_scheme$size)] <- 0
  col_scheme$lty[is.na(col_scheme$lty)] <- "blank"
  col_scheme$alpha[is.na(col_scheme$alpha)] <- 1
  
  legends <- col_scheme %>% dplyr::filter(!(line == "region" & symbol == "")) %>% 
    dplyr::mutate(stroke = dplyr::case_when(label == "Review comment" ~ 1.5, symbol == "point" ~ 0.7, TRUE ~ NA_real_),
                  size = ifelse(label == "Review comment", 5, max(min(size, width, 4, na.rm = TRUE), 1.25, na.rm = TRUE))) %>% 
    dplyr::select(label, color, lty, alpha, shape, fill, size, stroke) %>%
    dplyr::distinct(label, .keep_all = TRUE) %>% dplyr::arrange(label)
  
  legends_region <- col_scheme %>% dplyr::filter(line == "region") %>% 
    dplyr::select(label, lty, alpha, shape, fill, size) %>%
    dplyr::mutate(color = fill, lty = "blank", shape = 15, size = 6) %>%
    dplyr::distinct(label, .keep_all = TRUE) %>% dplyr::arrange(label)
  
  panel_name <- "panel_name"
  
  # Apply color scheme
  p <- p + ggplot2::theme_gray(base_size = text_size) + 
    ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(panel_name)), scales = "free_y") +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::guides(size = FALSE, alpha = FALSE, stroke = FALSE, shape = FALSE, linetype = FALSE,
                    colour = ggplot2::guide_legend(title = "", order = 1, nrow = n_legend_rows,
                                                   override.aes = list(fill = structure(legends$fill, names = legends$label),
                                                                       alpha = structure(legends$alpha, names = legends$label),
                                                                       size = structure(legends$size, names = legends$label),
                                                                       stroke = structure(legends$stroke, names = legends$label),
                                                                       shape = structure(legends$shape, names = legends$label),
                                                                       linetype = structure(legends$lty, names = legends$label))),
                    fill = ggplot2::guide_legend(title = "", order = 2, nrow = n_legend_rows,
                                                 override.aes = list(color = structure(legends_region$color, names = legends_region$label),
                                                                     alpha = structure(legends_region$alpha, names = legends_region$label),
                                                                     size = structure(legends_region$size, names = legends_region$label),
                                                                     shape = structure(legends_region$shape, names = legends_region$label),
                                                                     linetype = structure(legends_region$lty, names = legends_region$label)))) +
    ggplot2::scale_colour_manual(values = structure(legends$color, names = legends$label)) +
    ggplot2::scale_fill_manual(values = structure(legends_region$fill, names = legends_region$label))
  
  return(p)
}


#' Convert comment data for the pd table
#'
#' @param comments The complete comment data
#'
#' @return A data set formatted for use in the
#' @export
#' @importFrom dplyr mutate select filter bind_rows ungroup
#' @importFrom glue glue_collapse
createTablePDComment <- function(comments){

  # Create one comment per row
  if (nrow(comments)){
    comments_point <- comments %>% dplyr::ungroup() %>% 
      dplyr::filter(.data$time_type == "point") %>% dplyr::rowwise() %>%
      dplyr::mutate(type = "Time point(s)", 
                    time = ifelse(length(.data$time_points),
                                  glue::glue_collapse(round(unlist(.data$time_points), 2), sep = ", ", last = " and "),
                                  "")) %>%
      dplyr::select(profid = .data$profile, .data$user, .data$regarding, .data$comment, .data$time, .data$status, .data$type)
    
    comments_int <- comments %>% dplyr::ungroup() %>% 
      dplyr::filter(.data$time_type == "interval") %>% dplyr::rowwise() %>%
      dplyr::mutate(type = "Time interval",
                    time = ifelse(length(.data$time_points),
                                  glue::glue_collapse(round(unlist(.data$time_points), 2), sep = " to "),
                                  "")) %>%
      dplyr::select(profid = .data$profile, .data$user, .data$regarding, .data$comment, .data$time, .data$status, .data$type)
    
    comments_profile <- comments %>% dplyr::ungroup() %>% 
      dplyr::filter(.data$time_type == "profile" | .data$time_type == "") %>%
      dplyr::mutate(type = "Profile", time = "") %>%
      dplyr::select(profid = .data$profile, .data$user, .data$regarding, .data$comment, .data$time, .data$status, .data$type)
    
    out <- dplyr::bind_rows(comments_profile, comments_int, comments_point)
      
    colnames(out)[match(c("profid", "user", "regarding", "comment", "time",
                          "status", "type"), colnames(out))] <- c("Profile", "User", "Regarding",
                                                                  "Comment", "Time",
                                                                  "Status", "Type")
  }
  else {
    out <- NULL
  }

  return(out)
}
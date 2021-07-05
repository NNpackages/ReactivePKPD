#----------------------------------------#
######     Profile selector         ######
#----------------------------------------#

profile_choices <- reactive({

  req(all_profiles(), !is.null(pk_user_review()), !is.null(pk_comment_status()))
  
  cat("test--------------------------------------------------------------test\n")
  print("user_review")
  print(pk_user_review())
  print("pk_comment_status")
  print(pk_comment_status())
  
  
  if (PKPDEnv("verbose")) cat("profile_choices\n")
  profiles <- all_profiles()
  comment_status <- pk_comment_status()
  # Only none-reviewed
  if (input$show_profiles == "non-reviewed") {
    user_review <- pk_user_review()
    profiles <- setdiff(all_profiles(),  user_review)
    
    if (!length(profiles)) {
      shinyalert::shinyalert("No none reviewed profiles left", type = "info")
      profiles <- all_profiles()
      shinyWidgets::updatePrettyRadioButtons(session, "show_profiles", selected = "All")
    }
  }
  
  #$comment
  
  # only with comments
  if (input$show_profiles == "with comments") {
    profiles <- intersect(profiles,  comment_status[["with comments"]])
  }
  
  
  # Only with non-final comments
  if (input$show_profiles == "non-final") {
    profiles <- intersect(profiles,  
      unlist(comment_status[setdiff(names(comment_status), c("with comments", "Final", "No action"))]))
  }
  
  # Only with awaiting comments
  if (input$show_profiles == "Awaiting approval") {
    profiles <- intersect(profiles,  comment_status[["Awaiting approval"]])
  }
  
  # Only with re-evaluate comments
  if (input$show_profiles == "Re-evaluate") {
    profiles <- intersect(profiles,  comment_status[["Re-evaluate"]])
  }
  
  if (!length(profiles)) {
    shinyalert::shinyalert("No profiles fulfill the chosen criteria - showing all profiles", type = "info")
    profiles <- all_profiles()
    shinyWidgets::updatePrettyRadioButtons(session, "show_profiles", selected = "All")
  }
  
  #print(profiles)
  profiles
  data_adpc <- pkdata()
  sub_adpc <- data_adpc[data_adpc$profid %in% profiles, ]
  
  ord_var <- input$select_profile_order
  if (isolate(setupRV$add_order) && !is.null(ord_var))
    profiles <- dplyr::arrange(sub_adpc, !!! rlang::syms(ord_var)) %>% dplyr::pull("profid") %>% unique()
  
  profiles
})


output$profile_select_ui_order <- renderUI({
  req(pkdata())
  if (PKPDEnv("verbose")) cat("profile_select_ui_order\n")
  data_adpc <- pkdata()
  
  wanted <- structure(c("profid", "type", "drug" ), names = c("profile", "type", "drug"))
  available <- wanted[wanted %in% colnames(data_adpc)]
  
  if (length(available) > 1) {
    setupRV$add_order <- TRUE
    selectizeInput("select_profile_order", "Order of profiles", available,
                   selected = available, multiple = TRUE)
  } else {
    setupRV$add_order <- FALSE
    NULL
  }
})

output$profile_select_ui <- renderUI({
  req(pkdata(), pk_time_range(), profile_choices(), length(profile_choices() > 0))
  if (PKPDEnv("verbose")) cat("profile_select_ui\n")
  data_adpc <- pkdata()
  
  profiles <- profile_choices()
  
  isolate({
    if (!is.null(pkplotRV$profile_select_by_review) && pkplotRV$profile_select_by_review %in% profiles) {
      selected <- pkplotRV$profile_select_by_review
      pkplotRV$profile_select_by_review <- NULL
    }
    else if (!is.null(input$select_profile) && input$select_profile %in% profiles) {
      selected <- input$select_profile
    }
    else
      selected <-  profiles[1]
  })
  
  # generate initial range
  range <- pk_time_range() %>% dplyr::filter(profid == selected)
  pkplotRV$range = range
  
  
  pkplotRV$profile_select_ui <- selected
  selectizeInput("select_profile", "Select profile", choices = as.list(profiles),
              selected = as.list(selected), options = list(maxOptions = 5000))
  
})

output$pk_axes <- renderUI({
  req(pkdata(), pk_time_range(), profile_data())
  if (PKPDEnv("verbose")) cat("pk_axes\n")
  data_adpc <- pkdata()
  
  profiles <- unique(data_adpc$profid)
  
  # generate initial range
  isolate({
    if (!is.na(pkplotRV$profile_select_ui) && !is.null(pkplotRV$profile_select_ui))
      range <- pk_time_range() %>% dplyr::filter(profid == pkplotRV$profile_select_ui)
    
    else if (!is.na(input$select_profile) && !is.null(input$select_profile))
      range <- pk_time_range() %>% dplyr::filter(profid == input$select_profile)
    
    else
      range <- pk_time_range() %>% dplyr::filter(profid == profiles[1])
  })
  
  tagList(
    sliderInput("pk_xRange", "Range of x-axis:",
                min = range$min_x, max = range$max_x,
                value = c(range$min_x, range$max_x),
                round = TRUE, ticks = TRUE)
    
    # sliderInput("pk_yRange", "Range of y-axis:",
    #             min = range$min_y, max = range$max_y,
    #             value = c(range$min_y, range$max_y),
    #             round = TRUE, ticks = TRUE)
  )
})


observeEvent(input$select_profile, {
  pkplotRV$new_profile <- TRUE
  pkplotRV$profile_select_ui <- input$select_profile
  if (PKPDEnv("verbose")) cat(paste("New_profile:", pkplotRV$new_profile, "\n"))
}, priority = 100)

#----------------------------------------#
######       plot_profile_ui        ######
#----------------------------------------#

output$pk_profile_buttons <- renderUI({
  req(pkdata())
  user_type <- isolate(setupRV$user_type)
  
  if (user_type != "Visitor") {
    out <- tags$div( class = "form-group shiny-input-container",
                     splitLayout(
                       actionButton("add_pk_comment", "Add comment", width = "90%", style = paste0("color: white; background-color: ","#009FDA","; border-color: ","#009FDA")),
                       actionButton("profile_reviewed", "Profile reviewed", width = "90%", style = paste0("color: white; background-color: ","#3F9C35","; border-color: ","#3F9C35"))
                     )
    )
  } else {
    return(NULL)
  }
  return(out)
})

output$pk_profile_plot_box_name <- renderUI({
  paste("Pharmacokinetic profile for", input$select_profile)
})

output$plot_profile_ui <- renderUI({
  req(pkdata())
  user_type <- isolate(setupRV$user_type)
  
  data_adpc <- pkdata()
  
  wanted <- c("type", "drug") 
  available <- wanted[wanted %in% colnames(data_adpc)]
  
  # This needs to be trial specific
  
  fluidRow(
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = uiOutput("pk_profile_plot_box_name"),
      status = "primary",
      splitLayout(
        cellWidths = c("50%","50%"),
        div(
          style = "position:relative",
          plotOutput(
            "plot_profile" ,
            click = "plot_profile_click",
            dblclick = "plot_profile_dblclick",
            brush = brushOpts(id = "plot_profile_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_profile_hover", delay = 100, delayType = "debounce"),
            height = "600px", width = "95%"
          ),
          uiOutput("plot_profile_hover_info")
        ),
        div(
          style = "position:relative",
          plotOutput(
            "plot_log_profile" ,
            click = "plot_log_profile_click",
            dblclick = "plot_log_profile_dblclick",
            brush = brushOpts(id = "plot_log_profile_brush", resetOnNew = TRUE),
            hover = hoverOpts("plot_log_profile_hover", delay = 100, delayType = "debounce"),
            height = "600px", width = "95%"),
          uiOutput("plot_log_profile_hover_info")
        )
      ),
      uiOutput("pk_zoom_help"),
      uiOutput("pk_axes"),
      
      box(
        solidHeader = TRUE, collapsible = TRUE, width = "100%",
        status = "primary", collapsed = TRUE, style = "color: white; background-color: #222D32;",
        title = "Plot options",
        
        splitLayout(cellWidths = c(rep(paste0(100/2,"%"),2)),
                    tags$div(
                      h3("Options for original scale"),
                      "Show tail fit original scale: ",
                      shinyWidgets::prettyCheckbox(inputId = "tailfit_orig",
                                                   label = "Yes", value = TRUE, inline = TRUE),
                      br(),
                      "Show interpolation original scale: ",
                      shinyWidgets::prettyCheckbox(inputId = "interpolation_orig",
                                                   label = "Yes", value = TRUE, inline = TRUE)
                    ),
                    tags$div(
                      h3("Options for log scale"),
                      "Show tail fit log scale: ",
                      shinyWidgets::prettyCheckbox(inputId = "tailfit_log",
                                                   label = "Yes", value = TRUE, inline = TRUE),
                      br(),
                      "Show interpolation log scale: ",
                      shinyWidgets::prettyCheckbox(inputId = "interpolation_log",
                                                   label = "Yes", value = TRUE, inline = TRUE)
                    )
        ),
        tags$div(h3("General options")),
        splitLayout(cellWidths = c(rep(paste0(100/4,"%"),4)),
                    tags$div(
                      selectizeInput("align_x_range", "Align x-range within:", available,
                                     selected = available, multiple = TRUE, width = '80%')
                    ),
                    tags$div(
                      selectizeInput("align_y_range", "Align y-range within:", available,
                                     selected = available, multiple = TRUE, width = '80%')
                    ),
                    tags$div(
                      selectInput("mark_lloq", "Mark point(s) below LLOQ as:", choices = c("None", "Non-filled point(s)", "Vertical line(s)"),
                                  selected = "Non-filled point(s)", width = '80%')
                    ),
                    tags$div(
                      br(),
                      "Show excluded point(s): ",
                      shinyWidgets::prettyCheckbox(inputId = "show_excluded",
                                                   label = "Yes", value = TRUE, inline = TRUE)
                    )
        )
      )
    ),
    
    conditionalPanel(condition = "input.pk_commentstimeline == 0",
                     box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                         title = tagList(
                           "Profile comments - ",
                           "Show comments as timeline :",
                           shinyWidgets::prettyCheckbox(inputId = "pk_commentstimeline_table",
                                                        label = "Yes",
                                                        value = isTRUE(setupRV$timeline), inline = TRUE)), status = "primary",
                         DT::dataTableOutput("adpcCom_profile")
                     )
    ),
    conditionalPanel(condition = "input.pk_commentstimeline == 1",
                     box(solidHeader = TRUE, collapsible = TRUE, width = 6,
                         title = tagList(
                           "Profile comments - ",
                           "Show comments as timeline :",
                           shinyWidgets::prettyCheckbox(inputId = "pk_commentstimeline_timeline",
                                                        label = "Yes",
                                                        value = isTRUE(setupRV$timeline), inline = TRUE)
                         ),
                         status = "primary",
                         uiOutput("pk_profile_comments_ui")
                     )
    ),
    if (user_type == "superuser") {
      box(solidHeader = TRUE, collapsible = TRUE, width = 6,
          title = "Advanced settings",
          status = "primary",
          
          "Select best tail fit using points allowed in tail:",
          shinyWidgets::prettyCheckbox(inputId = "pk_best_fit",
                                       label = "Yes",
                                       value = FALSE, inline = TRUE),
          numericInput("pk_best_fit_n", "Minimum number of points for fit", 3)
      )
    }
  )
})



observeEvent(input$pk_commentstimeline_timeline, {
  setupRV$timeline <- input$pk_commentstimeline_timeline
})

observeEvent(input$pk_commentstimeline_table, {
  setupRV$timeline <- input$pk_commentstimeline_table
})

observeEvent(input$pk_commentstimeline, {
  setupRV$timeline <- input$pk_commentstimeline
})

output$pk_commentstimeline <- renderUI({
  tagList("Show comments as timeline :",
          shinyWidgets::prettyCheckbox(inputId = "pk_commentstimeline",
                                       label = "Yes",
                                       value = setupRV$timeline, inline = TRUE))
  
})


#----------------------------------------#
######   Plotting the profile       ######
#----------------------------------------#

# Function to predict last concentration based on tail fit
predLastConc <- function(lower, upper, time) { 
  fit <- tailFitCens(lower, upper, time)
  
  last_time <- max(time, na.rm = TRUE)
  
  predictConc(fit, last_time) 
}

# Get time ranges for all data
pk_time_range <- reactive({
  req(pkdata())
  
  adpc_data <- pkdata()
  
  profile_tail_fit <- adpc_data %>% dplyr::filter(tailfl == "Y") %>% dplyr::group_by(profid) %>%
    dplyr::summarize(n_points = dplyr::n()) %>% dplyr::ungroup() %>% dplyr::filter(n_points >= 3) %>% dplyr::pull(profid)
  if (length(profile_tail_fit)) {
    pred_last_conc <- adpc_data %>% dplyr::filter(profid %in% profile_tail_fit, tailfl == "Y") %>% dplyr::group_by(profid) %>% 
      dplyr::summarize(pred_last_conc = predLastConc(lower_conc, upper_conc, plot_time)) %>%
      dplyr::ungroup()
    
    adpc_data <- dplyr::left_join(adpc_data, pred_last_conc)
  }
  else {
    adpc_data$pred_last_conc <- NA
  }
  
  align_x_vars <- input$align_x_range
  align_x_vars <- align_x_vars[align_x_vars %in% colnames(adpc_data)]
  align_y_vars <- input$align_y_range
  align_y_vars <- align_y_vars[align_y_vars %in% colnames(adpc_data)]
  
  print(align_x_vars)
  # x-ranges
  if (!is.null(align_x_vars) && length(align_x_vars) > 0) {
    x_ranges <- adpc_data %>%
      dplyr::group_by(!!! rlang::syms(align_x_vars)) %>%
      dplyr::summarize(min_x = floor(min(plot_time, na.rm = TRUE)), max_x = ceiling(max(plot_time, na.rm = TRUE))) %>%
      dplyr::ungroup()
    
    x_ranges <- dplyr::left_join(adpc_data, x_ranges) %>% dplyr::select(profid, min_x, max_x) %>% unique()
  }
  
  else {
    x_ranges <- adpc_data %>%
      dplyr::group_by(profid) %>%
      dplyr::summarize(min_x = floor(min(plot_time, na.rm = TRUE)), max_x = ceiling(max(plot_time, na.rm = TRUE))) %>%
      dplyr::ungroup()
  }
  
  # y-ranges
  if (!is.null(align_y_vars) && length(align_y_vars) > 0) {
    orig_y_ranges <- adpc_data %>%
      dplyr::group_by(!!! rlang::syms(align_y_vars)) %>%
      dplyr::summarize(min_y = min(aval, lower_conc, upper_conc, pred_last_conc, na.rm = TRUE),
                       max_y = max(aval, lower_conc, upper_conc, pred_last_conc, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    log_y_ranges <- adpc_data %>% dplyr::mutate(aval = ifelse(aval <= 0, NA, aval),
                                                lower_conc = ifelse(lower_conc <= 0, NA, lower_conc),
                                                upper_conc = ifelse(upper_conc <= 0, NA, upper_conc)) %>%
      dplyr::group_by(!!! rlang::syms(align_y_vars)) %>%
      dplyr::summarize(min_log_y = min(log(aval), log(lower_conc), log(upper_conc), log(pred_last_conc), na.rm = TRUE),
                       max_log_y = max(log(aval), log(lower_conc), log(upper_conc), log(pred_last_conc), na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    orig_y_ranges <- dplyr::left_join(adpc_data, orig_y_ranges) %>% dplyr::select(profid, min_y, max_y) %>% unique()
    log_y_ranges <- dplyr::left_join(adpc_data, log_y_ranges) %>% dplyr::select(profid, min_log_y, max_log_y) %>% unique()
    
    y_ranges <- dplyr::left_join(orig_y_ranges, log_y_ranges)
  }
  
  else {
    orig_y_ranges <- adpc_data %>%
      dplyr::group_by(profid) %>%
      dplyr::summarize(min_y = min(aval, lower_conc, upper_conc, pred_last_conc, na.rm = TRUE),
                       max_y = max(aval, lower_conc, upper_conc, pred_last_conc, na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    log_y_ranges <- adpc_data %>% dplyr::mutate(aval = ifelse(aval <= 0, NA, aval),
                                                lower_conc = ifelse(lower_conc <= 0, NA, lower_conc),
                                                upper_conc = ifelse(upper_conc <= 0, NA, upper_conc)) %>%
      dplyr::group_by(profid) %>%
      dplyr::summarize(min_log_y = min(log(aval), log(lower_conc), log(upper_conc), log(pred_last_conc), na.rm = TRUE),
                       max_log_y = max(log(aval), log(lower_conc), log(upper_conc), log(pred_last_conc), na.rm = TRUE)) %>%
      dplyr::ungroup()
    
    y_ranges <- dplyr::left_join(orig_y_ranges, log_y_ranges)
  }
  
  ranges <- dplyr::left_join(x_ranges, y_ranges)
  
  return(ranges)
})


# Create dataset with only the selected profile

profile_data <- reactive({
  req(input$select_profile, !is.null(input$show_excluded))
  if (PKPDEnv("verbose")) cat("profile_data\n")
  profile <- input$select_profile
  isolate({
    profile_data_keep <- pkdata() %>% dplyr::filter(profid == !! rlang::enquo(profile))
  })
  
  if(!input$show_excluded)
    profile_data_keep <- profile_data_keep %>% dplyr::filter(anelfl == "Y")
  
  pkplotRV$highlighted <- NULL
  
  profile_data_keep
})


#----------------------------------------#
######   tail fit       ######
#----------------------------------------#

tail_fit <- reactive({
  req(profile_data())
  if (PKPDEnv("verbose")) cat("tail_fit\n")
  
  
  profile_data <- profile_data()
  
  if (all(profile_data$tailfl != "Y"))
    return("No points")
  
  if (!is.null(input$pk_best_fit) && input$pk_best_fit) {
    fit_list <- bestTailFitCens(profile_data$lower_conc[profile_data$tailfl == "Y"],
                                profile_data$upper_conc[profile_data$tailfl == "Y"],
                                profile_data$plot_time[profile_data$tailfl == "Y"],
                                min_points = input$pk_best_fit_n)
    
  } else {
    best_fit <- tailFitCens(profile_data$lower_conc[profile_data$tailfl == "Y"],
                            profile_data$upper_conc[profile_data$tailfl == "Y"],
                            profile_data$plot_time[profile_data$tailfl == "Y"])
    
    time_points <- profile_data$plot_time[profile_data$tailfl == "Y"]
    
    fit_list <- list(best_fit = best_fit, time_points = time_points)
  }
  
  fit_list
})

#----------------------------------------#
######        plot ranges           ######
#----------------------------------------#

observeEvent(input$pk_xRange, {
  
  current_range <- c(pkplotRV$range$min_x, pkplotRV$range$max_x)
  new_range     <- c(input$pk_xRange[1], input$pk_xRange[2])
  
  if (PKPDEnv("verbose")) cat(paste("pk_xRange\n old", current_range[1], current_range[2],
                                    "\n new", input$pk_xRange[1], input$pk_xRange[2], "\n"))
  
  
  if (any(current_range != new_range)) {
    pkplotRV$range$min_x <- input$pk_xRange[1]
    pkplotRV$range$max_x <- input$pk_xRange[2]
  }
})

plot_range <- reactive({
  if (PKPDEnv("verbose")) cat("pkplotRV$range updated\n")
  pkplotRV$range
})

# Initialize the highlight
pkplotRV$highlighted <- NULL

# collect into a reactive
pk_plot_highlight_points <- reactive({
  pkplotRV$highlighted
})

# New ranges by profile change
new_ranges_by_prof <- reactive({
  
  req(base_plot_log(), base_plot())
  
  if (PKPDEnv("verbose")) cat("new_ranges_by_prof - ")
  
  isolate({
    
    if (pkplotRV$new_profile) {
      if (PKPDEnv("verbose")) cat("done\n")
      pkplotRV$highlighted <- NULL
      
      range <- pk_time_range() %>% dplyr::filter(profid == input$select_profile)
      
      pkplotRV$range <- range
      
      updateSliderInput(session, "pk_xRange",
                        min = range$min_x, max = range$max_x,
                        value = c(range$min_x, range$max_x))
      
      pkplotRV$new_profile <- FALSE
    } else {
      if (PKPDEnv("verbose")) cat("not done\n")
    }
  })
  return(TRUE)
})


#----------------------------------------#
######   orig scale                 ######
#----------------------------------------#


base_plot <- reactive({
  req(profile_data(), tail_fit())
  
  if (PKPDEnv("verbose")) cat("base_plot\n")
  
  profile_data_keep <- profile_data()
  fit_list <- tail_fit()
  
  time_unit <- ""
  if ("time_unit" %in% colnames(profile_data_keep)) 
    time_unit <- paste0("(", profile_data_keep$time_unit[1], ")")
  
  conc_unit <- ""
  if ("conc_unit" %in% colnames(profile_data_keep)) 
    conc_unit <- profile_data_keep$conc_unit[1]
  
  # Mark points used in the tail
  if (is.list(fit_list))
    profile_data_keep$col_flag[profile_data_keep$plot_time %in% fit_list$time_points]  <- "In-tail"
  
  
  profile_data_keep$line_col <- "Non-tail"
  
  p <- pkPlot(profile_data_keep, time = "plot_time", value = "aval",
              colour = "col_flag", shape = "col_flag", size = "col_flag")
  
  
  if (input$interpolation_orig)
    p <- p + pkPlot_line(mapping = ggplot2::aes(color = line_col, shape = line_col, size = line_col),
                         data = . %>%  dplyr::filter(anelfl == "Y"))
  
  if (input$tailfit_orig && is.list(fit_list))
    p <- p + pkPlot_tailFit(fit_list$best_fit, colour = "#3F9C35")
  
  p <- p + pkPlot_dot()
  
  if (input$mark_lloq == "Non-filled point(s)")
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(lower_conc != upper_conc), color = "white", size = 1.5, show.legend = FALSE)
  
  
  if (input$mark_lloq == "Vertical line(s)")
    p <- p + pkPlot_lloq(data = profile_data_keep,  time = "plot_time",
                         lower = "lower_conc", upper = "upper_conc")
  
  if ("highlight" %in% colnames(profile_data_keep))
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(highlight != ""),
                        ggplot2::aes(colour = highlight, shape = highlight, size = highlight))
  
  p <- p + 
    ggplot2::labs(#title = paste("PK-profile for", profile),
      x = paste("Time", time_unit),
      y = paste("Concentration", conc_unit))
  
  
  # hard coded color scheme
  NNcol <- list(dark_blue = "#001965", black = "#231F20",
                forest_green = "#3F9C35", spring_green = "#C1D82F",
                lava_red = "#E64A0E", golden_yellow = "#EAAB00")
  
  labels <- c("Tail", "In-tail", "Non-tail", "Excluded")
  
  if ("highlight" %in% colnames(profile_data_keep)) {
    high_label <- unique(profile_data_keep$highlight[profile_data_keep$highlight != ""])
    
    high_col <- nncol$company[ match(c( "sunset_orange", "golden_yellow",  
                                        "light_blue", "granite_grey", "lava_red", 
                                        "forest_green", "ocean_blue",
                                        "dark_blue", "sky_blue",    "misty_blue", 
                                        "grass_green", "lime_green"), names(nncol$company))][
                                          seq_along(high_label)]
    
    high_shape <- rep(1, length(high_label))
    
    high_sizes <- rep(8, length(high_label))
  } else {
    high_label <- high_col <- high_shape <- high_sizes <- c()
  }
  
  colors <- unlist(c(NNcol$spring_green, NNcol$forest_green, NNcol$dark_blue, NNcol$lava_red))
  
  colors <- c(colors, high_col, NNcol$lava_red, NNcol$golden_yellow)
  shapes <- c(rep(19, length(labels)), high_shape, 1, 4)
  sizes  <- c(rep( 4, length(labels)), high_sizes, 9, 5)
  
  names(colors) <- c(labels, high_label, "Review comment","Lab comment")
  names(shapes) <- c(labels, high_label, "Review comment","Lab comment")
  names(sizes)  <- c(labels, high_label, "Review comment","Lab comment")
  
  p + ggthemes::theme_tufte(base_size = 18, base_family = "sans") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 18)) +
    ggplot2::scale_color_manual(breaks = names(colors), values = colors) +
    ggplot2::scale_shape_manual(breaks = names(shapes), values = shapes) +
    ggplot2::scale_size_manual(breaks =  names(sizes), values = sizes)
})




output$plot_profile <- renderPlot({
  if (PKPDEnv("verbose")) cat("render plot - 1\n")
  req(new_ranges_by_prof(), base_plot(), PK_plot_comment_profile(), profile_data(), plot_range())
  if (PKPDEnv("verbose")) cat("render plot - 2\n")
  
  isolate({
    print("pkplotRV$profile_select_ui: ")
    print(pkplotRV$profile_select_ui)
    print("input$select_profile: ")
    print(input$select_profile)
    if (!is.null(pkplotRV$profile_select_ui) && pkplotRV$profile_select_ui != input$select_profile)
      return(NULL)
    pkplotRV$profile_select_ui <- NULL
    if (PKPDEnv("verbose")) cat(paste("render_base_plot", input$select_profile, "\n"))
  })
  
  comments_data_keep <- PK_plot_comment_profile()
  
  range <- range_keep <- plot_range()
  
  breaks <- pretty(c(range$min_y, range$max_y), n = 4)
  
  breaks <- breaks[breaks >= range$min_y & breaks <= range$max_y]
  
  n_decimals <- nchar(strsplit(paste(min(breaks[breaks > 0])),"\\.")[[1]][2])
  
  accuracy <- ifelse(is.na(n_decimals), 1, 10^-n_decimals)
  
  p <- base_plot() + ggplot2::scale_y_continuous(breaks = breaks,
                                            labels = scales::number_format(accuracy = accuracy)) +
    ggplot2::coord_cartesian(xlim = c(range$min_x, range$max_x),
                             ylim = c(range$min_y, range$max_y)) +
    # ggplot2::xlim(range$min_x, range$max_x) +
    # ggplot2::ylim(range$min_y, range$max_y) +
    pkPlot_dot(mapping = ggplot2::aes(plot_time, aval),
               data = . %>%  dplyr::filter(plot_time %in% pk_plot_highlight_points()),
               size = 7, stroke = 1.5,
               show.legend = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(plot_time, aval,
                                               color = type_comment, shape = type_comment, size = type_comment),
                        data = comments_data_keep,  stroke = 1.5,
                        show.legend = FALSE, na.rm = TRUE) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 3, byrow = TRUE))
  
  if (input$mark_lloq == "Non-filled point(s)")
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(lower_conc != upper_conc) %>% dplyr::filter(plot_time %in% pk_plot_highlight_points()), 
                    color = "white", size = 4, show.legend = FALSE)
  
  p
})

#----------------------------------------#
######         log scale            ######
#----------------------------------------#

base_plot_log <- reactive({
  req(profile_data(), tail_fit())
  if (PKPDEnv("verbose")) cat("base_plot_log\n")
  
  profile_data_keep <- profile_data()
  fit_list <- tail_fit()
  
  time_unit <- ""
  if ("time_unit" %in% colnames(profile_data_keep)) 
    time_unit <- paste0("(", profile_data_keep$time_unit[1], ")")
  
  conc_unit <- ""
  if ("conc_unit" %in% colnames(profile_data_keep)) 
    conc_unit <- profile_data_keep$conc_unit[1]
  
  
  # Mark points used in the tail
  if (is.list(fit_list))
    profile_data_keep$col_flag[profile_data_keep$plot_time %in% fit_list$time_points]  <- "In-tail"
  
  profile_data_keep$line_col <- "Non-tail"
  
  p <- pkPlot(profile_data_keep, time = "plot_time", value = "aval",
              colour = "col_flag", shape = "col_flag", size = "col_flag", 
              log = TRUE)
  
  if (input$interpolation_log)
    p <- p + pkPlot_line(mapping = ggplot2::aes(color = line_col, shape = line_col, size = line_col),
                         data = . %>%  dplyr::filter(anelfl == "Y"))
  
  
  if (input$tailfit_log && is.list(fit_list))
    p <- p + pkPlot_tailFit(fit_list$best_fit, log = TRUE, colour = "#3F9C35",  show.legend = FALSE)
  
  p <- p + pkPlot_dot()
  
  if (input$mark_lloq == "Non-filled point(s)")
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(lower_conc != upper_conc), color = "white", size = 1.5, show.legend = FALSE)
    
  
  if (input$mark_lloq == "Vertical line(s)")
    p <- p + pkPlot_lloq(data = profile_data_keep,  time = "plot_time",
                         lower = "lower_conc", upper = "upper_conc", log = TRUE)
  
  p <- p + 
    ggplot2::labs(#title = paste("PK-profile for", profile),
      x = paste("Time", time_unit),
      y = paste("Concentration", conc_unit, "- log scale"))
  
  
  if ("highlight" %in% colnames(profile_data_keep))
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(highlight != ""),
                        ggplot2::aes(colour = highlight, shape = highlight, size = highlight))
  
  # hard coded color scheme
  NNcol <- list(dark_blue = "#001965", black = "#231F20",
                forest_green = "#3F9C35", spring_green = "#C1D82F",
                lava_red = "#E64A0E", golden_yellow = "#EAAB00")
  
  
  labels <- c("Tail", "In-tail", "Non-tail", "Excluded")
  
  
  if ("highlight" %in% colnames(profile_data_keep)) {
    high_label <- unique(profile_data_keep$highlight[profile_data_keep$highlight != ""])
    
    high_col <- nncol$company[ match(c("sunset_orange", "golden_yellow",  
                                       "light_blue", "granite_grey", "lava_red",
                                       "forest_green", "ocean_blue",
                                       "dark_blue", "sky_blue",    "misty_blue", 
                                       "grass_green", "lime_green"), names(nncol$company))][
                                         seq_along(high_label)]
    
    high_shape <- rep(1, length(high_label))
    
    high_sizes <- rep(8, length(high_label))
  } else {
    high_label <- high_col <- high_shape <- high_sizes <- c()
  }
  
  colors <- unlist(c(NNcol$spring_green, NNcol$forest_green, NNcol$dark_blue, NNcol$lava_red))
  
  colors <- c(colors, high_col, NNcol$lava_red, NNcol$golden_yellow)
  shapes <- c(rep(19, length(labels)), high_shape, 1, 4)
  sizes  <- c(rep( 4, length(labels)), high_sizes, 9, 5)
  
  names(colors) <- c(labels, high_label, "Review comment","Lab comment")
  names(shapes) <- c(labels, high_label, "Review comment","Lab comment")
  names(sizes)  <- c(labels, high_label, "Review comment","Lab comment")
  
  p + ggthemes::theme_tufte(base_size = 18, base_family = "sans") +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom",
                   plot.title = ggplot2::element_text(hjust = 0.5, size = 18)) +
    ggplot2::scale_color_manual(breaks = names(colors), values = colors) +
    ggplot2::scale_shape_manual(breaks = names(shapes), values = shapes) +
    ggplot2::scale_size_manual(breaks =  names(sizes), values = sizes)
})


output$plot_log_profile <- renderPlot({
  req(new_ranges_by_prof(), base_plot_log(),PK_plot_comment_profile(), profile_data(), plot_range())
  
  isolate({
    if (!is.null(pkplotRV$profile_select_ui) && pkplotRV$profile_select_ui != input$select_profile)
      return(NULL)
    pkplotRV$profile_select_ui <- NULL
    if (PKPDEnv("verbose")) cat(paste("render_base_plot_log", input$select_profile, "\n"))
  })
  
  comments_data_keep <- PK_plot_comment_profile()
  
  range <- plot_range()
  
  org_breaks <- pretty(c(range$min_y, range$max_y), n = 4)
  
  min_org_breaks <- org_breaks[org_breaks > 0] %>% min()
  
  second_break <- ifelse(floor(log10(min_org_breaks/4)) == floor(log10(min_org_breaks)) | min_org_breaks/4 <= 2.5*10^(floor(log10(min_org_breaks/4))),
                        plyr::round_any(min_org_breaks/4, 10^(floor(log10(min_org_breaks/4)))),
                        plyr::round_any(min_org_breaks/4, 5*10^(floor(log10(min_org_breaks/4)))))
  
  first_break <- ifelse(floor(log10(min_org_breaks/32)) == floor(log10(min_org_breaks/4)) | min_org_breaks/32 <= 2.5*10^(floor(log10(min_org_breaks/32))),
                        plyr::round_any(min_org_breaks/32, 10^(floor(log10(min_org_breaks/32)))),
                        plyr::round_any(min_org_breaks/32, 5*10^(floor(log10(min_org_breaks/32)))))
  
  breaks <- c(first_break, second_break, org_breaks) %>% 
                unique()
  
  breaks <- breaks[log(breaks) >= range$min_log_y & log(breaks) <= range$max_log_y]
  
  log_breaks <- log(breaks)
  
  # log_breaks <- log_breaks[c(1, 1 + which(diff(log_breaks) >= 0.2))]

  n_decimals <- nchar(strsplit(paste(min(breaks)),"\\.")[[1]][2])

  accuracy <- ifelse(is.na(n_decimals), 1, 10^-n_decimals)
  
  p <- base_plot_log()
  
  p <- p + ggplot2::scale_y_continuous(breaks = log_breaks,
                                       labels = scales::trans_format("exp", scales::number_format(accuracy = accuracy))) + 
      ggplot2::coord_cartesian(xlim = c(range$min_x, range$max_x),
                               ylim = c(range$min_log_y, range$max_log_y))
  
  p <- p + pkPlot_dot(mapping = ggplot2::aes(plot_time, aval),
                  data = . %>%  dplyr::filter(plot_time %in% pk_plot_highlight_points()),
                  size = 7, stroke = 1.5,
                  show.legend = FALSE) +
    ggplot2::geom_point(mapping = ggplot2::aes(plot_time, log_aval,
                                               color = type_comment, shape = type_comment, size = type_comment),
                        data = comments_data_keep,  stroke = 1.5,
                        show.legend = FALSE, na.rm = TRUE) +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 3, byrow = TRUE))
  
  if (input$mark_lloq == "Non-filled point(s)")
    p <- p + pkPlot_dot(data = . %>%  dplyr::filter(lower_conc != upper_conc) %>% dplyr::filter(plot_time %in% pk_plot_highlight_points()), 
                        color = "white", size = 4, show.legend = FALSE)
  
  p
})

#----------------------------------------#
######      Setup_highlight         ######
#----------------------------------------#


profile_data_for_high <- reactive({
  req(profile_data())
  
  profile_data() %>% dplyr::select(plot_time, aval, lower_conc, upper_conc) %>%
    dplyr::group_by(plot_time) %>%
    tidyr::gather(key = "type", value = "aval", aval, lower_conc, upper_conc, na.rm = TRUE) %>%
    dplyr::ungroup() %>% dplyr::mutate(log_aval = log(aval))
})

# original scale dbl click
observeEvent(input$plot_profile_dblclick, {
  output$pk_zoom_help <- NULL
  brush    <- input$plot_profile_brush
  range <- pk_time_range() %>% dplyr::filter(profid == input$select_profile)
  
  if (is.null(brush)) {
    click_at <- input$plot_profile_dblclick
    
    profile_data2 <- profile_data_for_high()
    
    brushed_data <- nearPoints(profile_data2, click_at, "plot_time", "aval")
    
    # subdata <- dplyr::select(brushed_data, time) %>% dplyr::distinct()
    
    if (nrow(brushed_data) == 0) {
      pkplotRV$highlighted <- NULL
      
      # clear zoom
      pkplotRV$range <- range
      updateSliderInput(session, "pk_xRange",  value = c(range$min_x, range$max_x))
      
      
    } else if (unique(brushed_data$plot_time) %in% pkplotRV$highlighted) {
      pkplotRV$highlighted <- setdiff(pkplotRV$highlighted, unique(brushed_data$plot_time))
    }else {
      pkplotRV$highlighted <- c(unique(brushed_data$plot_time), pkplotRV$highlighted)
    }
    
    DT::selectRows(DTproxy, NULL)
  } else {
    pkplotRV$range$min_x <- floor(max(brush$xmin, range$min_x))
    pkplotRV$range$max_x <- ceiling(min(brush$xmax, range$max_x))
    
    pkplotRV$range$min_y <- max(brush$ymin, range$min_y, na.rm = TRUE)
    pkplotRV$range$max_y <- min(brush$ymax, range$max_y, na.rm = TRUE)
    
    pkplotRV$range$min_log_y <- max(log(pkplotRV$range$min_y), range$min_log_y, na.rm = TRUE)
    pkplotRV$range$max_log_y <- min(log(pkplotRV$range$max_y), range$max_log_y, na.rm = TRUE)
    
    updateSliderInput(session, "pk_xRange", value = c(pkplotRV$range$min_x, pkplotRV$range$max_x))
  }
})


# select point by double click in log scale
observeEvent(input$plot_log_profile_dblclick, {
  output$pk_zoom_help <- NULL
  brush    <- input$plot_log_profile_brush
  range <- pk_time_range() %>% dplyr::filter(profid == input$select_profile)
  
  if (is.null(brush)) {
    click_at <- input$plot_log_profile_dblclick
    profile_data2 <- profile_data_for_high()
    
    brushed_data <- nearPoints(profile_data2, click_at, "plot_time", "log_aval")
    
    # subdata <- dplyr::select(brushed_data, time) %>% dplyr::distinct()
    
    if (nrow(brushed_data) == 0) {
      pkplotRV$highlighted <- NULL
      # clear zoom
      pkplotRV$range <- range
      updateSliderInput(session, "pk_xRange",  value = c(range$min_x, range$max_x))
      
    } else if (unique(brushed_data$plot_time) %in% pkplotRV$highlighted) {
      pkplotRV$highlighted <- setdiff(pkplotRV$highlighted, unique(brushed_data$plot_time))
    }else {
      pkplotRV$highlighted <- c(unique(brushed_data$plot_time), pkplotRV$highlighted)
    }
    
    DT::selectRows(DTproxy, NULL)
  } else {
    
    pkplotRV$range$min_x <- floor(max(brush$xmin, range$min_x))
    pkplotRV$range$max_x <- ceiling(min(brush$xmax, range$max_x))
    
    pkplotRV$range$min_log_y <- max(brush$ymin, range$min_log_y, na.rm = TRUE)
    pkplotRV$range$max_log_y <- min(brush$ymax, range$max_log_y, na.rm = TRUE)
    
    pkplotRV$range$min_y <- exp(pkplotRV$range$min_log_y)
    pkplotRV$range$max_y <- exp(pkplotRV$range$max_log_y)
    
    updateSliderInput(session, "pk_xRange", value = (c(pkplotRV$range$min_x, pkplotRV$range$max_x)))
  }
})


# Select brushed points in original scale.
observeEvent(input$plot_profile_click, {
  output$pk_zoom_help <- NULL
  brush <- input$plot_profile_brush
  
  if (!is.null(brush)) {
    profile_data2 <- profile_data_for_high()
    
    brushed_data <- brushedPoints(profile_data2, brush, "plot_time", "aval")
    
    # subdata <- dplyr::select(brushed_data, time) %>% dplyr::distinct()
    
    if (nrow(brushed_data) == 0) {
      pkplotRV$highlighted <- NULL
    } else {
      pkplotRV$highlighted <- unique(brushed_data$plot_time)
    }
    
    DT::selectRows(DTproxy, NULL)
  }
  else {
    return(NULL)
  }
})

# Select brushed points in log scale.
observeEvent(input$plot_log_profile_click, {
  output$pk_zoom_help <- NULL
  brush <- input$plot_log_profile_brush
  
  if (!is.null(brush)) {
    profile_data2 <- profile_data_for_high()
    
    brushed_data <- brushedPoints(profile_data2, brush, "plot_time", "log_aval")
    
    # subdata <- dplyr::select(brushed_data, time) %>% dplyr::distinct()
    
    if (nrow(brushed_data) == 0) {
      pkplotRV$highlighted <- NULL
    } else {
      pkplotRV$highlighted <- unique(brushed_data$plot_time)
    }
    
    DT::selectRows(DTproxy, NULL)
  }
  else {
    return(NULL)
  }
})


observeEvent(input$plot_profile_brush, {
  output$pk_zoom_help <-
    renderUI(tags$h1("click to choose or double click to zoom",
                     style = "color:blue;"))
})

#-----------------------------------------#
######      comment hover            ######
#-----------------------------------------#


PK_plot_comment <- reactive({
  req(PK_comments_data(), pkdata())
  
  createPlotComment(data = pkdata(), comments = PK_comments_data())
})




#Decouple profile comments from other comments

PK_plot_comment_profile <- reactive({
  req(PK_plot_comment(), input$select_profile)
  PK_plot_comment() %>% dplyr::filter(profid == input$select_profile)
}) %>% dedupe()

output$plot_profile_hover_info <- renderUI({
  req(nrow(PK_plot_comment_profile()) > 0 | nrow(profile_data()) > 0)
  
  hover <- input$plot_profile_hover
  
  #print("plot_profile_hover_info")
  
  PK_plot_comment_profile_keep <- PK_plot_comment_profile()
  
  if (nrow(PK_plot_comment_profile_keep)) {
    point <- nearPoints(PK_plot_comment_profile_keep, hover, xvar = "plot_time", yvar = "aval",
                      threshold = 10, maxpoints = NULL, addDist = TRUE)
  } else {
    point <- PK_plot_comment_profile_keep
  }
  #print("plot_profile_hover_info point done")
  #print(point)
  if (nrow(point) > 0) {
    point_keep <- point
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    to_print <- glue::glue('{tags$b("Time:")} {round(point_keep$plot_time,2)}   ', 
                           '{tags$b("Conc:")} {round(point_keep$aval,2)}<br/>',
                           '{tags$b("Nominal time:")} {round(point_keep$time,2)}<br/>',
                           '{tags$b("Comment by:")} {point_keep$user}<br/>', 
                           '{tags$b("Comment:")} {point_keep$comment}<br/>')
    
    to_print2 <- glue::glue_collapse(to_print, sep = "<br/>")
  } else {
    PK_plot_profile_keep <- profile_data()
    
    point <- nearPoints(PK_plot_profile_keep, hover, xvar = "plot_time", yvar = "aval",
                        threshold = 10, maxpoints = NULL, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)    
    
    point_keep <- point
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure the tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    to_print <- glue::glue('{tags$b("Time:")} {round(point_keep$plot_time,2)}   ',
                           '{tags$b("Conc:")} {round(point_keep$aval,2)}<br/>',
                           '{tags$b("Nominal time:")} {round(point_keep$time,2)}<br/>')
    
    to_print2 <- glue::glue_collapse(to_print, sep = "<br/>")
  }
  
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(to_print2))
  )
})

output$plot_log_profile_hover_info <- renderUI({
  req(nrow(PK_plot_comment_profile()) > 0 | nrow(profile_data()) > 0)
  hover <- input$plot_log_profile_hover
  
  PK_plot_comment_profile_keep <- PK_plot_comment_profile()
  
  if (nrow(PK_plot_comment_profile_keep)) {
    point <- nearPoints(PK_plot_comment_profile_keep, hover, xvar = "plot_time", yvar = "log_aval",
                        threshold = 5, maxpoints = NULL, addDist = TRUE)
  } else {
    point <- PK_plot_comment_profile_keep
  }
  
  if (nrow(point) > 0) {
    point_keep <- point
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left: -100%")
    
    to_print <- glue::glue('{tags$b("Time:")} {round(point_keep$plot_time,2)}   ',
                           '{tags$b("Conc:")} {round(exp(point_keep$log_aval),2)}<br/>',
                           '{tags$b("Nominal time:")} {round(point_keep$time,2)}<br/>',
                           '{tags$b("Comment by:")} {point_keep$user}<br/>',
                           '{tags$b("Comment:")} {point_keep$comment}<br/>')
    
    to_print2 <- glue::glue_collapse(to_print, sep = "<br/>")
  } else {
    PK_plot_profile_keep <- profile_data()
    
    PK_plot_profile_keep$log_aval <- log(PK_plot_profile_keep$aval)
    
    point <- nearPoints(PK_plot_profile_keep, hover, xvar = "plot_time", yvar = "log_aval",
                        threshold = 5, maxpoints = NULL, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)    
    
    point_keep <- point
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:relative; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left: -100%")
    
    to_print <- glue::glue('{tags$b("Time:")} {round(point_keep$plot_time,2)}   ',
                           '{tags$b("Conc:")} {round(exp(point_keep$log_aval),2)}<br/>',
                           '{tags$b("Nominal time:")} {round(point_keep$time,2)}<br/>')
    
    to_print2 <- glue::glue_collapse(to_print, sep = "<br/>")
  }
  
  # actual tooltip created as wellPanel
  div(
    style = paste0("position:absolute;", "left:", left_px - 2, "px; top:", top_px + 2, "px;"),
    wellPanel(
      style = style,
      p(HTML(to_print2))
    )
  )
})


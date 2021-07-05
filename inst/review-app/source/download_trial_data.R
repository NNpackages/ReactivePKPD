#----------------------------------------#
######     Download trial data      ######
#----------------------------------------#

output$download_trial_zip <- downloadHandler(
  filename = function() {
    paste('PKPD-review-data-', Sys.Date(), '.zip', sep = '')
  },
  content = function(con) {
    
    review_access_all <- GlobalRV$review_access
    review_access <- review_access_keep <-
      review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]
    
    files <- file.path(PKPDEnv("access_data_path"),
                       c("trial_setup", "trial_data"),
                       paste0(c("access_", "data_"), review_access$trial_id))
    
    files <- files[dir.exists(files)]
    
    if (length(files))
      zip::zipr(zipfile = con, files)
    
  },
  contentType = "application/zip"
)

output$download_trial_comments <- downloadHandler(
  filename = function() {
    paste('PK-review-comments-', Sys.Date(), '.csv', sep = '')
  },
  content = function(con) {
    
    all_comments <- PK_comments_data()
    
    writeTable(all_comments, con,
               which_sep3 = c("edit_time", "view_range", "time_points", "reply"))
  }
)

output$pd_download_trial_comments <- downloadHandler(
  filename = function() {
    paste('PD-review-comments-', Sys.Date(), '.csv', sep = '')
  },
  content = function(con) {
    
    all_comments <- pd_comments_data()
    
    writeTable(all_comments, con,
               which_sep3 = c("edit_time", "view_range", "time_points", "reply"))
  }
)

output$download_minutes <- downloadHandler(
  filename = function() {
    paste('PK-review-minutes-', Sys.Date(), '.docx', sep = '')
  },
  
  content = function(con) {
    
    review_access_all <- all_trial_data()
    
    trial_info_keep <- trial_info()
    
    access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
    
    all_comments <- PK_comments_data()
    
    selected_comments <- all_comments %>% dplyr::filter(status %in% input$min_status)
    
    
    if (input$incl_replies == FALSE)
      selected_comments <- selected_comments %>% dplyr::mutate(reply = list(list()))
    
    if(is.null(input$min_excused))
      excused <- ""
    else
      excused <- input$min_excused
    
    doc <- minutes(project = access_info$Project, trial = access_info$Trial, 
                   participants = input$min_partic,
                   excused = excused,
                   ref = ifelse(input$min_ref == "None", "", input$min_ref),
                   comments = selected_comments,
                   overall_desc = "Add an overall description here")
    
    print(doc, target = con)
  }
)

output$pd_download_minutes <- downloadHandler(
  filename = function() {
    paste('PD-review-minutes-', Sys.Date(), '.docx', sep = '')
  },
  
  content = function(con) {
    
    review_access_all <- all_trial_data()
    
    trial_info_keep <- trial_info()
    
    access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
    
    all_comments <- pd_comments_data()
    
    selected_comments <- all_comments %>% dplyr::filter(status %in% input$min_status)
    
    if (input$incl_replies == FALSE)
      selected_comments <- selected_comments %>% dplyr::mutate(reply = list(list()))
    
    if(is.null(input$min_excused))
      excused <- ""
    else
      excused <- input$min_excused
    
    doc <- minutesPD(project = access_info$Project, trial = access_info$Trial, 
                     participants = input$min_partic,
                     excused = excused,
                     ref = ifelse(input$min_ref == "None", "", input$min_ref),
                     comments = selected_comments,
                     overall_desc = "Add an overall description here")
    
    print(doc, target = con)
  }
)

output$minutes_ui <- renderUI({
  req(trial_info())
  
  validate(need(setupRV$user_type == "superuser" , "Sorry you don't have permission to create minutes for this trial"))
  
  review_access_all <- all_trial_data()
  
  trial_info_keep <- trial_info()
  
  access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
  
  reviewers <- trial_info_keep$reviewers
  
  tagList(
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Minutes", status = "primary",
      splitLayout(cellWidths = c("50%","30%", "20%"), cellArgs = list(style = "padding-right: 15px"),
                  selectizeInput("min_partic", "Participants:", choices = toupper(reviewers),
                                 selected = toupper(reviewers), multiple = TRUE),
                  selectizeInput("min_excused", "Excused:", choices = c(toupper(reviewers)), multiple = TRUE),
                  selectizeInput("min_ref", "Keeper of the minutes:", choices = c("None", toupper(reviewers)), 
                                 selected = setupRV$user, multiple = FALSE)
                  ),
      splitLayout(cellWidths = c(paste0(100/2, "%"), paste0(100/2, "%")), cellArgs = list(style = "padding-right: 15px"),
                  selectizeInput("min_status", "Only incl. comments with following status:",
                                 choices = c("Final", "Approved", "No action", "Query", "Re-evaluate", "Awaiting approval"), 
                                 selected = c("Final", "Approved", "No action", "Query", "Re-evaluate", "Awaiting approval"), multiple = TRUE),
                  tags$div(
                    br(),
                    strong("Incl. replies: "),
                    shinyWidgets::prettyCheckbox(inputId = "incl_replies",
                                                 label = "Yes", value = TRUE, inline = TRUE)
                  )
                  ),
      
      uiOutput("download_minutes_ui"),
      tags$div(br()),
      uiOutput("pd_download_minutes_ui")
      
      
    ),
    
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Accompanying figures", status = "primary",
      
      h3("Header titles"),
      splitLayout(cellWidths = c(rep(paste0(98/4, "%"), 4), "2%"), cellArgs = list(style = "padding-right: 15px"),
                  textInput("PK_EOT_title1", "Title 1", paste0("NN", access_info$Project, "-", access_info$Trial)),
                  textInput("PK_EOT_title2", "Title 2", "First draft"),
                  textInput("PK_EOT_title3", "Title 3", "Profiles"),
                  textInput("PK_EOT_title4", "Title 4", "Not validated"),
                  tags$div()
      ),
      h3("Header other"),
      splitLayout(cellWidths = c(rep(paste0(98/3, "%"), 3), "2%"), cellArgs = list(style = "padding-right: 15px"),
                  selectInput("PK_EOT_status", "Status", choices = c("Draft", "Review", "Final")),
                  textInput("PK_EOT_version", "Version", "0.1"),
                  textInput("PK_EOT_date",  "Date", format(Sys.Date(), "%d %b %Y")),
                  tags$div()
      ),
      
      splitLayout(cellWidths = c("250px","250px"), uiOutput("createWordPlot_button_ui"), uiOutput("download_plots_ui")),
      tags$div(br()),
      splitLayout(cellWidths = c("250px","250px"), uiOutput("pd_createWordPlot_button_ui"), uiOutput("pd_download_plots_ui"))
    )
  ) 
})

output$download_minutes_ui <- renderUI({
  req(length(list.files(trial_info()$trial_pk_data_dir, pattern = "app_adpc.rds")) > 0)
  
  shiny::downloadButton("download_minutes", "Download PK minutes")
})

output$createWordPlot_button_ui <- renderUI({
  req(length(list.files(trial_info()$trial_pk_data_dir, pattern = "app_adpc.rds")) > 0)
  
  shiny::actionButton("createWordPlot_button", "Create document with PK figures")
})

output$pd_download_minutes_ui <- renderUI({
  req(length(list.files(trial_info()$trial_pd_data_dir, pattern = "app_adpd.rds")) > 0)
  
  shiny::downloadButton("pd_download_minutes", "Download PD minutes")
})

output$pd_createWordPlot_button_ui <- renderUI({
  req(length(list.files(trial_info()$trial_pd_data_dir, pattern = "app_adpd.rds")) > 0)
  
  shiny::actionButton("pd_createWordPlot_button", "Create document with PD figures")
})


observeEvent(input$createWordPlot_button, {
  
  review_access_all <- all_trial_data()
  
  trial_info_keep <- trial_info()
  
  access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
  
  ui <- tagList(
    p("Please be aware that this action takes a couple of minutes and will freeze the app during that time. This will also affect other online users.", style = "color:red;"),
    p(em("You can check who is online in the 'Online user' tab."))
  )
  
  showModal(modalDialog(title = glue::glue("Create word document consisting of all PK figures for NN", access_info$Project, "-", access_info$Trial),
                        footer = tagList(modalButton("Cancel", icon("remove")),
                                         actionButton("createWordPlot", "Create document")),
                        ui))
})

word_plots <- eventReactive(input$createWordPlot, {
  
  if (PKPDEnv("verbose")) cat("generating word plots\n")
  
  removeModal()
  
  review_access_all <- all_trial_data()
  
  trial_info_keep <- trial_info()
  
  access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
  
  all_comments <- PK_comments_data()
  
  selected_comments <- all_comments %>% dplyr::filter(status %in% input$min_status)
  
  pkdata_all <- pkdata()
  
  title   <- c(input$PK_EOT_title1, input$PK_EOT_title2, input$PK_EOT_title3, input$PK_EOT_title4)
  status  <- input$PK_EOT_status 
  version <- input$PK_EOT_version
  date    <- input$PK_EOT_date
  
  
  plot_data <- plyr::dlply(pkdata_all, "profid", tibble::as_tibble)
  
  con <- file.path("access_info$app_data",
                   paste('PK-review-plots', '.docx', sep = ''))
  
  future({
    future.apply::future_lapply(plot_data, create_comb_plot,
                                comments = createPlotComment(pkdata_all, selected_comments))
  }) %...>% wordplot( comments = createTableComment(data = pkdata_all, comments = selected_comments),
                      title = title,
                      status = status, version = version,
                      date = date) 
})

output$download_plots_ui <- renderUI({
  #req(!is.null(word_plots()) && class(word_plots()) != "promise")
  req(word_plots())
  
  shiny::downloadButton("download_plots", "Download PK figures")
})
# 
output$download_plots <- downloadHandler(
  filename = function() {
    paste('PK-review-plots-', Sys.Date(), '.docx', sep = '')
  },
  
  content = function(con) {
    
    word_plots() %...>% print(target = con) 
    
  }
)

observeEvent(input$pd_createWordPlot_button, {
  
  review_access_all <- all_trial_data()
  
  trial_info_keep <- trial_info()
  
  access_info <- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
  
  ui <- tagList(
    p("Please be aware that this action takes a couple of minutes and will freeze the app during that time. This will also affect other online users.", style = "color:red;"),
    p(em("You can check who is online in the 'Online user' tab."))
  )
  
  showModal(modalDialog(title = glue::glue("Create word document consisting of all PD figures for NN", access_info$Project, "-", access_info$Trial),
                        footer = tagList(modalButton("Cancel", icon("remove")),
                                         actionButton("pd_createWordPlot", "Create document")),
                        ui))
})

pd_word_plots <- eventReactive(input$pd_createWordPlot, {
  
  if (PKPDEnv("verbose")) cat("generating word plots\n")
  
  req(pd_plot_data(),
      setup_pd_panel_data(),
      setup_pd_plots_data(), 
      pd_col_scheme())
  
  removeModal()
  
  review_access_all <- all_trial_data()
  
  trial_info_keep <- trial_info()
  
  access_info <<- review_access_all[review_access_all$trial_id == trial_info_keep$trial_id, ]
  
   pd_plot_data_keep <<-  pd_plot_data()
  panel_data <- setup_pd_panel_data_keep <<- setup_pd_panel_data()
  plot_data <- setup_pd_plots_data_keep <<- setup_pd_plots_data()
  col_scheme <- pd_col_scheme_keep <<-pd_col_scheme()
  
  all_comments <- pd_comments_data()
  
  selected_comments <- all_comments %>% dplyr::filter(status %in% input$min_status)

  title <- title_keep <- c(input$PK_EOT_title1, input$PK_EOT_title2, input$PK_EOT_title3, input$PK_EOT_title4)
  status <- status_keep  <- input$PK_EOT_status 
  version <- version_keep <- input$PK_EOT_version
  date  <- date_keep  <- input$PK_EOT_date
  
  
  profile_data <-  plyr::dlply(pd_plot_data_keep, "profid", tibble::as_tibble)
  
  con <- file.path("access_info$app_data",
                   paste('PD-review-plots', '.docx', sep = ''))
 
  future({
    future.apply::future_lapply(profile_data, create_base_pd_plot,
                      comments = selected_comments, 
                      panel_data = setup_pd_panel_data_keep,
                      plot_data = setup_pd_plots_data_keep,
                      col_scheme = pd_col_scheme_keep)
  }) %...>% wordplot(comments = createTablePDComment(selected_comments),
                     title = title,
                     type = "PD",
                     status = status, version = version,
                     date = date)
})

output$pd_download_plots_ui <- renderUI({
  #req(!is.null(word_plots()) && class(word_plots()) != "promise")
  req(pd_word_plots())
  
  shiny::downloadButton("pd_download_plots", "Download PD figures")
})
# 
output$pd_download_plots <- downloadHandler(
  filename = function() {
    paste('PD-review-plots-', Sys.Date(), '.docx', sep = '')
  },
  
  content = function(con) {
    
    pd_word_plots() %...>% print(target = con) 
    
  }
)
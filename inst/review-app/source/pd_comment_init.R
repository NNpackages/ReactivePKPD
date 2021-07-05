#---------------------------------------------#
######        comment connection         ######
#---------------------------------------------#

# Initialise the comments if it is not allready initialised
pd_comments_init <- reactive({
  if (PKPDEnv("verbose")) cat("pd_comments_init\n")
  
  req(trial_info(), pd_all_profiles())
  trial_info <- trial_info()
  
  isolate({
    
    if (is.null(GlobalRV$pd_comment[[trial_info$trial_id]]) || !identical(attr(GlobalRV$pd_comment[[trial_info$trial_id]], "users"), trial_info$reviewers)) {
      
      pd_comments_path <- file.path(trial_info$trial_app_dir)
      
      dir.create(pd_comments_path, recursive = TRUE, showWarnings = FALSE)
      
      comment_data <- initCommentData(pd_comments_path, folder_name = "pd_comment_data_folder",
                                      users =  trial_info$reviewers, profiles = pd_all_profiles())
      
      GlobalRV$pd_comment[[trial_info$trial_id]] <<- comment_data
      
    } else {
      GlobalRV$pd_comment[[trial_info$trial_id]]
    }
  })
})


# Update the comment data based on the global reactive
pd_comments_data <- reactive({
  if (PKPDEnv("verbose")) cat("pd_comments_data\n")
  req(pd_comments_init(), trial_info())
  inst_info <- trial_info()
  pd_comments_data_all <- GlobalRV$pd_comment[[inst_info$trial_id]]
  GlobalRV$pd_comment[[inst_info$trial_id]]
})

observe({
  req(pd_comments_data(), input$pd_select_profile)
  if (PKPDEnv("verbose")) cat("pd_comments_data_profile_proxy\n")
  
  new_data <- pd_comments_data() 
  
  if (nrow(new_data)) {
    new_data2 <- new_data %>% dplyr::filter(profile == input$pd_select_profile)
    old_data2 <- old_data <- pdplotRV$pd_comments_data_profile_proxy
    
    attr(new_data2, "review_status") <- NULL
    attr(old_data2, "review_status") <- NULL
    
    if (!identical(new_data2, old_data2)) {
      pdplotRV$pd_comments_data_profile_proxy <- new_data %>% dplyr::filter(profile == input$pd_select_profile)
      if (PKPDEnv("verbose")) cat("pd_comments_data_profile_proxy updated\n")
    }
  } else {
    # test whether I can set this to empty
    pdplotRV$pd_comments_data_profile_proxy <- new_data
  }
 
})


# subset to the currently selected profile
pd_comments_data_profile <- reactive({
  if (PKPDEnv("verbose")) cat("pd_comments_data_profile\n")
  # req(pd_comments_data(), input$select_profile)
  
  pdplotRV$pd_comments_data_profile_proxy
})


#-----------------------------------------#
######    pd profile comment table   ######
#-----------------------------------------#


# show the profile data
output$pd_adpcCom_profile <- DT::renderDataTable({
  
  if (PKPDEnv("verbose")) cat("pd_adpcCom_profile\n")
  req(pd_comments_data_profile())
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  comment_data <- pd_comments_data_profile()
  comment_data <- comment_data[, setdiff(colnames(comment_data), 
                                         c("paramcd", "color_by", "time_point_paramcd"))]
  
  if (is.null(comment_data) || nrow(comment_data) == 0)
    return(NULL)
  
  comment_data$reply <- lapply(comment_data$reply, lapply, lapply, as.character)
  
  comment_data$reply <-
    lapply(comment_data$reply, lapply, paste, collapse = "<br/>", sep = "") %>%
    sapply(paste, collapse = "<br/><br/>", sep = "")
  
  if ("time_points" %in% colnames(comment_data)) {
    if (any(sapply(comment_data$time_points, length))) {
      comment_data$time_points <- lapply(comment_data$time_points, round, digits = 2) %>%
        sapply(paste, collapse = ", ")
    } else {
      comment_data$time_points <- ""
    }
  }
  
  shinyInput <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(id = paste0(id, i), ...))
    }
    inputs
  }
  
  # convert timezone
  time1 <- comment_data$time
  time1 <- .POSIXct(time1, tz = "UTC")
  attr(time1, "tzone") <- Sys.timezone()
  comment_data$time <- time1
  
  comment_data$Status <- comment_data$status
  # set status
  for (i in seq_len(nrow(comment_data))) {
    status <- comment_data[i, "status"]
    
    if (status == "Awaiting approval") {
      status <- "Awaiting"
      stat_button = "btn btn-danger action-button"
      stat_button_class = "glyphicon glyphicon-unchecked"
    }
    if (status == "Approved" | status == "Approve") {
      stat_button = "btn btn-info action-button"
      stat_button_class = "glyphicon glyphicon-edit"
    }
    if (status == "Query") {
      stat_button = "btn btn-primary action-button"
      stat_button_class = "glyphicon glyphicon-share-alt"
    }
    if (status == "Re-evaluate") {
      stat_button = "btn btn-warning action-button"
      stat_button_class = "glyphicon glyphicon glyphicon-repeat"
    }
    if (status == "Final") {
      stat_button = "btn btn-success action-button"
      stat_button_class = "glyphicon glyphicon glyphicon-check"
    }
    if (status == "No action") {
      stat_button = "btn btn-success action-button"
      stat_button_class = "glyphicon glyphicon glyphicon-check"
    }
    comment_data$Status[i] <-
      as.character(tags$button( id = paste0("pd_comment_status_tab_", i),
                                #   type="button",
                                style   = paste0("width: ", validateCssUnit("100px"), ";"),
                                class   = stat_button,
                                onclick = 'Shiny.onInputChange("pd_comment_status_tab",  this.id, {priority: \"event\"})',
                                #tags$span(class = stat_button_class),
                                status))
  }
  
  Reply <-
    shinyInput(tags$button, nrow(comment_data), "pd_write_reply_button_tab_",
               type    = "button",
               class   = "btn btn-info action-button",
               onclick = 'Shiny.onInputChange("pd_write_reply_button_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-share-alt"), "")
  
  Edit <-
    shinyInput(tags$button, nrow(comment_data), "pd_write_edit_button_tab_",
               type    = "button",
               class   = "btn btn-primary action-button",
               onclick = 'Shiny.onInputChange("pd_write_edit_button_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-edit"), "")
  
  Delete <-
    shinyInput(tags$button, nrow(comment_data), "pd_comment_delete_tab_",
               type    = "button",
               class   = "btn btn-danger action-button",
               onclick = 'Shiny.onInputChange("pd_comment_delete_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-trash"), "")
  
  
  comment_data <- comment_data %>% dplyr::select(user, regarding, comment, time_points, time_type, reply, Status)

  
  comment_data_2 <- dplyr::bind_cols(comment_data, Reply = Reply, Edit = Edit, Delete = Delete)
  
  comment_data_2[comment_data_2$user != user & user_type != "superuser", c("Edit", "Delete")] <- c("", "")
  
  DT::datatable(comment_data_2, #class = 'compact striped',
                escape = FALSE,
                rownames = FALSE,
                colnames = c("User", "Regarding", "Comment", "Time",
                             "Time type", "Replies", "Status", "Reply", "Edit", "Delete"),
                selection = list(mode = 'single'),
                options = list(scrollX = TRUE, paging = FALSE,
                               columnDefs = list(list(visible = FALSE, targets = "id"))))

})

pd_DTproxy <- DT::dataTableProxy("pd_adpcCom_profile")


observe({
  req(input$pd_adpcCom_profile_rows_selected)
  selected_rows <- input$pd_adpcCom_profile_rows_selected
  isolate({
    data_keep <- pd_comments_data_profile()
    pdplotRV$highlighted <- unlist(data_keep[selected_rows, ]$time_points)
  })
})


#------------------------------------------#
######   pd profile comment timeline  ######
#------------------------------------------#


output$pd_profile_comments_ui <- renderUI({
  if (PKPDEnv("verbose")) cat("pd_profile_comments_ui\n")
  req(pd_comments_data_profile())

  user <- setupRV$user
  user_type <- setupRV$user_type

  comments_sub <- pd_comments_data_profile()


  if (nrow(comments_sub)) {
  # create missing avatars
    user_names <- unique(comments_sub$user)

    dir.create(file.path("www", "avatars"), showWarnings = FALSE)

    files <-
      structure(file.path("www", "avatars", paste0(tolower(user_names), ".png")),
                names = user_names)

    create_avatars <- files[!file.exists(files)]

    avatar_colors2 <- avatar_colors_keep <- avatar_colors()

    for (initials in names(create_avatars)){
        if (initials %in% avatar_colors2$user) {
          color <- avatar_colors2[avatar_colors2$user == initials, ]$col
        } else {
          color <- sample(nncol$company[-1], 1)
        }
        create_avatar(user = initials, save_png = TRUE, file = create_avatars[initials], type = "square",
                    col = color)
    }
  }

  tags$div( class = "row",
    tags$div( class = "col-sm-12",
      tags$div( class = "comment-tabs",
        tags$div( class = "tab-content",
          tags$ul(class = "media-list",
            lapply(seq_len(nrow(comments_sub)), FUN = function(i){

              if (!length(comments_sub$reply[i][[1]])) {
                n_responses <- 0
              }else{

                responses <- comments_sub$reply[i][[1]]

                n_responses <- length(responses)
              }

              time1 <- comments_sub[i, "time"]
              time1 <- .POSIXct(time1, tz = "UTC")
              attr(time1, "tzone") <- Sys.timezone()

              status <- comments_sub[i, "status"]

              if (status == "Awaiting approval") {
                status <- "Awaiting"
                stat_button = "btn btn-danger action-button"
                stat_button_class = "glyphicon glyphicon-unchecked"
              }
              if (status == "Approved" | status == "Approve") {
                stat_button = "btn btn-info action-button"
                stat_button_class = "glyphicon glyphicon-edit"
              }
              if (status == "Query") {
                stat_button = "btn btn-primary action-button"
                stat_button_class = "glyphicon glyphicon-share-alt"
              }
              if (status == "Re-evaluate") {
                stat_button = "btn btn-warning action-button"
                stat_button_class = "glyphicon glyphicon glyphicon-repeat"
              }
              if (status == "Final") {
                stat_button = "btn btn-success action-button"
                stat_button_class = "glyphicon glyphicon glyphicon-check"
              }
              if (status == "No action") {
                stat_button = "btn btn-success action-button"
                stat_button_class = "glyphicon glyphicon glyphicon-check"
              }

              tags$li(class = "media",
                tags$a(class = "pull-left", href = "#",
                  tags$img(class = "media-object img-circle",
                           src = paste0("avatars/",  tolower(comments_sub[i, "user"]), ".png"),
                           alt = "profile", height = "64", width = "64")
                  ),
                  tags$div(class = "media-body",
                    tags$div(class = "well well-lg",
                      tags$h4(class = "media-heading text-uppercase reviews", comments_sub[i, "regarding"]),

                      tags$ul( class = "media-date text-uppercase reviews list-inline",
                               tags$li(class = "dd", paste(time1)),
                               br(),
                               tags$button( id = paste0("pd_comment_status_", i),
                                            type="button",
                                            class = stat_button,
                                            style = paste0("width: ", validateCssUnit("130px"), ";"),
                                            onclick='Shiny.onInputChange("pd_comment_status",  this.id, {priority: \"event\"})',
                                            tags$span(class = stat_button_class), status)
                      ),

                      tags$p(class = "media-comment",
                        if (length(unlist(comments_sub[i, "time_points"][[1]])) && comments_sub[i, "time_type"] == "point") {
                            paste("Time point(s):",
                                  glue::glue_collapse(round(unlist(comments_sub[i, "time_points"][[1]]),2),
                                                           sep = ", ", last = " and "))
                        } 
                        else if (length(unlist(comments_sub[i, "time_points"][[1]])) && comments_sub[i, "time_type"] == "interval") {
                          paste("Time interval:",
                                glue::glue_collapse(round(unlist(comments_sub[i, "time_points"][[1]]),2),
                                                    sep = " to "))
                        } else {
                          "Regards the entire profile"
                        },
                        br(),
                        comments_sub[i, "comment"]
                      ),

                      tags$button(id      = paste0("pd_comment_highlight_", i),
                                  type    = "button",
                                  class   = "btn btn-primary action-button",
                                  onclick = 'Shiny.onInputChange("pd_comment_highlight",  this.id, {priority: \"event\"})',
                                  tags$span(class = "glyphicon glyphicon-eye-open"), "Highlight"),
                      tags$button(id      = paste0("pd_write_reply_button_", i),
                                  type    = "button",
                                  class   = "btn btn-info action-button",
                                  onclick = 'Shiny.onInputChange("pd_write_reply_button",  this.id, {priority: \"event\"})',
                                  tags$span(class = "glyphicon glyphicon-share-alt"), "Write reply"),
                #      tags$a(class = "btn btn-info text-uppercase", `data-toggle`="collapse", href=paste0("#writereply", i), tags$span(class="glyphicon glyphicon-share-alt"),  "Write reply"),
                      tags$a(class = "btn btn-warning text-uppercase", `data-toggle`="collapse", href=paste0("#reply", i), tags$span(class="glyphicon glyphicon-comment"), paste(n_responses, ifelse(n_responses == 1, "reply", "replies"))),
                      if ( comments_sub[i, "user"] == user | user_type == "superuser") {
                        list(
                          tags$span(style = "float:right",
                            tags$button(id      = paste0("pd_comment_delete_", i),
                                        type    = "button",
                                        class   = "btn btn-danger action-button",
                                        onclick = 'Shiny.onInputChange("pd_comment_delete",  this.id, {priority: \"event\"})',
                                        tags$span(class = "glyphicon glyphicon-trash"), ""),
                            tags$button(id      = paste0("pd_write_edit_button_", i),
                                        type    = "button",
                                        class   = "btn btn-primary action-button",
                                        onclick = 'Shiny.onInputChange("pd_write_edit_button",  this.id, {priority: \"event\"})',
                                        tags$span(class = "glyphicon glyphicon-edit"), "")
                          )
                        )
                      },
                      tags$head(tags$style(HTML(".shiny-split-layout > media {overflow: visible;}")))
                    ),

                    tags$div(class = "collapse in", id = paste0("reply", i),
                    tags$ul(class = "media-list",
                      lapply(seq_len(n_responses), FUN = function(j) {

                        time2 <- fasttime::fastPOSIXct( responses[[j]][[2]], "UTC")
                        attr(time2, "tzone") <- Sys.timezone()

                        tags$li(class = "media media-replied", id = paste0("replied", i, j),
                          tags$a(class = "pull-left", href = "#",
                             tags$img(class = "media-object img-circle",
                                      src   = paste0("avatars/",  tolower(responses[[j]][[1]]), ".png"),
                                      alt   = "profile", height = "64", width = "64")
                          ),
                          tags$div(class = "media-body",
                            tags$div(class = "well well-lg",
                              tags$h4(class = "media-heading text-uppercase reviews", tags$span(class = "glyphicon glyphicon-share-alt"), toupper(comments_sub[i, "user"])),
                              tags$ul(class = "media-date text-uppercase reviews list-inline",
                                tags$li(class = "dd", paste(time2))),
                              tags$p(class = "media-comment", responses[[j]][[3]])
                            )
                          )
                        )}
                      )
                    )
                  )
                )
              )}
            )
          )
        )
      )
    )
  )
})

observeEvent(input$pd_comment_highlight, {
  
  plot_data <- setup_pd_plots_data()
  
  selected_rows <- as.numeric(strsplit(input$pd_comment_highlight, "_")[[1]][4])
  
  plot_data_out <- pd_plot_data() %>% dplyr::filter(!is.na(panel_name))
  
  adpdc_sub <- plot_data_out[plot_data_out$profid == input$pd_select_profile, ]
  
  isolate({
    data_keep <- pd_comments_data_profile()
    
    data_row  <- data_keep[selected_rows, ]
    time_type <- data_row$time_type
    
    time_points <- unlist(data_row$time_points)

    if (time_type == "interval") {
      
      pdplotRV$highlighted <- data_row %>% dplyr::mutate(key = list(c("from", "to"))) %>%
        tidyr::unnest(time_points, key, .preserve = paramcd) %>% 
        tidyr::spread(key = key, value = time_points) %>% 
        tidyr::unnest(paramcd) %>% 
        dplyr::left_join(plot_data[, c("paramcd", "panel_name")], by = "paramcd") %>%
        dplyr::mutate(panel_name = factor(panel_name, levels = unique(plot_data$panel_name)))

    }
    else if (time_type == "point") {
     
      # "%pin%" <- function(x, table) pmatch(x, table, nomatch = 0) > 0
      
      pdplotRV$highlighted <- adpdc_sub %>% 
        dplyr::filter(paste(round(time, 5), paramcd) %in% 
                        paste(round(unlist(data_row$time_points), 5), unlist(data_row$time_point_paramcd))) %>% 
        dplyr::mutate(time_type = time_type)
    }
    else {
      pdplotRV$highlighted <- NULL
    }
  })
})


output$pd_all_trial_comments <- DT::renderDataTable({
  
  if (PKPDEnv("verbose")) cat("pd_all_trial_comments\n")
  req(pd_comments_data())
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  comment_data <- pd_comments_data()
  
  if (is.null(comment_data) || nrow(comment_data) == 0)
    return(NULL)
  
  comment_data$reply <- lapply(comment_data$reply, lapply, lapply, as.character)
  
  comment_data$reply <-
    lapply(comment_data$reply, lapply, paste, collapse = "<br/>", sep = "") %>%
    sapply(paste, collapse = "<br/><br/>", sep = "")
  
  if ("time_points" %in% colnames(comment_data)) {
    if (any(sapply(comment_data$time_points, length))) {
      comment_data$time_points <-
        sapply(comment_data$time_points, paste, collapse = ", ")
    } else {
      comment_data$time_points <- ""
    }
  }
  
  # convert timezone
  time1 <- comment_data$time
  time1 <- .POSIXct(time1, tz = "UTC")
  attr(time1, "tzone") <- Sys.timezone()
  comment_data$time <- time1
  
  comment_data$Status <- comment_data$status
  
  
  comment_data2 <- comment_data %>% dplyr::select(profile, user, regarding, comment, time_points, time_type, reply, Status)
  
  DT::datatable(comment_data2, #class = 'compact striped',
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Profile", "User", "Regarding", "Comment", "Time",
                             "Time type", "Replies", "Status"),
                #selection = list(mode = 'single'),
                options = list(scrollX = TRUE, paging = FALSE,
                               columnDefs = list(list(visible = FALSE, targets = "id"))))
  
})




#-----------------------------------------#
######      pd_comment_status        ######
#-----------------------------------------#

# decouple commentstatus from comments

pd_comment_status <- reactive({
  req(pd_comments_data())
  
  if (PKPDEnv("verbose")) cat("comment_status\n")
  
  comments <- pd_comments_data()
  comment_status <- list("with comments"   = comments$profile)
  for (stat in unique(comments$status)) {
    comment_status[[stat]] <- comments[comments$status == stat, ]$profile
  }
  
  comment_status
}) %>% dedupe()


#-----------------------------------------#
######      pd_reviewed profiles     ######
#-----------------------------------------#


pd_user_review <- reactive({
  req(pd_comments_data())
  if (PKPDEnv("verbose")) cat("pd_user_review\n")
  
  if (!"review_status" %in% names(attributes(pd_comments_data())))
    return(NULL)
  
  attr(pd_comments_data(), "review_status")[[tolower(setupRV$user)]]
}) %>% dedupe()


pd_review_status <- reactive({
  req(pd_comments_data())
  if (PKPDEnv("verbose")) cat("pd_review_status\n")
  
  if (!"review_status" %in% names(attributes(pd_comments_data())))
    return(NULL)
  
  attr(pd_comments_data(), "review_status")
}) %>% dedupe()




observeEvent(input$pd_profile_reviewed, {
  req(trial_info())
  if (PKPDEnv("verbose")) cat("profile_reviewed\n")
  initials  <- setupRV$user
  inst_info <- trial_info()
  
  # Indicate that we are saving the status
  withProgress(message = "saving review status", value = 0, {
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$pd_comment[[trial_info()$trial_id]] <<-
      reviewProfile(comment_data = pd_comments_data(),
                    profile      = input$pd_select_profile,
                    user         = setupRV$user)
    
    incProgress(1/1, "Saving", "Done")
  })
  
  profiles <- pd_profile_choices()
  
  if (!input$pd_select_profile == profiles[length(profiles)]) {
    if (input$pd_show_profiles != 2) {
      selected <- profiles[which(input$pd_select_profile == profiles) + 1]
      pdplotRV$profile_select_by_review <- selected
      pdplotRV$profile_select_ui <- selected
      updateSelectizeInput(session, "pd_select_profile",
                        selected = selected)
    }
  } else {
    shinyalert::shinyalert("The currently reviewed profile is the last", type = "info")
  }
})

output$pd_review_status_user_ui <-  renderUI({
  req(pd_user_review(), pd_all_profiles())
  if (PKPDEnv("verbose")) cat("pd_review_status_user_ui\n")
  user <- tolower(setupRV$user)
  
  profiles <- pd_all_profiles()
  review_status <- pd_user_review()
  
  progressBar <- function(text, value = 0, color = "aqua") {
    tags$div( class = "form-group shiny-input-container",
              tags$p(text, tags$small(class = "pull-right", paste0(value, "%"))),
              tags$div(class = "progress xs",
                       div(class = paste0("progress-bar progress-bar-", color),
                           style = paste0("width: ", value, "%"),
                           role  = "progressbar",
                           `aria-valuenow` = value, `aria-valuemin` = "0", `aria-valuemax` = "100",
                           span(class = "sr-only", paste0(value, "% complete"))
                       )
              )
    )
  }
  
  progressBar(value = round(sum(review_status %in% profiles)/length(profiles)*100),
              text = "Review progress")
})


pd_review_status_items <- reactive({
  req(pd_review_status(),  pd_all_profiles())
  
  if (PKPDEnv("verbose")) cat("pd_review_status_ui\n")
  
  user <- tolower(setupRV$user)
  
  profiles <- pd_all_profiles()
  review_status <- pd_review_status()
  
  #  if (PKPDEnv("verbose")) {cat("profiles\n"); print(profiles)}
  if (PKPDEnv("verbose")) {cat("review_status\n"); print(review_status)}
  
  
  # Start with logged in user
  if (user %in% names(review_status))
    review_status <- review_status[c(user, setdiff(names(review_status), user))]
  
  items <- lapply(seq_len(length(review_status)), function(i) {
    taskItem(value = round(sum(review_status[[i]] %in% profiles)/length(profiles)*100), 
             text  = toupper(names(review_status)[i]))
  })
  
  return(items)
})


# output$pd_review_status_ui <-  renderMenu({
#   req(pd_review_status(),  pd_all_profiles())
#   
#   if (PKPDEnv("verbose")) cat("pd_review_status_ui\n")
#   
#   user <- tolower(setupRV$user)
#   
#   profiles <- pd_all_profiles()
#   review_status <- pd_review_status()
#   
#   #  if (PKPDEnv("verbose")) {cat("profiles\n"); print(profiles)}
#   if (PKPDEnv("verbose")) {cat("review_status\n"); print(review_status)}
#   
#   
#   # Start with logged in user
#   if (user %in% names(review_status))
#     review_status <- review_status[c(user, setdiff(names(review_status), user))]
#   
#   items <- lapply(seq_len(length(review_status)), function(i) {
#     taskItem(value = round(sum(review_status[[i]] %in% profiles)/length(profiles)*100), 
#              text  = toupper(names(review_status)[i]))
#   })
#   
#   dropdownMenu(
#     type = "tasks", badgeStatus = "primary", headerText = "Review progress ",
#     .list = items
#   )
# })






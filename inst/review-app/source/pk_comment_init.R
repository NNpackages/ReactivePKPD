#---------------------------------------------#
######        comment connection         ######
#---------------------------------------------#

# Initialise the comments if it is not allready initialised
PK_comments_init <- reactive({
  if (PKPDEnv("verbose")) cat("PK_comments_init\n")
  
  req(trial_info(), all_profiles())
  trial_info <- trial_info()
  
  isolate({
    
    if (is.null(GlobalRV$comment[[trial_info$trial_id]]) || !identical(attr(GlobalRV$comment[[trial_info$trial_id]], "users"), trial_info$reviewers)) {
      
      pk_comments_path <- file.path(trial_info$trial_app_dir)
      
      dir.create(pk_comments_path, recursive = TRUE, showWarnings = FALSE)
      
      comment_data <- initCommentData(pk_comments_path, folder_name = "comment_data_folder",
                                      users =  trial_info$reviewers, profiles = all_profiles())
      
      GlobalRV$comment[[trial_info$trial_id]] <<- comment_data
      
    } else {
      GlobalRV$comment[[trial_info$trial_id]]
    }
  })
})


# Update the comment data based on the global reactive
PK_comments_data <- reactive({
  if (PKPDEnv("verbose")) cat("PK_comments_data\n")
  req(PK_comments_init(), trial_info())
  inst_info <- trial_info()
  PK_comments_data_all <- GlobalRV$comment[[inst_info$trial_id]]
  GlobalRV$comment[[inst_info$trial_id]]
})

observe({
  req(PK_comments_data(), input$select_profile)
  if (PKPDEnv("verbose")) cat("PK_comments_data_profile_proxy\n")
  
  new_data2 <- new_data <- PK_comments_data() %>% dplyr::filter(profile == input$select_profile)
  old_data2 <- old_data <- pkplotRV$PK_comments_data_profile_proxy
  
  attr(new_data2, "review_status") <- NULL
  attr(old_data2, "review_status") <- NULL
  
  if (!identical(new_data2, old_data2)) {
    pkplotRV$PK_comments_data_profile_proxy <- new_data
    if (PKPDEnv("verbose")) cat("PK_comments_data_profile_proxy updated\n")
  }
})


# subset to the currently selected profile
PK_comments_data_profile <- reactive({
  if (PKPDEnv("verbose")) cat("PK_comments_data_profile\n")
  # req(PK_comments_data(), input$select_profile)
  
  pkplotRV$PK_comments_data_profile_proxy
})


# The comments display
output$pk_profile_comments_ui <- renderUI({
  if (PKPDEnv("verbose")) cat("pk_profile_comments_ui\n")
  req(PK_comments_data_profile())
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  comments_sub <- PK_comments_data_profile()
  
  
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
                             tags$button( id = paste0("pk_comment_status_", i),
                                          type="button",
                                          class = stat_button,
                                          style = paste0("width: ", validateCssUnit("130px"), ";"),
                                          onclick='Shiny.onInputChange("pk_comment_status",  this.id, {priority: \"event\"})',
                                          tags$span(class = stat_button_class), status)
                    ),
                          
                    tags$p(class = "media-comment",
                      if (length(unlist(comments_sub[i, "time_points"][[1]]))) {
                       paste("Nominal time:",
                         glue::glue_collapse(round(unlist(comments_sub[i, "time_points"][[1]]),2),
                               sep = ", ", last = " and "))
                      } else {
                        "Regards the entire profile"
                      },
                      br(),
                      comments_sub[i, "comment"]
                    ),
                          
                    tags$button(id      = paste0("pk_comment_highlight_", i),
                                type    = "button",
                                class   = "btn btn-primary action-button",
                                onclick = 'Shiny.onInputChange("pk_comment_highlight",  this.id, {priority: \"event\"})',
                                tags$span(class = "glyphicon glyphicon-eye-open"), "Highlight"),
                    tags$button(id      = paste0("pk_write_reply_button_", i),
                                type    = "button",
                                class   = "btn btn-info action-button",
                                onclick = 'Shiny.onInputChange("pk_write_reply_button",  this.id, {priority: \"event\"})',
                                tags$span(class = "glyphicon glyphicon-share-alt"), "Write reply"),
                    #      tags$a(class = "btn btn-info text-uppercase", `data-toggle`="collapse", href=paste0("#writereply", i), tags$span(class="glyphicon glyphicon-share-alt"),  "Write reply"),
                    tags$a(class = "btn btn-warning text-uppercase", `data-toggle`="collapse", href=paste0("#reply", i), tags$span(class="glyphicon glyphicon-comment"), paste(n_responses, ifelse(n_responses == 1, "reply", "replies"))),
                    if ( comments_sub[i, "user"] == user | user_type == "superuser") {
                      list(
                        tags$span(style = "float:right",
                          tags$button(id      = paste0("pk_comment_delete_", i),
                                      type    = "button",
                                      class   = "btn btn-danger action-button",
                                      onclick = 'Shiny.onInputChange("pk_comment_delete",  this.id, {priority: \"event\"})',
                                      tags$span(class = "glyphicon glyphicon-trash"), ""),
                          tags$button(id      = paste0("pk_write_edit_button_", i),
                                      type    = "button",
                                      class   = "btn btn-primary action-button",
                                      onclick = 'Shiny.onInputChange("pk_write_edit_button",  this.id, {priority: \"event\"})',
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
                        )
                      })
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

observeEvent(input$pk_comment_highlight, {
  
  selected_rows <- as.numeric(strsplit(input$pk_comment_highlight, "_")[[1]][4])
  isolate({
    data_keep <- PK_comments_data_profile()
    data_sub  <- data_keep[selected_rows, ]
    
    profile_time <- profile_data() %>% dplyr::select(time, plot_time)
    
    if (length(unlist(data_sub$time_points))) {
      time_points <- profile_time %>% dplyr::filter(time %in% unlist(data_sub$time_points)) %>% dplyr::pull(plot_time) %>% unique()
    }
    else {
      time_points <- NULL
    }
    
    pkplotRV$highlighted <- time_points
  })
})


# show the profile data
output$adpcCom_profile <- DT::renderDataTable({
  
  if (PKPDEnv("verbose")) cat("adpcCom_profile\n")
  req(PK_comments_data_profile())
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  comment_data <- PK_comments_data_profile()
  comment_data <- comment_data[, setdiff(colnames(comment_data), 
                                         c("paramcd", "color_by", "time_type", "time_point_paramcd"))]
  
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
      as.character(tags$button( id = paste0("pk_comment_status_tab_", i),
                                #   type="button",
                                style   = paste0("width: ", validateCssUnit("100px"), ";"),
                                class   = stat_button,
                                onclick = 'Shiny.onInputChange("pk_comment_status_tab",  this.id, {priority: \"event\"})',
                                #tags$span(class = stat_button_class),
                                status))
  }
  
  Reply <-
    shinyInput(tags$button, nrow(comment_data), "pk_write_reply_button_tab_",
               type    = "button",
               class   = "btn btn-info action-button",
               onclick = 'Shiny.onInputChange("pk_write_reply_button_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-share-alt"), "")
  
  Edit <-
    shinyInput(tags$button, nrow(comment_data), "pk_write_edit_button_tab_",
               type    = "button",
               class   = "btn btn-primary action-button",
               onclick = 'Shiny.onInputChange("pk_write_edit_button_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-edit"), "")
  
  Delete <-
    shinyInput(tags$button, nrow(comment_data), "pk_comment_delete_tab_",
               type    = "button",
               class   = "btn btn-danger action-button",
               onclick = 'Shiny.onInputChange("pk_comment_delete_tab",  this.id, {priority: \"event\"})',
               tags$span(class = "glyphicon glyphicon-trash"), "")
  
  
  comment_data <- comment_data %>% dplyr::select(user, regarding, comment, time_points, reply, Status)
  
  comment_data_2 <- dplyr::bind_cols(comment_data, Reply = Reply, Edit = Edit, Delete = Delete)
  
  comment_data_2[comment_data_2$user != user & user_type != "superuser", c("Edit", "Delete")] <- c("", "")
  
  DT::datatable(comment_data_2, #class = 'compact striped',
                escape = FALSE,
                rownames = FALSE,
                colnames = c("User", "Regarding", "Comment", "Nominal time",
                             "Replies", "Status", "Reply", "Edit", "Delete"),
                selection = list(mode = 'single'),
                options = list(scrollX = TRUE, paging = FALSE,
                               columnDefs = list(list(visible = FALSE, targets = "id"))))
})

DTproxy <- DT::dataTableProxy("adpcCom_profile")


observe({
  req(input$adpcCom_profile_rows_selected)
  selected_rows <- input$adpcCom_profile_rows_selected
  isolate({
    data_keep <- PK_comments_data_profile()
    pkplotRV$highlighted <- unlist(data_keep[selected_rows, ]$time_points)
  })
})



output$all_trial_comments <- DT::renderDataTable({
  
  if (PKPDEnv("verbose")) cat("all_trial_comments\n")
  req(PK_comments_data())
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  comment_data <- PK_comments_data()
  
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
  
  
  comment_data2 <- comment_data %>% dplyr::select(profile, user, regarding, comment, time_points, reply, Status)
  
  DT::datatable(comment_data2, #class = 'compact striped',
                escape = FALSE,
                rownames = FALSE,
                colnames = c("Profile", "User", "Regarding", "Comment", "Nominal time",
                             "Replies", "Status"),
                #selection = list(mode = 'single'),
                options = list(scrollX = TRUE, paging = FALSE,
                               columnDefs = list(list(visible = FALSE, targets = "id"))))
  
})


#-----------------------------------------#
######      pk_comment_status        ######
#-----------------------------------------#

# decouple commentstatus from comments

pk_comment_status <- reactive({
  
  req(PK_comments_data())
  
  if (PKPDEnv("verbose")) cat("pk_comment_status\n")
  
  comments <- PK_comments_data()
  comment_status <- list("with comments"   = comments$profile)
  for (stat in unique(comments$status)) {
    comment_status[[stat]] <- comments[comments$status == stat, ]$profile
  }
  
  comment_status
  
}) %>% dedupe()


#-----------------------------------------#
######      pk_reviewed profiles     ######
#-----------------------------------------#


pk_user_review <- reactive({
  req(PK_comments_data())
  if (PKPDEnv("verbose")) cat("pk_user_review\n")
  
  if (!"review_status" %in% names(attributes(PK_comments_data())))
    return(NULL)
  
  attr(PK_comments_data(), "review_status")[[tolower(setupRV$user)]]
}) %>% dedupe()


pk_review_status <- reactive({
  req(PK_comments_data())
  if (PKPDEnv("verbose")) cat("pk_review_status\n")
  
  if (!"review_status" %in% names(attributes(PK_comments_data())))
    return(NULL)
  
  attr(PK_comments_data(), "review_status")
}) %>% dedupe()


observeEvent(input$profile_reviewed, {
  req(trial_info())
  if (PKPDEnv("verbose")) cat("profile_reviewed\n")
  initials  <- setupRV$user
  inst_info <- trial_info()
  
  # Indicate that we are saving the status
  withProgress(message = "saving review status", value = 0, {
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$comment[[trial_info()$trial_id]] <<-
      reviewProfile(comment_data = PK_comments_data(),
                    profile      = input$select_profile,
                    user         = setupRV$user)
    
    incProgress(1/1, "Saving", "Done")
  })
  
  profiles <- profile_choices()
  
  if (!input$select_profile == profiles[length(profiles)]) {
    if (input$show_profiles != 2) {
      selected <- profiles[which(input$select_profile == profiles) + 1]
      pkplotRV$profile_select_by_review <- selected
      pkplotRV$profile_select_ui <- selected
      updateSelectizeInput(session, "select_profile",
                        selected = selected)
    }
  } else {
    shinyalert::shinyalert("The currently reviewed profile is the last", type = "info")
  }
})

output$review_status_user_ui <-  renderUI({
  
  req(pk_user_review(), all_profiles())
  
  if (PKPDEnv("verbose")) cat("pk_review_status_user_ui\n")
  user <- tolower(setupRV$user)
 
  profiles <- all_profiles()
  review_status <- pk_user_review()
  
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



pk_review_status_items <- reactive({
  
  if (PKPDEnv("verbose")) cat("review_status_ui\n")
  
  user <- tolower(setupRV$user)
  
  profiles <- all_profiles()
  review_status <- pk_review_status()
  
  #  if (PKPDEnv("verbose")) {cat("profiles\n"); print(profiles)}
  if (PKPDEnv("verbose")) {cat("review_status\n"); print(review_status)}
  
  
  # Start with logged in user
  if (user %in% names(review_status))
    review_status <- review_status[c(user, setdiff(names(review_status), user))]
  
  items <- lapply(seq_len(length(review_status)), function(i) {
    taskItem(value = round(sum(review_status[[i]] %in% profiles)/length(profiles)*100), text = toupper(names(review_status)[i]))
  })
  
  return(items)
})


# output$review_status_ui <-  renderMenu({
#   req(pk_review_status(),  all_profiles())
#   
#   if (PKPDEnv("verbose")) cat("review_status_ui\n")
#   
#   user <- tolower(setupRV$user)
# 
#   profiles <- all_profiles()
#   review_status <- pk_review_status()
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
#     taskItem(value = round(sum(review_status[[i]] %in% profiles)/length(profiles)*100), text = toupper(names(review_status)[i]))
#   })
#   
#   dropdownMenu(
#     type = "tasks", badgeStatus = "primary", headerText = "Review progress",
#     .list = items
#   )
# })



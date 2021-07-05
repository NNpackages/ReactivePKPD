#-----------------------------------------#
######      pk_comment modal         ######
#-----------------------------------------#

observeEvent(input$add_pk_comment, {
  
  selected_points <- pk_plot_highlight_points()
  
  flags <- attributes(PK_comments_data())$flag_data
  
  if (!is.null(selected_points) && length(selected_points)) {
    flags_point <- flags[flags$type == "point", ] 
    
    ui <- tagList(
      h2("Adding comment for selected points:"),
      selectInput(inputId = "comment_regard", "Comment(s) regards:",
                  choices = c("", unique(flags_point$regarding)),
                  selected = NULL, width = "100%"),
      uiOutput("pk_comment_input_ui")
    )
  } else {
    flags_profile <- flags[flags$type == "profile", ] 
    
    ui <- tagList(
      h2("Adding comment for Whole profile:"),
      selectInput(inputId = "comment_regard", "Comment(s) regards:",
                  choices = c("", unique(flags_profile$regarding)),
                  selected = NULL, width = "100%"),
      uiOutput("pk_comment_input_ui")
    )
  }
  
  showModal(modalDialog(title = "Add comment",  ui))
})


output$pk_comment_input_ui <- renderUI({
  req(input$comment_regard)
  
  regarding <- input$comment_regard
  
  isolate({
    selected_points <- pk_plot_highlight_points()
    
    flags <- attributes(PK_comments_data())$flag_data
    
    if (!is.null(selected_points) && length(selected_points)) {
      choices <- flags[flags$regarding == regarding & flags$type == "point", ]$comment 
    } else {
      choices <- flags[flags$regarding == regarding & flags$type == "profile", ]$comment 
    }
    
    ui <- tagList(
      #h2("Adding comment for selected points:"),
      
      
      splitLayout(cellWidths = c("80%", "20%"),
                  selectizeInput("pk_comment",
                                 label = "Input your comment",
                                 choices = c("", choices),
                                 options = list(create = TRUE), width = "90%"),
                  uiOutput("save_pk_comment_ui")),
      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
      
      
    )
  })
})

output$save_pk_comment_ui <- renderUI({
  
  req(input$pk_comment)
  
  div(br(),
      actionButton("save_pk_comment", "Save", width = "90%",
                   style = paste0("color: white; background-color: ",
                                  "#3F9C35",";
                                       border-color: ","#3F9C35"))
  )
})


observeEvent(input$save_pk_comment, {
  
  withProgress(message = "saving comment data", value = 0, {
    
    initials  <- setupRV$user
    profile   <- input$select_profile
    regarding <- input$comment_regard
    comment   <- input$pk_comment
    selected_points <- pk_plot_highlight_points()
    
    
    
    
    
    range <- plot_range()
    view_range <- c(range$min_x, range$max_x)
    
    if (is.null(selected_points)) {
      time_points <- numeric(0)
      
      time_type <- "profile"
    } 
    else {
      profile_time <- profile_data() %>% dplyr::select(time, plot_time)
      
      time_points <- profile_time %>% dplyr::filter(plot_time %in% selected_points) %>% dplyr::pull(time) %>% unique()
      
      time_type <- "point"
    }
    
    inst_info <- trial_info()
    
    GlobalRV$comment[[inst_info$trial_id]]
    
    comment_data <- PK_comments_data()
    
    GlobalRV$comment[[inst_info$trial_id]] <<-
      writeComment(comment_data, user = initials, profile = profile,
                   regarding = regarding,
                   comment = comment, time_points = time_points, time_type = time_type,
                   view_range = view_range)
    
    # Initialize the highlight
    pkplotRV$highlighted <- NULL
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})


#-----------------------------------------#
######       pk_modify_comment       ######
#-----------------------------------------#
commentRV <- reactiveValues()

observeEvent(input$pk_write_edit_button_tab, {
  commentRV$edit_comment <- 0
  commentRV$edit_comment <- as.numeric(strsplit(input$pk_write_edit_button_tab, "_")[[1]][6])
})

observeEvent(input$pk_write_edit_button, {
  commentRV$edit_comment <- 0
  commentRV$edit_comment <- as.numeric(strsplit(input$pk_write_edit_button, "_")[[1]][5])
})

observeEvent(commentRV$edit_comment, {
  #print(commentRV$edit_comment )
  req(commentRV$edit_comment, profile_data())
  selected_rows <- commentRV$edit_comment
  
  profile_data_keep <- profile_data()
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  data_keep <- PK_comments_data_profile()
  data_sub <- data_keep[selected_rows, ]
  
  if (length(unlist(data_sub$time_points))) {
    selected_points <- unlist(data_sub$time_points)
    
    time_points <- structure(profile_data_keep$time, names = paste(profile_data_keep$time, "/", profile_data_keep$plot_time))
  }
  
  
  if (toupper(data_sub$user) != toupper(user) & user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You can only edit comments you have made yourself",
                           type = "warning")
    
  } else {
    
    ui <- tagList(
      h4(glue::glue("Edit comment by {data_sub$user}")),
      
      if (length(unlist(data_sub$time_points)))
        selectInput("edit_points_selected", label = "Edit selected point(s): (Nominal time / Time)",
                    choices = time_points,
                    selected = selected_points,
                    multiple = TRUE),
      
      
      p(em("Regarding: "), data_sub$regarding),
      splitLayout(cellWidths = c("80%", "20%"),
                  textInput("pk_edited_comment", label = "Change comment:",
                            value = data_sub$comment, width = "90%"),
                  uiOutput("save_pk_edit_ui"))
    )
    
    showModal(modalDialog(title = "Edit comment",  ui))
    
  }
})


output$save_pk_edit_ui <- renderUI({
  req(input$pk_edited_comment)
  
  selected_rows <- commentRV$edit_comment
  
  data_keep <- PK_comments_data_profile()
  data_sub  <- data_keep[selected_rows, ]
  
  points_selcted <- as.numeric(input$edit_points_selected)
  
  if (length(unlist(data_sub$time_points))) {
    current_points <- unlist(data_sub$time_points)
  }
  
  check_points <- length(unlist(data_sub$time_points)) &&
    !is.null(input$edit_points_selected) &&
    !(all(points_selcted %in% current_points) &
        all(current_points %in% points_selcted))
  
  comment_check <- input$pk_edited_comment != data_sub$comment
  
  if (check_points | comment_check)
    div(br(),
        actionButton("save_pk_edit", "Save", width = "90%",
                     style = paste0("color: white; background-color: ",
                                    "#3F9C35",";
                                       border-color: ","#3F9C35"))
    )
})

observeEvent(input$save_pk_edit, {
  selected_rows <- commentRV$edit_comment
  
  data_keep <- PK_comments_data_profile()
  data_sub <- data_keep[selected_rows, ]
  
  if (length(unlist(data_sub$time_points))) {
    time_points <- as.numeric(input$edit_points_selected)
  }
  else {
    time_points <- numeric(0)
  }
  
  # Indicate that we are saving the comment
  withProgress(message = "saving comment data", value = 0, {
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$comment[[trial_info()$trial_id]] <<-
      writeEdit(comment_data = PK_comments_data(),
                comment_id   = PK_comments_data_profile()[commentRV$edit_comment, ]$id,
                comment      = input$pk_edited_comment,
                time_points  = time_points,
                view_range   = c(plot_range()$min_x, plot_range()$max_x))
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})

#-----------------------------------------#
######       pk_Delete               ######
#-----------------------------------------#

observeEvent(input$pk_comment_delete_tab, {
  commentRV$delete_comment <- 0
  commentRV$delete_comment <- as.numeric(strsplit(input$pk_comment_delete_tab, "_")[[1]][5])
})

observeEvent(input$pk_comment_delete, {
  commentRV$delete_comment <- 0
  commentRV$delete_comment <- as.numeric(strsplit(input$pk_comment_delete, "_")[[1]][4])
})


observeEvent(commentRV$delete_comment, {
  
  req(commentRV$delete_comment)
  #print(commentRV$delete_comment)
  selected_rows <- commentRV$delete_comment
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  data_keep <- PK_comments_data_profile()
  data_sub <- data_keep[selected_rows, ]
  
  #print(PK_comments_data_profile()[commentRV$delete_comment, ])
  
  if (toupper(data_sub$user) != toupper(user) & user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You can only delete comments you have made yourself",
                           type = "warning")
    
  } else {
    
    ui <- tagList(
      p(em("Regarding: "), data_sub$regarding),
      p(em("Comment: "), data_sub$comment)
    )
    
    showModal(modalDialog(title = glue::glue("Delete comment by {data_sub$user}"),
                          footer = tagList(modalButton("Cancel"),
                                           actionButton("pk_Delete_com", "Delete comment",style = paste0("color: white; background-color: ","#E64A0E","; border-color: ","#E64A0E"))),
                          ui))
  }
})


observeEvent(input$pk_Delete_com, {
  
  # Indicate that we are saving the comment
  withProgress(message = "Deleting comment", value = 0, {
    
    selected_rows <- commentRV$delete_comment
    
    data_keep <- PK_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
    comment_data <- PK_comments_data()
    comment_id <- data_sub$id
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$comment[[trial_info()$trial_id]] <<-
      deleteComment(comment_data = PK_comments_data(),
                    comment_id   = data_sub$id,
                    user         = setupRV$user)
    
    incProgress(1/1, "Deleting", "Done")
  })
  removeModal()
})


#-----------------------------------------#
######          pk_reply             ######
#-----------------------------------------#

observeEvent(input$pk_write_reply_button_tab, {
  commentRV$reply_comment <- 0
  commentRV$reply_comment <- as.numeric(strsplit(input$pk_write_reply_button_tab, "_")[[1]][6])
})

observeEvent(input$pk_write_reply_button, {
  commentRV$reply_comment <- 0
  commentRV$reply_comment <- as.numeric(strsplit(input$pk_write_reply_button, "_")[[1]][5])
})


observeEvent( commentRV$reply_comment, {
  
  req( commentRV$reply_comment)
  selected_rows <-  commentRV$reply_comment
  
  isolate({
    data_keep <- PK_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
  })
  
  
  ui <- tagList(
    h4(glue::glue("Reply to comment by {data_sub$user}")),
    p(em("Regarding: "), data_sub$regarding),
    p(em("Comment: "), data_sub$comment),
    splitLayout(cellWidths = c("80%", "20%"),
                textInput("pk_reply", "Write reply",  width = "90%"),
                uiOutput("save_pk_reply_ui"))
  )
  
  showModal(modalDialog(title = "Reply to comment",  ui))
})

output$save_pk_reply_ui <- renderUI({
  req(input$pk_reply)
  div(br(),
      actionButton("save_pk_reply", "Save", width = "90%",
                   style = paste0("color: white; background-color: ",
                                  "#3F9C35",";
                                       border-color: ","#3F9C35"))
  )
})


observeEvent(input$save_pk_reply, {
  req(input$pk_reply)
  
  # Indicate that we are saving the comment
  withProgress(message = "saving comment data", value = 0, {
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$comment[[trial_info()$trial_id]] <<-
      writeReply(comment_data = PK_comments_data(),
                 comment_id    = PK_comments_data_profile()[commentRV$reply_comment, ]$id,
                 user          = setupRV$user,
                 reply         = input$pk_reply)
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})



#------------------------------------------#
#####  pk_review_meeting buttons      ######
#------------------------------------------#

observeEvent(input$pk_comment_status_tab, {
  commentRV$pk_comment_status <- 0
  commentRV$pk_comment_status <- as.numeric(strsplit(input$pk_comment_status_tab, "_")[[1]][5])
})

observeEvent(input$pk_comment_status, {
  commentRV$pk_comment_status <- 0
  commentRV$pk_comment_status <- as.numeric(strsplit(input$pk_comment_status, "_")[[1]][4])
})


observeEvent( commentRV$pk_comment_status, {
  
  req( commentRV$pk_comment_status)
  selected_rows <-  commentRV$pk_comment_status
  
  isolate({
    data_keep <- PK_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
  })
  
  
  ui <- tagList(
    h4(glue::glue("Change status for comment by {data_sub$user}")),
    p(em("Regarding: "), data_sub$regarding),
    p(em("Comment: "), data_sub$comment),
    splitLayout(cellWidths = c("80%", "20%"),
                selectInput("pk_status_select", "Select status",  width = "90%",
                            choices = c("Awaiting approval", "No action", "Approved", "Query", "Re-evaluate", "Final"),
                            selected = data_sub$status),
                div(br(),
                    actionButton("save_pk_status", "Save", width = "90%",
                                 style = paste0("color: white; background-color: ",
                                                "#3F9C35",";
                                       border-color: ","#3F9C35"))
                ))
  )
  
  showModal(modalDialog(title = "Change status for comment",  ui))
})




observeEvent(input$save_pk_status, {
  
  # check for super user
  if (setupRV$user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You do not have the rights to change status",
                           type = "warning")
    
  } else {
    
    withProgress(message = "saving comment data", value = 0, {
      
      # Indicate that we are saving the comment
      
      # save the comment to the gloabal comment data as well as on disk
      GlobalRV$comment[[trial_info()$trial_id]] <<-
        writeStatus(comment_data = PK_comments_data(),
                    comment_id    = PK_comments_data_profile()[commentRV$pk_comment_status, ]$id,
                    user          = setupRV$user,
                    status        = input$pk_status_select)
      
      incProgress(1/1, "Saving", "Done")
      
      removeModal()
    })
  }
})



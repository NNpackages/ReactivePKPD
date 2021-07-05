
#-----------------------------------------#
######      pd_comment modal         ######
#-----------------------------------------#

observeEvent(input$pd_add_comment, {
  
  
  selected_points <- pd_plot_highlight_time()
  
  col_scheme <- pd_col_scheme()
  paramcd_data <- col_scheme %>% dplyr::distinct(paramcd, param)
  panels <- structure(c("Other", paramcd_data$paramcd), names = c("Other", paramcd_data$param))
  
  time_type  =  structure(c("interval", "point"), names = c("Time interval", "Points"))[input$pd_select_times]
  
  if (!is.null(selected_points) && !identical(selected_points, numeric(0)) && nrow(selected_points) && selected_points$time_type[1] == time_type) {
    
    if (selected_points$time_type[1] == "point") {
      paramcd_data <- paramcd_data %>% dplyr::filter(paramcd %in% selected_points$paramcd)
      panels <- structure(paramcd_data$paramcd, names = paramcd_data$param)
      
      ui <- tagList(
        h2(glue::glue("Adding comment for the time points: {glue::glue_collapse(round(selected_points$time, 2), sep = ', ', last = ' and ')}")),
        
        selectInput(inputId = "pd_comment_regard", "Comment regards:",
                    choices = panels, selected = panels, width = "100%",
                    multiple = FALSE),
        
        textInput("pd_comment_input", "Write the comment:", width = "100%")
      )
    } else {
      ui <- tagList(
        h2(glue::glue("Adding comment for the time frame: {round(selected_points$from)} to {round(selected_points$to)}")),
        
        selectInput(inputId = "pd_comment_regard", "Comment regards: (Select more than one if applicable)",
                    choices = panels, width = "100%",
                    multiple = TRUE),
        
        textInput("pd_comment_input", "Write the comment:", width = "100%")
      )
    }
  } else {
    ui <- tagList(
      h2("Adding comment for Whole profile:"),
      selectInput(inputId = "pd_comment_regard", "Comment regards: (Select more than one if applicable)",
                  choices = panels, width = "100%",
                  multiple = TRUE),
      
      textInput("pd_comment_input", "Write the comment:", width = "100%")
    )
  }
  
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    uiOutput("save_pd_comment_ui",inline = TRUE)
  )
  
  showModal(modalDialog(title = "Add comment",  ui, footer = footer))
})



output$save_pd_comment_ui <- renderUI({
  
  req(input$pd_comment_regard, input$pd_comment_input)
  
  if ("Other" %in% input$pd_comment_regard)
  validate(need(length(input$pd_comment_regard) == 1 , "'Other' should be chosen alone"))
  
  actionButton("save_pd_comment", "Save", icon = icon("check"),
               style = paste0("color: white; background-color: ",
                              "#3F9C35","; border-color: ","#3F9C35"))
})


observeEvent(input$save_pd_comment, {
  
  withProgress(message = "saving comment data", value = 0, {
    
    col_scheme <- pd_col_scheme()
    selected_param <- col_scheme %>% dplyr::distinct(paramcd, param) %>% 
      dplyr::filter(paramcd %in% input$pd_comment_regard) %>% 
      dplyr::pull(param)
    
    time_type  =  structure(c("interval", "point"), names = c("Time interval", "Points"))[input$pd_select_times]
    
    initials  <- setupRV$user
    profile   <- input$pd_select_profile
    
    if ("Other" %in% input$pd_comment_regard) {
      paramcd  <- list("")
      regarding <- glue::glue_collapse(input$pd_comment_regard, sep = ", ", last = " and ")
    }
      
    else {
      paramcd  <- list(input$pd_comment_regard)
      regarding <- glue::glue_collapse(selected_param, sep = ", ", last = " and ")
    }
    
    color_by <- ""
    
    comment   <- input$pd_comment_input
    selected_points <- pd_plot_highlight_time()
    
    range <- pd_plot_range()
    view_range <- c(range$min_x, range$max_x)
    
    time_point_paramcd <- ""
    
    if (!is.null(selected_points) && !identical(selected_points, numeric(0)) && nrow(selected_points) && selected_points$time_type[1] == time_type) {
      if (time_type == "interval")
        selected_points <- c(selected_points$from, selected_points$to)
      
      if (time_type == "point") {
        selected_points <- selected_points %>% dplyr::filter(paramcd %in% input$pd_comment_regard)
        if (nrow(selected_points)) {
          time_point_paramcd <- selected_points$paramcd
          selected_points <- selected_points$time
        }
      }
        
    } 
    else {
      selected_points <- numeric(0) 
      time_type <- "profile"
    }
    
    inst_info <- trial_info()
    
    GlobalRV$pd_comment[[inst_info$trial_id]]
    
    comment_data <- pd_comments_data()
    
    GlobalRV$pd_comment[[inst_info$trial_id]] <<-
      writeComment(comment_data, user = initials, profile = profile,
                   regarding = regarding,
                   comment = comment, time_points = selected_points,
                   view_range = view_range,
                   paramcd = paramcd, color_by = color_by, time_type = time_type,
                   time_point_paramcd = time_point_paramcd)
    
    # Initialize the highlight
    pdplotRV$highlighted <- NULL
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})


#-----------------------------------------#
######       pd_modify_comment       ######
#-----------------------------------------#
pd_commentRV <- reactiveValues()

observeEvent(input$pd_write_edit_button_tab, {
  pd_commentRV$edit_comment <- 0
  pd_commentRV$edit_comment <- as.numeric(strsplit(input$pd_write_edit_button_tab, "_")[[1]][6])
})

observeEvent(input$pd_write_edit_button, {
  pd_commentRV$edit_comment <- 0
  pd_commentRV$edit_comment <- as.numeric(strsplit(input$pd_write_edit_button, "_")[[1]][5])
})

observeEvent(pd_commentRV$edit_comment, {
  #print(commentRV$edit_comment )
  req(pd_commentRV$edit_comment, pd_plot_data())
  selected_rows <- pd_commentRV$edit_comment
  
  profile_data_keep <- pd_plot_data()
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  data_keep <- pd_comments_data_profile()
  data_sub <- data_keep[selected_rows, ]
  profile   <- input$pd_select_profile
  
  # get the profiles ranges
  data_adpc <- pddata()
  profiles <- unique(data_adpc$profid)
  range <- pd_time_range() %>% dplyr::filter(profid == profile)

  col_scheme <- pd_col_scheme()
  paramcd_data <- col_scheme %>% dplyr::distinct(paramcd, param)
  panels <- structure(c("Other", paramcd_data$paramcd), names = c("Other", paramcd_data$param))
  
  if (data_sub$regarding == "Other") {
    selected_regard <- "Other"
  }
  else {
    selected_regard <- unlist(data_sub$paramcd)
  }
  
  if (toupper(data_sub$user) != toupper(user) & user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You can only edit comments you have made yourself",
                           type = "warning")
    
  } else {
    
    ui <- tagList(
      h4(glue::glue("Edit comment by {data_sub$user}")),
      
      if (data_sub$time_type == "interval")
        sliderInput("pd_edit_points_selected", "Range of x-axis:",
                    min = range$min_x, max = range$max_x,
                    value = c(unlist(data_sub$time_points)[1], unlist(data_sub$time_points)[2]),
                    round = TRUE, ticks = TRUE),
      
      if (data_sub$time_type == "point" && length(profile_data_keep$time) < 100)
        selectInput("pd_edit_points_selected", label = "Edit selected point(s):",
                    choices = profile_data_keep$time,
                    selected = unlist(data_sub$time_points),
                    multiple = TRUE),
      
      if (data_sub$time_type %in% c("profile", "interval"))
        selectInput(inputId = "pd_edit_comment_regard", "Edit comment regards:",
                    choices = panels, selected = selected_regard, width = "100%",
                    multiple = TRUE),
      
      textInput("pd_edited_comment", label = "Change comment:",
                value = data_sub$comment, width = "90%")
    )
    
    footer = tagList(
      modalButton("Cancel", icon("remove")),
      uiOutput("save_pd_edit_ui",inline = TRUE)
    )
    
    showModal(modalDialog(title = "Edit comment",  ui, footer = footer))
    
  }
})


output$save_pd_edit_ui <- renderUI({
  req(input$pd_edited_comment)
  
  selected_rows <- pd_commentRV$edit_comment
  
  data_keep <- pd_comments_data_profile()
  data_sub  <- data_keep[selected_rows, ]
  
  if (data_sub$time_type %in% c("profile", "interval") && "Other" %in% input$pd_edit_comment_regard)
    validate(need(length(input$pd_edit_comment_regard) == 1 , "'Other' should be chosen alone"))
  
  if (data_sub$time_type %in% c("profile", "interval"))
    validate(need(length(input$pd_edit_comment_regard) > 0 , "All inputs have to be specified"))
  
  col_scheme <- pd_col_scheme()
  selected_param <- col_scheme %>% dplyr::distinct(paramcd, param) %>% 
    dplyr::filter(paramcd %in% input$pd_edit_comment_regard) %>% 
    dplyr::pull(param)
  
  if ("Other" %in% input$pd_edit_comment_regard) {
    regarding <- glue::glue_collapse(input$pd_edit_comment_regard, sep = ", ", last = " and ")
  }
  else {
    regarding <- glue::glue_collapse(selected_param, sep = ", ", last = " and ")
  }
  
  points_selcted <- as.numeric(input$pd_edit_points_selected)
  
  check_points <- length(unlist(data_sub$time_points)) &&
    !is.null(input$pd_edit_points_selected) &&
    !(all(points_selcted %in% unlist(data_sub$time_points)) &
        all(unlist(data_sub$time_points) %in% points_selcted))
  
  
  comment_check <- input$pd_edited_comment != data_sub$comment
  
  if (!is.null(input$pd_edit_comment_regard) & data_sub$time_type %in% c("profile", "interval")) {
    regard_check <- length(c(setdiff(regarding, data_sub$regarding),
                             setdiff(data_sub$regarding, regarding))) > 0
  } else {
    regard_check <- FALSE
  }
  print(regard_check)
  if (check_points | comment_check | regard_check)
    actionButton("save_pd_edit_ui_comment", "Save", icon = icon("check"),
                 style = paste0("color: white; background-color: ",
                                "#3F9C35","; border-color: ","#3F9C35"))
})

observeEvent(input$save_pd_edit_ui_comment, {
  
  
  
  # Indicate that we are saving the comment
  withProgress(message = "saving comment data", value = 0, {
    profile_data_keep <- pd_plot_data()
    
    selected_rows <- pd_commentRV$edit_comment
    
    data_keep <- pd_comments_data_profile()
    data_sub  <- data_keep[selected_rows, ]
    
    col_scheme <- pd_col_scheme()
    selected_param <- col_scheme %>% dplyr::distinct(paramcd, param) %>% 
      dplyr::filter(paramcd %in% input$pd_edit_comment_regard) %>% 
      dplyr::pull(param)
    
    if (data_sub$time_type %in% c("profile", "interval")) {
      
      if ("Other" %in% input$pd_edit_comment_regard) {
        paramcd  <- list("")
        regarding <- glue::glue_collapse(input$pd_edit_comment_regard, sep = ", ", last = " and ")
      }
      
      else {
        paramcd  <- list(input$pd_edit_comment_regard)
        regarding <- glue::glue_collapse(selected_param, sep = ", ", last = " and ")
      }
    } else {
      paramcd <- NULL
      regarding <- NULL
    }
    
    if (data_sub$time_type == "interval" | (data_sub$time_type == "point" && length(profile_data_keep$time) < 100)) {
      time_points <- as.numeric(input$pd_edit_points_selected)
    }
    else {
      time_points <- numeric(0)
    }
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$pd_comment[[trial_info()$trial_id]] <<-
      writeEdit(comment_data = pd_comments_data(),
                comment_id   = pd_comments_data_profile()[pd_commentRV$edit_comment, ]$id,
                comment      = input$pd_edited_comment,
                regarding    = regarding,
                paramcd      = paramcd,
                time_points  = time_points,
                view_range   = c(pd_plot_range()$min_x, pd_plot_range()$max_x))
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})


#-----------------------------------------#
######       pd_Delete               ######
#-----------------------------------------#

observeEvent(input$pd_comment_delete_tab, {
  pd_commentRV$delete_comment <- 0
  pd_commentRV$delete_comment <- as.numeric(strsplit(input$pd_comment_delete_tab, "_")[[1]][5])
})

observeEvent(input$pd_comment_delete, {
  pd_commentRV$delete_comment <- 0
  pd_commentRV$delete_comment <- as.numeric(strsplit(input$pd_comment_delete, "_")[[1]][4])
})


observeEvent(pd_commentRV$delete_comment, {
  
  req(pd_commentRV$delete_comment)
  #print(commentRV$delete_comment)
  selected_rows <- pd_commentRV$delete_comment
  
  user <- setupRV$user
  user_type <- setupRV$user_type
  
  data_keep <- pd_comments_data_profile()
  data_sub <- data_keep[selected_rows, ]
  
  #print(pd_comments_data_profile()[commentRV$delete_comment, ])
  
  if (toupper(data_sub$user) != toupper(user) & user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You can only delete comments you have made yourself",
                           type = "warning")
    
  } else {
    
    ui <- tagList(
      p(em("Regarding: "), data_sub$regarding),
      p(em("Comment: "), data_sub$comment)
    )
    
  
    footer = tagList(modalButton("Cancel"),
                     actionButton("pd_Delete_com", "Delete comment", icon = icon("trash-alt"),
                                  style = paste0("color: white; background-color: ",
                                                 "#E64A0E","; border-color: ","#E64A0E")))
    
    showModal(modalDialog(title = glue::glue("Delete comment by {data_sub$user}"),
                          footer = footer,
                          ui))
  }
})


observeEvent(input$pd_Delete_com, {
  
  # Indicate that we are saving the comment
  withProgress(message = "Deleting comment", value = 0, {
    
    selected_rows <- pd_commentRV$delete_comment
    
    data_keep <- pd_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
    comment_data <- pd_comments_data()
    comment_id <- data_sub$id
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$pd_comment[[trial_info()$trial_id]] <<-
      deleteComment(comment_data = pd_comments_data(),
                    comment_id   = data_sub$id,
                    user         = setupRV$user)
    
    incProgress(1/1, "Deleting", "Done")
  })
  removeModal()
})


#-----------------------------------------#
######          pd_reply             ######
#-----------------------------------------#

observeEvent(input$pd_write_reply_button_tab, {
  pd_commentRV$reply_comment <- 0
  pd_commentRV$reply_comment <- as.numeric(strsplit(input$pd_write_reply_button_tab, "_")[[1]][6])
})

observeEvent(input$pd_write_reply_button, {
  pd_commentRV$reply_comment <- 0
  pd_commentRV$reply_comment <- as.numeric(strsplit(input$pd_write_reply_button, "_")[[1]][5])
})


observeEvent( pd_commentRV$reply_comment, {
  
  req( pd_commentRV$reply_comment)
  selected_rows <-  pd_commentRV$reply_comment
  
  isolate({
    data_keep <- pd_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
  })
  
  
  ui <- tagList(
    h4(glue::glue("Reply to comment by {data_sub$user}")),
    p(em("Regarding: "), data_sub$regarding),
    p(em("Comment: "), data_sub$comment),
    textInput("pd_reply", "Write reply", width = "100%")
  )
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    uiOutput("save_pd_reply_ui",inline = TRUE)
  )
  
  showModal(modalDialog(title = "Reply to comment",  ui, footer = footer))

})

output$save_pd_reply_ui <- renderUI({
  req(input$pd_reply)
 
  actionButton("save_pd_reply", "Save",
               style = paste0("color: white; background-color: ",
                              "#3F9C35",";
                                   border-color: ","#3F9C35"))
})


observeEvent(input$save_pd_reply, {
  req(input$pd_reply)
  
  # Indicate that we are saving the comment
  withProgress(message = "saving comment data", value = 0, {
    
    # save the comment to the gloabal comment data as well as on disk
    GlobalRV$pd_comment[[trial_info()$trial_id]] <<-
      writeReply(comment_data = pd_comments_data(),
                 comment_id    = pd_comments_data_profile()[pd_commentRV$reply_comment, ]$id,
                 user          = setupRV$user,
                 reply         = input$pd_reply)
    
    incProgress(1/1, "Saving", "Done")
  })
  removeModal()
})


#------------------------------------------#
#####  pd_review_meeting buttons      ######
#------------------------------------------#

observeEvent(input$pd_comment_status_tab, {
  pd_commentRV$pd_comment_status <- 0
  pd_commentRV$pd_comment_status <- as.numeric(strsplit(input$pd_comment_status_tab, "_")[[1]][5])
})

observeEvent(input$pd_comment_status, {
  pd_commentRV$pd_comment_status <- 0
  pd_commentRV$pd_comment_status <- as.numeric(strsplit(input$pd_comment_status, "_")[[1]][4])
})


observeEvent( pd_commentRV$pd_comment_status, {
  
  req( pd_commentRV$pd_comment_status)
  selected_rows <-  pd_commentRV$pd_comment_status
  
  isolate({
    data_keep <- pd_comments_data_profile()
    data_sub <- data_keep[selected_rows, ]
  })
  
  
  ui <- tagList(
    h4(glue::glue("Change status for comment by {data_sub$user}")),
    p(em("Regarding: "), data_sub$regarding),
    p(em("Comment: "), data_sub$comment),
    selectInput("pd_status_select", "Select status",  width = "100%",
                            choices = c("Awaiting approval", "No action", "Approved", "Query", "Re-evaluate", "Final"),
                            selected = data_sub$status)
  )
  
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("save_pd_status", "Save", 
                 style = paste0("color: white; background-color: ",
                                "#3F9C35",";
                                       border-color: ","#3F9C35"))
  )

  showModal(modalDialog(title = "Change status for comment",  ui, footer = footer))
})




observeEvent(input$save_pd_status, {
  
  # check for super user
  if (setupRV$user_type != "superuser") {
    shinyalert::shinyalert("Oops!",
                           "You do not have the rights to change status",
                           type = "warning")
    
  } else {
    
    withProgress(message = "saving comment data", value = 0, {
      
      # Indicate that we are saving the comment
      
      # save the comment to the gloabal comment data as well as on disk
      GlobalRV$pd_comment[[trial_info()$trial_id]] <<-
        writeStatus(comment_data = pd_comments_data(),
                    comment_id    = pd_comments_data_profile()[pd_commentRV$pd_comment_status, ]$id,
                    user          = setupRV$user,
                    status        = input$pd_status_select)
      
      incProgress(1/1, "Saving", "Done")
      
      removeModal()
    })
  }
})

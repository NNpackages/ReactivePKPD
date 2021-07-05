#----------------------------------------#
######     Trial create/update      ######
#----------------------------------------#


# update the selections in new/create trial section
observeEvent(setupRV$trial_select_id, {
  
  req(GlobalRV$review_access, nrow(GlobalRV$review_access))
  if (PKPDEnv("verbose")) cat("update_trial_edit_parms\n")
  
  review_access_all <- GlobalRV$review_access
  review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]
  
  updateTextInput(session, "new_trial_project", value = review_access$Project)
  updateTextInput(session, "new_trial_trial", value = review_access$Trial)
  updateTextInput(session, "new_trial_title", value = review_access$Title)
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    updatePathInput(session, "new_trial_datapath", value = review_access$data_path)
    updatePathInput(session, "new_trial_appdata", value = review_access$app_data)
  }
  
  updateSelectizeInput(session, "new_trial_superusers", selected = as.list(unlist(review_access$superusers)),
                       choices = as.list(unlist(review_access$superusers)))
  
  updateSelectizeInput(session, "new_trial_users", selected = as.list(unlist(review_access$users)),
                       choices = as.list(unlist(review_access$users)))
  
  updateCheckboxInput(session, "new_trial_ongoing", value = review_access$ongoing)
})


# when all info fields are specified and saved it is possible to upload data
output$new_trial_save_ui <- renderUI({
  
  validate(
    need(input$new_trial_project, "Project needs to be defined"),
    need(input$new_trial_trial, "Trial needs to be four digits"),
    need(input$new_trial_title, "Protocol title needs to be added"),
    need(input$new_trial_superusers, "Some superuser need to be assigned")
  )
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    if (PKPDEnv("verbose")) cat("checking dir exist\n")
    validate(
      #need(dir.exists(input$new_trial_datapath), "The data path needs to exist"),
      need(dir.exists(input$new_trial_appdata), "The app data path needs to exist")
    )
  }
  actionButton("new_trial_save",   "Save as new trial", width = "150px")
})

# when editing an existing trial it is possible to update
output$new_trial_update_ui <- renderUI({
  req(input$new_trial_project, input$new_trial_trial,
      input$new_trial_title, input$new_trial_superusers)
  
  trial_data_store <- all_trial_data()
  trial_id <- setupRV$trial_select_id
  
  
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    if (PKPDEnv("verbose")) cat("checking dir exist\n")
    req(dir.exists(input$new_trial_appdata))
  }
  
  req(all_trial_data(), setupRV$trial_select_id %in% all_trial_data()$trial_id)
  
  
  req(setupRV$user %in% 
        unlist(trial_data_store[trial_data_store$trial_id == trial_id,]$superusers))
  
  actionButton("new_trial_update", "Update current trial", width = "150px")
})

# When pushing the trial info save button
observeEvent(input$new_trial_save, {
  
  
  trial_id <- paste0("PKPD_review_",
                     gsub(":", "-", gsub(" ", "_", as.character(clockTime()))),
                     "_", sample(100:999, 1))
  
  setupRV$trial_select_id <- trial_id
  setupRV$trial_select <- input$new_trial_trial
  
  trial_info <-
    tibble::tibble(Project    = input$new_trial_project,
                   Trial      = input$new_trial_trial,
                   Title      = input$new_trial_title,
                   superusers = list(toupper(input$new_trial_superusers)),
                   users      = list(toupper(input$new_trial_users)),
                   ongoing    = input$new_trial_ongoing,
                   data_path  = "",
                   app_data   = file.path(PKPDEnv("data_storage"), paste0("data_", trial_id)), # the place to store data, comments
                   trial_id   = trial_id)
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    trial_info$data_path <- input$new_trial_datapath
    trial_info$app_data  <- input$new_trial_appdata
  } else {
    dir.create(trial_info$app_data, showWarnings = FALSE, recursive = TRUE)
  }
  
  file <- trial_id
  
  dir.create(file.path(PKPDEnv("access_data_path"), "trial_setup", paste0("access_", trial_id)),
             showWarnings = FALSE, recursive = TRUE)
  
  time <- gsub("[:-]", "", gsub(" ", "_", as.character(clockTime())))
  
  
  GlobalRV$review_access <<- dplyr::bind_rows(GlobalRV$review_access, trial_info)
  
  setupRV$just_added <- TRUE
  
  saveRDS(trial_info, file.path(PKPDEnv("access_data_path"), "trial_setup", paste0("access_", trial_id),
                                paste0(time, "_", "trial_info", ".rds")))
})

# when clicking update
observeEvent(input$new_trial_update, {
  
  trial_id <- setupRV$trial_select_id
  
  trial_info <-
    tibble::tibble(Project    = input$new_trial_project,
                   Trial      = input$new_trial_trial,
                   Title      = input$new_trial_title,
                   superusers = list(toupper(input$new_trial_superusers)),
                   users      = list(toupper(input$new_trial_users)),
                   ongoing    = input$new_trial_ongoing,
                   data_path  = "",
                   app_data   = file.path(PKPDEnv("data_storage"), paste0("data_", trial_id)), # the place to store data, comments
                   trial_id   = trial_id)
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    trial_info$data_path <- input$new_trial_datapath
    trial_info$app_data  <- input$new_trial_appdata
  } else {
    dir.create(trial_info$app_data, showWarnings = FALSE, recursive = TRUE)
  }
  
  file <- trial_id
  
  dir.create(file.path(PKPDEnv("access_data_path"), "trial_setup",  paste0("access_", trial_id)),
             showWarnings = FALSE, recursive = TRUE)
  
  time <- gsub("[:-]", "", gsub(" ", "_", as.character(clockTime())))
  
  c_access <- GlobalRV$review_access
  
  c_access[c_access$trial_id == trial_id, ] <- trial_info
  
  GlobalRV$review_access <<- c_access
  
  setupRV$just_added <- TRUE
  
  saveRDS(trial_info, file.path(PKPDEnv("access_data_path"), "trial_setup", paste0("access_", trial_id),
                                paste0(time, "_", "trial_info", ".rds")))
})

output$new_trial_save_success <- renderUI({
  req(input$new_trial_project, input$new_trial_trial,
      input$new_trial_title, input$new_trial_superusers)
  
  trial_data_store <- all_trial_data()
  trial_id <- setupRV$trial_select_id
  
  
  
  if (PKPDEnv("data_storage") == "trial_specific") {
    if (PKPDEnv("verbose")) cat("checking dir exist\n")
    req(dir.exists(input$new_trial_appdata))
  }
  
  req(all_trial_data(), setupRV$trial_select_id %in% all_trial_data()$trial_id)
  
  
  req(setupRV$user %in% 
        unlist(trial_data_store[trial_data_store$trial_id == trial_id,]$superusers))
  
  
  box(solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Trial info available", status = "primary",
      h3("The trial info is available!"),
      p("Go to Upload PK data or Upload PD data to upload relevant data")
  )
})
#----------------------------------------#
######     Upload trial data      ######
#----------------------------------------#

#observeEvent(input$new_trial_upload_zip, {

output$zipupload <- renderText({
  
  req(input$new_trial_upload_zip)
  req(rlang::is_string(input$new_trial_upload_zip$datapath))
  isolate({
    file_path <- input$new_trial_upload_zip$datapath
    
    zip_dir <- file.path("zip_archive", basename(tempfile()))
    zip::unzip(file_path, exdir = zip_dir)
    
    copy_dirs <- dir(zip_dir)
    
    trial <- readReviewAccess(file.path(zip_dir, copy_dirs[grep("^access_", copy_dirs)][1]))
    
    if (PKPDEnv("data_storage") != "trial_specific") {
      trial$app_data <- file.path(PKPDEnv("data_storage"), paste0("data_", trial$trial_id))
      
      time <- gsub("[:-]", "", gsub(" ", "_", as.character(clockTime())))
      
      saveRDS(trial,  file.path(zip_dir, copy_dirs[grep("^access_", copy_dirs)][1],
                                paste0(time, "_", "trial_info", ".rds")))
    }
    
    
    file.copy(file.path(zip_dir, copy_dirs[grep("^access_", copy_dirs)]),
              file.path(PKPDEnv("access_data_path"), "trial_setup"),
              recursive = TRUE, overwrite = TRUE, copy.mode = FALSE)
    
    file.copy(file.path(zip_dir, copy_dirs[grep("^data_", copy_dirs)]),
              PKPDEnv("data_storage"),
              recursive = TRUE, overwrite = TRUE, copy.mode = FALSE)
    
    # change view to pk plot
    if (length(dir(file.path(trial$app_data), "data"))) {
      updateTabItems(session, "sideMenu", selected = "plot_profile")
    } else {
      updateTabItems(session, "sideMenu", selected = "setup_trial")
    }
    
    setupRV$trial_select_id <- NULL
    setupRV$trial_select <- trial$Trial
    setupRV$trial_select_id <- trial$trial_id
    
    "upload complete"
  })
})


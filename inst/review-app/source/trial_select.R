#---------------------------------#
######     setup trials      ######
#---------------------------------#

all_trial_data <- reactive({
  
  req(setupRV$user,  !is.null(input$only_ongoing))
  if (PKPDEnv("verbose")) cat("all_trial_data\n")
  
  # All trials
  all <- GlobalRV$review_access
  
  if (nrow(all) == 0) return(NULL)
  
  # User specific trials
  trial_data_store <-
    all[sapply(all$users, function(x) tolower(setupRV$user) %in% tolower(x)) |
          sapply(all$superusers, function(x) tolower(setupRV$user) %in% tolower(x)), ]
  
  # Only include ongoing
  if (input$only_ongoing)
    trial_data_store <-
    trial_data_store %>% dplyr::filter(Trial %in% on_going_review())
  
  # If possible preselect trial
  if (!is.null(PKPDEnv("trial"))) {
    setupRV$trial_select <- PKPDEnv("trial")
    if (PKPDEnv("verbose")) cat(paste("forced trial:",  PKPDEnv("trial")))
    updateTabItems(session, "sideMenu", selected = "plot_profile")
    PKPDEnv("trial", NULL)
    if (PKPDEnv("verbose")) cat(paste("reset to:", PKPDEnv("trial", NULL), "\n"))
  }
  
  if (nrow(trial_data_store) == 1 & is.null(setupRV$trial_select_id)) {
    setupRV$trial_select <- trial_data_store$Trial
    setupRV$trial_select_id <- trial_data_store$trial_id
    if (length(dir(file.path(trial_data_store$app_data), "data"))) {
      updateTabItems(session, "sideMenu", selected = "plot_profile")
    } else {
      updateTabItems(session, "sideMenu", selected = "setup_trial")
    }
  }
  trial_data_store
})


output$setup_all_available <- DT::renderDataTable({
  req(all_trial_data())
  if (PKPDEnv("verbose")) cat("setup_all_available\n")
  trial_data_store <- all_trial_data()
  
  
  
  buttons <- data.frame(goto = shinyInput(actionButton, nrow(trial_data_store),
                                          'setup_trial_sel_button_', label = "Go to Trial",
                                          onclick = 'Shiny.onInputChange(\"setup_trial_sel_button\",  this.id, {priority: \"event\"})' ))
  
  
  shinyInput2 <- function(FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
      inputs[i] <- as.character(FUN(id = paste0(id, i), ...))
    }
    inputs
  }
  
  
  Edit <-
    shinyInput2(tags$button, nrow(trial_data_store), "pk_trial_edit_button_",
                type    = "button",
                class   = "btn btn-primary action-button",
                onclick = 'Shiny.onInputChange("pk_trial_edit_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-edit"), "")
  
  Delete <-
    shinyInput2(tags$button, nrow(trial_data_store), "pk_trial_delete_button_",
                type    = "button",
                class   = "btn btn-danger action-button",
                onclick = 'Shiny.onInputChange("pk_trial_delete_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-trash"), "")
  
  
  super <- sapply(trial_data_store$superusers, function(x) setupRV$user %in% x)
  
  if (!toupper(setupRV$user) %in% toupper(PKPDEnv("admins"))) {
    Edit[!super] <- ""
    Delete[!super] <- ""
  }
  
  comb <- cbind(buttons, 
                trial_data_store[, c("Project", "Trial", "Title", "ongoing")], 
                data.frame(Edit = Edit), 
                data.frame(Delete = Delete))
  
  return(comb)
},

server = TRUE, escape = FALSE, selection = 'single' ,

options = list(
  #        paging=TRUE,
  preDrawCallback = DT::JS('function() {
                       Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = DT::JS('function() {
                    Shiny.bindAll(this.api().table().node()); } ')
)
)


#-----------------------------------#
######     delete trial        ######
#-----------------------------------#

observeEvent(input$pk_trial_delete_button, {
  
  selectedRow <- as.numeric(strsplit(input$pk_trial_delete_button, "_")[[1]][5])
  all_trials <- all_trial_data()
  
  trial <- all_trials[rownames(all_trials) == selectedRow, ]
  
  setupRV$trial_to_delete <- trial
  
  ui <- tagList(
    p(em("Project: "), trial$Project),
    p(em("Trial: "), trial$Trial),
    p(em("Title: "), trial$Title),
    p(""),
    p(em("Warning:"), "This is a non-reversible action", style = "color:red;")
    
    
  )
  
  showModal(modalDialog(title = glue::glue("Delete all data for {trial$Project}-{trial$Trial}"),
                        footer = tagList(modalButton("Cancel"),
                                         actionButton("pk_Delete_trial", "Delete trial",style = paste0("color: white; background-color: ","#E64A0E","; border-color: ","#E64A0E"))),
                        ui))
})


observeEvent(input$pk_Delete_trial, {
  
  if (PKPDEnv("verbose")) cat("pk_Delete_trial\n")
  
  trial <- setupRV$trial_to_delete
  
  if (dir.exists(trial$app_data))
    unlink(trial$app_data)
  
  if (dir.exists(file.path(PKPDEnv("access_data_path"), "trial_setup", paste0("access_", trial$trial_id))))
    unlink(file.path(PKPDEnv("access_data_path"), "trial_setup", paste0("access_", trial$trial_id)), 
           recursive = TRUE)
  
  c_access <- GlobalRV$review_access
  
  GlobalRV$review_access <<- c_access[c_access$trial_id != trial$trial_id, ] 
  
  removeModal()
})

#-----------------------------------#
######     select trial        ######
#-----------------------------------#

observeEvent(input$pk_trial_edit_button, {
  if (PKPDEnv("verbose")) cat("pk_trial_edit_button\n")
  selectedRow <- as.numeric(strsplit(input$pk_trial_edit_button, "_")[[1]][5])
  all_trials <- all_trial_data()
  
  trial <- all_trials[rownames(all_trials) == selectedRow, ]
  
  # change view to pk plot
  updateTabItems(session, "sideMenu", selected = "trial_info")
  
  setupRV$trial_select_id <- NULL
  setupRV$trial_select <- trial$Trial
  setupRV$trial_select_id <- trial$trial_id
  #shinyjs::js$activateTab("setup_trial")
})




observeEvent(input$setup_trial_sel_button, {
  if (PKPDEnv("verbose")) cat("setup_trial_sel_button\n")
  selectedRow <- as.numeric(strsplit(input$setup_trial_sel_button, "_")[[1]][5])
  all_trials <- all_trial_data()
  
  trial <- all_trials[rownames(all_trials) == selectedRow, ]
  
  # change view to pk plot
  if (length(dir(file.path(trial$app_data), "data"))) {
    updateTabItems(session, "sideMenu", selected = "plot_profile")
  } else {
    updateTabItems(session, "sideMenu", selected = "setup_trial")
  }
  
  setupRV$trial_select_id <- NULL
  setupRV$trial_select <- trial$Trial
  setupRV$trial_select_id <- trial$trial_id
})


trial_info <- reactive({
  
  req(setupRV$user, setupRV$trial_select_id, all_trial_data())
  if (PKPDEnv("verbose")) cat("trial_info\n")
  
  # all trials info
  review_access_all <- all_trial_data()
  review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]
  
  # user info
  if (PKPDEnv("verbose")) cat(paste("selected_trial", setupRV$trial_select, "\n"))
  
  if (setupRV$user %in% unlist(review_access$superusers)) {
    setupRV$user_type <- "superuser"
  } else if (setupRV$user %in% unlist(review_access$users)) {
    setupRV$user_type <- "reviewer"
  } else {
    setupRV$user_type <- "Visitor"
  }
  
  # Get all data directories
  dirs <- list.dirs(file.path(review_access$app_data, "data"))
  
  # subset to actual data directories
  dirs <- dirs[grep("review_round_", dirs)]
  
  pk_dirs <- sapply(file.path(dirs, "app_adpc.rds"), file.exists)
  pd_dirs <- sapply(file.path(dirs, "app_adpd.rds"), file.exists)
  
  setupRV$trial_pk_data_dir <- file.path(review_access$app_data, "data", paste0("review_round_", ifelse(length(pk_dirs) && any(pk_dirs), max(which(pk_dirs)), 0)))
  setupRV$trial_pd_data_dir <- file.path(review_access$app_data, "data", paste0("review_round_", ifelse(length(pd_dirs) && any(pd_dirs), max(which(pd_dirs)), 0)))
  
  if (PKPDEnv("verbose")) cat(paste("user type:", setupRV$user_type, "\n"))
  
  # Get the reviewers
  setupRV$reviewers <-
    unique(c(unlist(review_access[review_access$Trial == setupRV$trial_select, "superusers"]),
             unlist(review_access[review_access$Trial == setupRV$trial_select, "users"])))
  
  out <- list(
    user              = setupRV$user,
    user_type         = setupRV$user_type,
    reviewers         = setupRV$reviewers,
    trial_id          = setupRV$trial_select_id,
    trial             = setupRV$trial_select,
    trial_app_dir     = review_access$app_data,
    trial_pk_data_dir = setupRV$trial_pk_data_dir,
    trial_pd_data_dir = setupRV$trial_pd_data_dir
  )
  
  return(out)
})



#------------------------------------------------------------#
###### Function to decouple globla observes from local  ######
#------------------------------------------------------------#

dedupe <- function(rexpr, domain = getDefaultReactiveDomain()) {
  force(rexpr)
  x <- reactiveVal()
  observe({
    x(tryCatch(
      {
        list(success = rexpr())
      },
      error = function(e) {
        list(error = e)
      }
    ))
  }, domain = domain)
  
  reactive({
    result <- x()
    if (!is.null(result$success))
      result$success
    else
      stop(result$error)
  })
}

#---------------------------------#
###### Global info shared    ######
#---------------------------------#

options(warn = 0)
#options(warn = 2)
options(shiny.maxRequestSize = 30*1024^2) 

# update ongoing trials every 10 minutes

checkReviewAccess <- function(path = file.path(PKPDEnv("access_data_path"), "trial_setup")) {
  list.files(path, pattern = ".rds", full.names = TRUE, recursive = TRUE)
}


readReviewAccess <- function(path = file.path(PKPDEnv("access_data_path"), "trial_setup/")) {
  checkfile <- list.dirs(path)

  files <- lapply(checkfile, list.files,  pattern = ".rds", full.names = TRUE)

  newest_files <- lapply(files, function(x) x[length(x)])

  avail_files <- newest_files[seq_along(newest_files)[unlist(lapply(newest_files, length)) > 0]]

  l <- lapply(avail_files, readRDS)

  dplyr::bind_rows(l)
}

GlobalRV <- reactiveValues(data       = list(),
                           comments   = list(),
                           online     = list(),
                           review_access = readReviewAccess(),
                           avatar_col = tibble::tibble())


# Function for reading avatar colour files
readAvatars <- function(path = file.path(PKPDEnv("access_data_path"), "avatars"),
                        current = NULL) {
  
  files <- files_read <- list.files(path, pattern = "[.rds][.csv]", full.names = TRUE)
  
  if (!is.null(current))
    files_read <- setdiff(files, attr(current, "file_track"))
  
  if (length(files_read)) { 
    read_file_by_type <- function(file) {
      switch(tools::file_ext(file),
             rds = readRDS(file),
             csv = readTable(file))
    }
    l <- lapply(files, read_file_by_type)
    avatar_col <- dplyr::bind_rows(l)
  } else {
    avatar_col <- tibble::tibble(user = character(0), col = character(0))
  }
  
  if (!is.null(current))
    avatar_col <- dplyr::bind_rows(current, avatar_col)
  
  attr(avatar_col, "file_track") <- files
  avatar_col
}


# when the server stops this is run
onStop(fun = function() {
  message("The server has shut down")

  PKPDEnv(c("online_user", "trial", "username", 
            "workspace", "verbose", "libref",
            "access_data_path", "runApp", "admins"), remove = TRUE)
})

shinyServer(function(input, output, session) {

  session.id <- session$token
  
  #----------------------------------------#
  ######      On login/logout         ######
  #----------------------------------------#
  
  source("source/log_on_out.R", local = TRUE)$value

  #----------------------------------------#
  ######         Trial select         ######
  #----------------------------------------#
  
  source("source/trial_select.R", local = TRUE)$value

  source("source/trial_setup.R", local = TRUE)$value
  
  #----------------------------------------#
  ######            pk plot           ######
  #----------------------------------------#
  
  source("source/pk_upload_data.R", local = TRUE)$value
  
  source("source/pk_load_data.R", local = TRUE)$value
  
  source("source/pk_comment_init.R", local = TRUE)$value  
  
  source("source/pk_plot.R", local = TRUE)$value
  
  source("source/pk_comment.R", local = TRUE)$value  
  
  #----------------------------------------#
  ######            pd plot           ######
  #----------------------------------------#
  
  source("source/pd_upload_data.R", local = TRUE)$value
  
  source("source/pd_load_data.R", local = TRUE)$value
  
  source("source/pd_comment_init.R", local = TRUE)$value  
  
  source("source/pd_plot.R", local = TRUE)$value
  
  source("source/pd_comment.R", local = TRUE)$value  
  
  #----------------------------------------#
  ######       active review status   ######
  #----------------------------------------#
  
  
  output$review_status_ui <-  renderMenu({
    # reactive on sidebar choice
    what <- input$sideMenu
    
    items <- NULL

    if (what == "plot_profile") {
      req(pk_review_status_items())
     items <- pk_review_status_items()
    }
    
    if (what == "plot_pd_profile") {
      req(pd_review_status_items())
     items <- pd_review_status_items()
    }    
    
    if (!is.null(items))
      dropdownMenu(
        type = "tasks", badgeStatus = "primary", headerText = "Review progress",
        .list = items
      )
  })
  
  #----------------------------------------#
  ######     Download trial data      ######
  #----------------------------------------#

  source("source/download_trial_data.R", local = TRUE)$value 

  #----------------------------------------#
  ######     Upload trial data      ######
  #----------------------------------------#
  #
  source("source/upload_trial_data.R", local = TRUE)$value 
  
  #-----------------------------------------#
  ######        usage_information      ######
  #-----------------------------------------#

  #session_info_re <- checkPKPDEnv(500, check = "session_info", name = "session_info")

  login_info <-
    reactivePoll(500, NULL, checkFunc = function() loginInfo(TRUE),
               valueFunc = loginInfo)

  output$review_overview <- DT::renderDataTable({
    req(login_info())
    if (PKPDEnv("verbose")) cat("initialising review table\n")

    return(login_info())
    #req(setupRV$user)


    setupRV$user

    session_info <- session_info_re()

    session_info[grep(" ", names(session_info), invert = TRUE)] <- NULL

    data <- dplyr::bind_rows(lapply(session_info, tibble::as_tibble))

    data[data$online, "logout_time"] <- Sys.time()

    data
  })
})














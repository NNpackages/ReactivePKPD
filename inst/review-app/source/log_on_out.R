review_access <- reactive({
  GlobalRV$review_access
})


# ongoing review
on_going_review <- reactive({
  if (PKPDEnv("verbose")) cat("on_going_review\n")
  all <- review_access()
  all[all$ongoing, ]$Trial
})

# all user that may log in to the app
app_users <- reactive({
  if (PKPDEnv("verbose")) cat("app_users\n")
  all <- review_access()
  
  if (nrow(all)) {
    app_users <- toupper(unique(c(unlist(all$superusers), unlist(all$users))))
  } else {
    app_users <- c()
  }
  
  unique(c(app_users, toupper(PKPDEnv("admins"))))
})

# colour for the user avatars
avatar_colors <- reactive({
  if (PKPDEnv("verbose")) cat("avatar_colors\n")
  user <- app_users()
  sample(nncol$company[-1], length(user), replace = length(user) >= length(nncol$company))
  
  isolate({ 
    GlobalRV$avatar_col <<- 
      avatar_colors <- readAvatars(current = GlobalRV$avatar_col)
  })
  
  new_user <- setdiff(c(user), avatar_colors$user)
  
  if (length(new_user)) {
    
    new_colors <-
      tibble::tibble(user = new_user,
                     col  = sample(nncol$company[-1], length(user),
                                   replace = length(user) >= length(nncol$company)))
    
    lapply(seq_len(nrow(new_colors)), function(i) { 
      writeTable(new_colors[i,], file.path(PKPDEnv("access_data_path"), "avatars", paste0(new_user[i], ".csv")))
    })
    
    avatar_colors <- dplyr::bind_rows(avatar_colors, new_colors)
  }
  
  return(avatar_colors)
})
#---------------------------------#
######        log-in       ######
#---------------------------------#

setupRV <- reactiveValues(user = "", timeline = TRUE, pd_timeline = TRUE)

if (PKPDEnv("workspace") == "server") {
  
  showModal(modalDialog(footer = "",
                        title = "Login",
                        
                        splitLayout(cellWidths = c("80%", "20%"),
                                    textInput(inputId = "username", "Initials:"),
                                    div(br(),
                                        actionButton("login", "Login", style = paste0("color: white; background-color: ",
                                                                                      "#3F9C35",";border-color: ","#3F9C35"))
                                    ))
  ))
} else if (PKPDEnv("workspace") == "shinyServer") {
  reactive(app_users())
  setupRV$user <- toupper(session$user)
  updateLoginInfo(session.id, toupper(session$user), login = TRUE)
} else {
  reactive(app_users())
  setupRV$user <- toupper(PKPDEnv("username"))
  updateLoginInfo(session.id, PKPDEnv("username"), login = TRUE)
}


observeEvent(input$login, {
  if (PKPDEnv("verbose")) cat(paste("login-started", input$username, "\n"))
  if (input$username == "") {
    shinyalert::shinyalert("Oops!","Initials not filled in",type = "warning")
  }
  
  else if (!(toupper(input$username) %in% app_users())) {
    setupRV$user <- ""
    shinyalert::shinyalert("Oops!","You do not have permission to use this app",type = "warning")
  }
  else {
    removeModal()
    # Assign the username
    setupRV$user <- toupper(input$username)
    
    # updateLoginInfo
    updateLoginInfo(session.id, toupper(input$username), login = TRUE)
    
    # Reset the textInput
    updateTextInput(session, "username", "")
  }
})

#---------------------------------#
######        avatar         ######
#---------------------------------#

avatar_link <- reactive({
  if (PKPDEnv("verbose")) cat("avatar_link\n")
  req(avatar_colors())
  avatar_colors <- avatar_colors()
  
  if (!is.null(setupRV$user) && setupRV$user != "") {
    
    if (setupRV$user %in% avatar_colors$user) {
      color <- avatar_colors[avatar_colors$user == setupRV$user, ]$col
    } else {
      color <- sample(nncol$company[-1], 1)
    }
    
    file <- file.path("www", "avatars", paste0(tolower(setupRV$user), ".png"))
    dir.create(file.path("www", "avatars"), showWarnings = FALSE)
    create_avatar(setupRV$user, save_png = TRUE, file = file, type = "square",
                  col = color)
    
    return(file.path("avatars", paste0(tolower(setupRV$user), ".png")))
  }
})

output$userpanel <- renderUI({
  if (PKPDEnv("verbose")) cat("userpanel\n")
  req(avatar_link())
  
  text <- "Logged in"
  
  if (!is.null(setupRV$trial_select) && setupRV$trial_select != "")
    text <- paste(text, "to", setupRV$trial_select)
  
  if (!is.null(setupRV$user_type) && setupRV$user_type != "")
    text <- paste(text, "as", setupRV$user_type)
  
  if (PKPDEnv("verbose")) cat(paste("userpanel:", text, setupRV$user, "\n"))
  if (!is.null(setupRV$user) && setupRV$user != "") {
    
    if (PKPDEnv("workspace") == "shinyServer") {
      subtitle <- a(icon("sign-out"), "Logout", href = "__logout__")
    } else {
      subtitle <- actionLink("signout_link", "Logout", icon = icon("sign-out"))
    }
    
    sidebarUserPanel(
      name     = span(text),
      subtitle = subtitle,
      image    = avatar_link()
    )
  }
})

#---------------------------------#
######        signout        ######
#---------------------------------#

observeEvent(input$signout_link, {
  message(setupRV$user, " signing out")
  updateTabItems(session, "sideMenu", selected = "select_trial")
  
  # update session_info
  updateLoginInfo(session.id, setupRV$user, logout = TRUE)
  
  # set user info
  setupRV$user <- ""
  setupRV$trial_select <- ""
  setupRV$user_type <- ""
  pkplotRV$highlighted <- NULL
  setupRV$trial_id <- NULL
  
  showModal(modalDialog(footer = "",
                        title = "Login",
                        
                        splitLayout(cellWidths = c("80%", "20%"),
                                    textInput(inputId = "username", "Initials:"),
                                    div(br(),
                                        actionButton("login", "Login", style = paste0("color: white; background-color: ",
                                                                                      "#3F9C35",";
                                                             border-color: ","#3F9C35"))
                                    ))
  ))
})

#---------------------------------#
######       On stop         ######
#---------------------------------#

onStop(fun = function(env_id = session.id) {
  
  message("exit by ", session.id)
  
  # Update session_info
  updateLoginInfo(session.id, logout = TRUE)
})

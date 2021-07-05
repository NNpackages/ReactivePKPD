#----------------------------------------#
######        upload PD data          ######
#----------------------------------------#


output$new_pd_trial_upload_file <- renderUI({
  
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
  
  req(all_trial_data(), setupRV$trial_select_id %in% all_trial_data()$trial_id)
  
  tagList(
    box(solidHeader = TRUE, collapsible = TRUE, width = 12,
        title = "Data selector", status = "primary",
        shiny::fileInput(inputId = "new_pd_trial_new_data", "Upload data"),
        #verbatimTextOutput("file_location"),
        uiOutput("new_pd_data_choose"),
        uiOutput("new_pd_data_subset")
    ),
    box(solidHeader = TRUE, collapsible = TRUE, width = 12,
        title = "Data viewer", status = "primary",
        dataTableOutput("uploaded_pd_data")
    )
  )
})

uploaded_pd_data <- reactive({
  req(input$new_pd_trial_new_data$datapath)
  file_path <- input$new_pd_trial_new_data$datapath
  
  # Delete file after exit
  on.exit(unlink(file_path))
  
  ext <- tools::file_ext(file_path)
  
  ds <- switch(ext,
               "sas7bdat" = haven::read_sas(data_file = file_path),
               "csv"      = utils::read.csv(file = file_path),
               "rds"      = readRDS(file = file_path),
               "xlsx"     = readxl::read_xlsx(path = file_path))
  
  
  ds
})

output$new_pd_data_choose <- renderUI({
  req(uploaded_pd_data())
  ds <- uploaded_pd_data()
  
  # The classes of the columns in data
  classes <- sapply(ds, class)
  
  # Choices for unique profile ID
  if("FSUBJID" %in% names(classes)){
    choices_prof <- c("FSUBJID",names(classes[classes == "character"])) %>% unique()
  }
  else{
    choices_prof <- names(classes[classes == "character"])
  }
  
  # Function for finding relevant columns
  selectRel <- function(available, prefered) {
    
    selected <- ""
    
    prefered <- paste0("^", c(prefered), "$")
    
    i <- 1
    while (selected == "" & i <= length(prefered)) {
      check <- grep(prefered[i], available, ignore.case = TRUE)
      if (length(check))
        selected <- available[check]
      i <- i + 1
    }
    
    if (all(selected == "") & any(grepl("None", prefered)))
      selected <- "None"
    
    return(selected)
  }
  tagList(
    splitLayout(cellWidths = c(rep(paste0(98/4, "%"), 4), "2%"),
                selectInput("new_pd_data_profid", "Unique profile ID", choices =  choices_prof,
                            selected = c(selectRel(available = choices_prof, prefered = c("FSUBJID", "SUBJID", "USUBJID")),
                                         selectRel(available = choices_prof, prefered = c("PROFID"))),
                            multiple = TRUE),
                selectInput("new_pd_data_panelcd_sel", "Paramcd selector", choices =  names(classes[classes == "character"]),
                            selected = selectRel(available = names(classes[classes == "character"]), prefered = c("PARAMCD", "TOPICCD", "TOPIC_CD"))),
                selectInput("new_pd_data_panel_sel", "Param selector", choices =  names(classes[classes == "character"]),
                            selected = selectRel(available = names(classes[classes == "character"]), prefered = c("PARAM"))),
                selectInput("new_pd_data_color_by", "Color by selector", choices =   c("None", names(classes[classes == "character"])),
                            selected = selectRel(available = names(classes[classes == "character"]), prefered = c("None", "TYPE"))),
                tags$div()
    ),
    splitLayout(cellWidths = c(rep(paste0(98/3, "%"), 3), "2%"),
                selectInput("new_pd_data_nom_time", "Nominel time", choices = c("None", names(classes[classes == "numeric"])),
                            selected = selectRel(names(classes[classes == "numeric"]), c("nomtime", "time", "ATPT1N", "ATPT2N", "ATPTN"))),
                selectInput("new_pd_data_time", "Time in plots", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("time", "AREL1TM", "AREL2TM", "ARELTM1", "ARELTM2", "ARELTM", "ATPT1N", "ATPT2N", "ATPTN"))),
                selectInput("new_pd_data_time_unit", "Time unit", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("time_unit", "timeunit", "AREL1TMU", "AREL2TMU", "ARELTM1U", "ARELTM2U", "ARELTMU", "ATPT1NU", "ATPT2NU", "ATPTNU", "None"))),
                tags$div()
    ),
    splitLayout(cellWidths = c(rep(paste0(98/6, "%"), 6), "2%"),
                selectInput("new_pd_data_aval",   "Analysis value", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("AVAL"))),
                
                selectInput("new_pd_data_lower",   "Lower border", choices = c("None", names(classes[classes == "numeric"])),
                            selected = selectRel(names(classes[classes == "numeric"]), c("A1LO", "LOWER_CONC", "LOW_CONC", "LOWCONC", "LOWER", "None"))),
                selectInput("new_pd_data_upper",   "Upper border", choices = c("None", names(classes[classes == "numeric"])),
                            selected = selectRel(names(classes[classes == "numeric"]), c("A1HI", "UPPER_CONC", "UP_CONC", "UPCONC", "UPPER", "None"))),
                
                selectInput("new_pd_data_unit", "Unit", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("AVALU", "PCORRESU", "None"))),
                selectInput("new_pd_data_highlight", "Markers", choices =  c("None", names(classes[classes == "character"]))),
                selectInput("new_pd_data_other", "Other variables", choices = colnames(ds), multiple = TRUE),
                tags$div()
    )
  )
})

output$new_pd_data_subset <- renderUI({
  req(uploaded_pd_data())
  tagList(
    splitLayout(cellWidths = c("80%","1%", "9%", "9%"),
                textInput("new_pd_data_subset_by_text", "Subset the data"),
                div(),
                div(br(),actionButton("new_pd_data_do_subset", "Perform subset")),
                div(br(),actionButton("new_pd_data_save", "Save the data"))
    ),
    verbatimTextOutput("saved_pd_data_path")
  )
})


new_pd_data_subsetted <- reactive({
  req(uploaded_pd_data(), input$new_pd_data_profid, input$new_pd_data_panelcd_sel, input$new_pd_data_panel_sel, input$new_pd_data_time, input$new_pd_data_aval)
  
  ds <- uploaded_pd_data()
  
  input$new_pd_data_do_subset
  
  # isolate so that it doesn't update everytime you enter something
  isolate({
    if (!is.null(input$new_pd_data_subset_by_text) && input$new_pd_data_subset_by_text != "") {
      
      eval(parse(text = paste0("res <- try(dplyr::filter(ds, ",
                               input$new_pd_data_subset_by_text, "))")))
      
      
      if (inherits(res, 'try-error')) {
        showNotification(I(res), duration = 15, type = "error")
      } else {
        if (nrow(res) == 0) {
          showNotification("The where clause gives 0 rows", duration = 15,
                           type = "error")
        } else {
          ds <- res
        }
      }
    }
  })
  
  ds$new_unique_prof_var <- apply(ds[, input$new_pd_data_profid], 1, paste, collapse = " ")
  
  cols <- c("new_unique_prof_var",  
            input$new_pd_data_panelcd_sel, input$new_pd_data_panel_sel,
            if (input$new_pd_data_color_by != "None") {input$new_pd_data_color_by},
            input$new_pd_data_time,
            if (input$new_pd_data_nom_time != "None") {input$new_pd_data_nom_time},
            if (input$new_pd_data_time_unit != "None") {input$new_pd_data_time_unit},
            input$new_pd_data_aval,
            if (input$new_pd_data_lower != "None") {input$new_pd_data_lower},
            if (input$new_pd_data_upper != "None") {input$new_pd_data_upper},
            if (input$new_pd_data_unit != "None") {input$new_pd_data_unit},
            if (input$new_pd_data_highlight != "None") {input$new_pd_data_highlight},
            if (!is.null(input$new_pd_data_other) && input$new_pd_data_other != "") {
              input$new_pd_data_other})
  
  ds <- ds[, cols]
  
  colnames(ds) <- c("profid", "paramcd", "param",
                    if (input$new_pd_data_color_by != "None") {"color_by"},
                    "time", 
                    if (input$new_pd_data_nom_time != "None") {"nom_time"},
                    if (input$new_pd_data_time_unit != "None") {"time_unit"},
                    "aval",
                    if (input$new_pd_data_lower != "None") {"lower"},
                    if (input$new_pd_data_upper != "None") {"upper"},
                    if (input$new_pd_data_unit != "None") {"aval_unit"},
                    if (input$new_pd_data_highlight != "None") {"highlight"},
                    if (!is.null(input$new_pd_data_other) && input$new_pd_data_other != "") {
                      input$new_pd_data_other})
  
  ds
})


output$uploaded_pd_data <- renderDataTable({
  req(new_pd_data_subsetted())
  
  new_name <- paste0(input$new_pd_data_profid, collapse = "/")
  
  ds <- new_pd_data_subsetted()
  
  colnames(ds) <- c(new_name,  
            input$new_pd_data_panelcd_sel, input$new_pd_data_panel_sel,
            if (input$new_pd_data_color_by != "None") {input$new_pd_data_color_by},
            input$new_pd_data_time,
            if (input$new_pd_data_nom_time != "None") {input$new_pd_data_nom_time},
            if (input$new_pd_data_time_unit != "None") {input$new_pd_data_time_unit},
            input$new_pd_data_aval,
            if (input$new_pd_data_lower != "None") {input$new_pd_data_lower},
            if (input$new_pd_data_upper != "None") {input$new_pd_data_upper},
            if (input$new_pd_data_unit != "None") {input$new_pd_data_unit},
            if (input$new_pd_data_highlight != "None") {input$new_pd_data_highlight},
            if (!is.null(input$new_pd_data_other) && input$new_pd_data_other != "") {
              input$new_pd_data_other})
  
  ds
})



observeEvent(input$new_pd_data_save, {
  
  review_access_all <- GlobalRV$review_access
  review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]
  
  app_data_dir <- review_access$app_data
  
  if (!dir.exists(file.path(app_data_dir, "data"))) {
    dir.create(file.path(app_data_dir, "data"))
  }
  
  dirs <- list.dirs(file.path(app_data_dir, "data"))
  
  # subset to actual dirs
  dirs <- dirs[grep("review_round_", dirs)]
  
  pd_dirs <- sapply(file.path(dirs, "app_adpd.rds"), file.exists)
  
  n.dirs <- ifelse(length(pd_dirs) && any(pd_dirs), max(which(pd_dirs)), 0)
  
  ava1 <- c(n.dirs, n.dirs + 1)
  
  choices <- structure(file.path(app_data_dir, "data", paste0("review_round_", ava1[ava1 > 0])),
                       names = paste("Round", ava1[ava1 > 0]))
  
  ui <- tagList(
    splitLayout(cellWidths = c("80%", "20%"),
                selectInput("new_pd_data_R_round", "Select the review round to save the data for",
                            width = "90%",
                            choices = choices),
                div(br(),
                    actionButton("new_pd_data_save_modal", "Save", width = "90%",
                                 style = paste0("color: white; background-color: ",
                                                "#3F9C35",";
                                       border-color: ","#3F9C35"))
                ))
  )
  showModal(modalDialog(title = "Save the PD data",  ui))
})



output$saved_pd_data_path <- renderText({file.path(input$new_pd_data_R_round, "app_adpd.rds")})

observeEvent(input$new_pd_data_save_modal, {
  
  isolate({
    req(new_pd_data_subsetted(), input$new_pd_data_R_round)
    ds <- new_pd_data_subsetted()
    
    print(dir(file.path(input$new_pd_data_R_round), ".."))
    
    print(paste("create dir", file.path(input$new_pd_data_R_round)))
    dir.create(file.path(input$new_pd_data_R_round), showWarnings = FALSE, recursive = TRUE)
    
    print(paste("input$new_trial_trial", input$new_trial_trial))
    
    GlobalRV$PDdata[[setupRV$trial_select_id]] <<- ds
    saveRDS(ds, file.path(input$new_pd_data_R_round, "app_adpd.rds"))
    
    # Force reset of datadir for new data
    setupRV$trial_pd_data_dir <- ""
    setupRV$trial_pd_data_dir <- input$new_pd_data_R_round
    
    # change view to pd plot
    updateTabItems(session, "sideMenu", selected = "plot_pd_profile")
    
    setupRV$trial_select <- input$new_trial_trial
    
    removeModal()
    file.path(input$new_pd_data_R_round, "app_adpd.rds")
  })
})
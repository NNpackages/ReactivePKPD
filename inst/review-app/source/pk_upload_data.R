#----------------------------------------#
######        upload PK data        ######
#----------------------------------------#


output$new_trial_upload_file <- renderUI({
  
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
        shiny::fileInput(inputId = "new_trial_new_data", "Upload data"),
        #verbatimTextOutput("file_location"),
        uiOutput("new_data_choose"),
        uiOutput("new_data_subset")
    ),
    box(solidHeader = TRUE, collapsible = TRUE, width = 12,
        title = "Data viewer", status = "primary",
        dataTableOutput("uploaded_data")
    )
  )
})

uploaded_data <- reactive({
  req(input$new_trial_new_data$datapath)
  file_path <- input$new_trial_new_data$datapath
  
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

output$new_data_choose <- renderUI({
  req(uploaded_data())
  ds <- uploaded_data()
  
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
    h3("Profile info"),
    splitLayout(cellWidths = c(rep(paste0(98/3, "%"), 3), "2%"),
                selectInput("new_data_profid", "Unique profile ID", choices =  choices_prof,
                            selected = c(selectRel(available = choices_prof, prefered = c("FSUBJID", "SUBJID", "USUBJID")),
                                         selectRel(available = choices_prof, prefered = c("PROFID"))),
                            multiple = TRUE),
                selectInput("new_data_type", "Profile type", choices = c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("TYPE", "PROFID", "None"))),
                selectInput("new_data_drug", "Drug", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("TRTA", "TRTP", "DRUG", "None"))),
                tags$div()
    ),
    h3("Time variables"),
    splitLayout(cellWidths = c(rep(paste0(98/3, "%"), 3), "2%"),
                selectInput("new_data_time", "Nominal time", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("nomtime", "time", "ATPT1N", "ATPT2N", "ATPTN"))),
                selectInput("new_data_plot_time", "Time in plots", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("time", "AREL1TM", "AREL2TM", "ARELTM1", "ARELTM2", "ARELTM", "ATPT1N", "ATPT2N", "ATPTN"))),
                selectInput("new_data_time_unit", "Time unit", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("time_unit", "timeunit", "AREL1TMU", "AREL2TMU", "ARELTM1U", "ARELTM2U", "ARELTMU", "ATPT1NU", "ATPT2NU", "ATPTNU", "None"))),
                tags$div()
    ),
    h3("Concentration variables"),
    splitLayout(cellWidths = c(rep(paste0(98/4, "%"), 4), "2%"),
                selectInput("new_data_aval",   "Concentration", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("AVAL"))),
                selectInput("new_data_lower",  "Lower concentration", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("A1LO", "LOWER_CONC", "LOW_CONC", "LOWCONC", "AVAL"))),
                selectInput("new_data_upper",  "Upper concentration", choices = names(classes[classes == "numeric"]),
                            selected = selectRel(names(classes[classes == "numeric"]), c("A1HI", "UPPER_CONC", "UP_CONC", "UPCONC", "AVAL"))),
                selectInput("new_data_unit", "Unit", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("AVALU", "PCORRESU", "conc_unit", "concunit", "None"))),
                tags$div()
    ),
    h3("Other variables"),
    splitLayout(cellWidths = c(rep(paste0(98/5, "%"), 5), "2%"),
                selectInput("new_data_anelfl", "Eligibility flag", choices = colnames(ds),
                            selected = selectRel(colnames(ds), c("ANELFL"))),
                selectInput("new_data_tailfl", "Tail flag", choices = colnames(ds),
                            selected = selectRel(colnames(ds), c("TAILFL"))),
                selectInput("new_data_comment", "Lab comment", choices =  c("None", names(classes[classes == "character"])),
                            selected = selectRel(names(classes[classes == "character"]), c("PCCOM", "lab_comment", "labcomment", "None"))),
                selectInput("new_data_highlight", "Markers", choices =  c("None", names(classes[classes == "character"]))),
                selectInput("new_data_other", "Other variables", choices = colnames(ds), multiple = TRUE),
                tags$div()
    )
  )
})

output$new_data_subset <- renderUI({
  req(uploaded_data())
  tagList(
    splitLayout(cellWidths = c("80%","1%", "9%", "9%"),
                textInput("new_data_subset_by_text", "Subset the data"),
                div(),
                div(br(),actionButton("new_data_do_subset", "Perform subset")),
                div(br(),actionButton("new_data_save", "Save the data"))
    ),
    verbatimTextOutput("saved_data_path")
  )
})


new_data_subsetted <- reactive({
  req(uploaded_data(), input$new_data_profid, input$new_data_time, input$new_data_plot_time, input$new_data_aval,
      input$new_data_lower, input$new_data_upper, input$new_data_anelfl, input$new_data_tailfl)
  
  ds <- uploaded_data()
  
  input$new_data_do_subset
  
  # isolate so that it doesn't update everytime you enter something
  isolate({
    if (!is.null(input$new_data_subset_by_text) && input$new_data_subset_by_text != "") {
      
      eval(parse(text = paste0("res <- try(dplyr::filter(ds, ",
                               input$new_data_subset_by_text, "))")))
      
      
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
  
  
  ds$new_unique_prof_var <- apply(ds[, input$new_data_profid], 1, paste, collapse = " ")
  
  
  cols <- c("new_unique_prof_var", 
            if (input$new_data_type != "None") {input$new_data_type},
            if (input$new_data_drug != "None") {input$new_data_drug},
            input$new_data_time,
            input$new_data_plot_time,
            if (input$new_data_time_unit != "None") {input$new_data_time_unit},
            input$new_data_aval,
            input$new_data_lower, input$new_data_upper, 
            if (input$new_data_unit != "None") {input$new_data_unit},
            input$new_data_anelfl,
            input$new_data_tailfl,
            if (input$new_data_comment != "None") {input$new_data_comment},
            if (input$new_data_highlight != "None") {input$new_data_highlight},
            if (!is.null(input$new_data_other) && input$new_data_other != "") {
              input$new_data_other})
  
  ds <- ds[, cols]
  
  colnames(ds) <- c("profid", 
                    if (input$new_data_type != "None") {"type"},
                    if (input$new_data_drug != "None") {"drug"},
                    "time",
                    "plot_time",
                    if (input$new_data_time_unit != "None") {"time_unit"},
                    "aval",
                    "lower_conc", "upper_conc", 
                    if (input$new_data_unit != "None") {"conc_unit"},
                    "anelfl","tailfl", 
                    if (input$new_data_comment != "None") {"lab_comment"},
                    if (input$new_data_highlight != "None") {"highlight"},
                    if (!is.null(input$new_data_other) && input$new_data_other != "") {
                      input$new_data_other})
  
  ds
})


output$uploaded_data <- renderDataTable({
  req(new_data_subsetted())
  
  new_name <- paste0(input$new_data_profid, collapse = "/")
  
  ds <- new_data_subsetted()
  
  colnames(ds) <- c(new_name, 
            if (input$new_data_type != "None") {input$new_data_type},
            if (input$new_data_drug != "None") {input$new_data_drug},
            input$new_data_time,
            input$new_data_plot_time,
            if (input$new_data_time_unit != "None") {input$new_data_time_unit},
            input$new_data_aval,
            input$new_data_lower, input$new_data_upper, 
            if (input$new_data_unit != "None") {input$new_data_unit},
            input$new_data_anelfl,
            input$new_data_tailfl,
            if (input$new_data_comment != "None") {input$new_data_comment},
            if (input$new_data_highlight != "None") {input$new_data_highlight},
            if (!is.null(input$new_data_other) && input$new_data_other != "") {
              input$new_data_other})
  
  ds
})



observeEvent(input$new_data_save, {
  
  review_access_all <- GlobalRV$review_access
  review_access <- review_access_keep <- review_access_all[review_access_all$trial_id == setupRV$trial_select_id, ]
  
  app_data_dir <- review_access$app_data
  
  if (!dir.exists(file.path(app_data_dir, "data"))) {
    dir.create(file.path(app_data_dir, "data"))
  }
  
  dirs <- list.dirs(file.path(app_data_dir, "data"))
  
  # subset to actual dirs
  dirs <- dirs[grep("review_round_", dirs)]
  
  pk_dirs <- sapply(file.path(dirs, "app_adpc.rds"), file.exists)
  
  n.dirs <- ifelse(length(pk_dirs) && any(pk_dirs), max(which(pk_dirs)), 0)
  
  ava1 <- c(n.dirs, n.dirs + 1)
  
  choices <- structure(file.path(app_data_dir, "data", paste0("review_round_", ava1[ava1 > 0])),
                       names = paste("Round", ava1[ava1 > 0]))
  
  ui <- tagList(
    splitLayout(cellWidths = c("80%", "20%"),
                selectInput("new_data_R_round", "Select the review round to save the data for",
                            width = "90%",
                            choices = choices),
                div(br(),
                    actionButton("new_data_save_modal", "Save", width = "90%",
                                 style = paste0("color: white; background-color: ",
                                                "#3F9C35",";
                                       border-color: ","#3F9C35"))
                ))
  )
  showModal(modalDialog(title = "Save the pk data",  ui))
})



output$saved_data_path <- renderText({file.path(input$new_data_R_round, "app_adpc.rds")})

observeEvent(input$new_data_save_modal, {
  
  
  
  isolate({
    req(new_data_subsetted(), input$new_data_R_round)
    ds <- new_data_subsetted()
    
    print(dir(file.path(input$new_data_R_round), ".."))
    
    print(paste("create dir", file.path(input$new_data_R_round)))
    dir.create(file.path(input$new_data_R_round), showWarnings = FALSE, recursive = TRUE)
    
    if (input$new_data_comment == "None") {ds$lab_comment <- ""}
    
    print(paste("input$new_trial_trial", input$new_trial_trial))
    
    GlobalRV$PKdata[[setupRV$trial_select_id]] <<- ds
    saveRDS(ds, file.path(input$new_data_R_round, "app_adpc.rds"))
    
    # Force reset of datadir for new data
    setupRV$trial_pk_data_dir <- ""
    setupRV$trial_pk_data_dir <- input$new_data_R_round
    
    # change view to pk plot
    updateTabItems(session, "sideMenu", selected = "plot_profile")
    
    setupRV$trial_select <- input$new_trial_trial
    
    removeModal()
    file.path(input$new_data_R_round, "app_adpc.rds")
  })
})

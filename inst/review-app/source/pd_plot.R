
#----------------------------------------#
######        auxilliary            ######
#----------------------------------------#

pdplotRV <- reactiveValues(new_profile = TRUE, change_from_input = TRUE)


shinyInput2 <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(id = paste0(id, i), ...))
  }
  inputs
}

#----------------------------------------#
######      Setup PD                ######
#----------------------------------------#

output$setup_pd_plot_ui <- renderUI({
  
  if (PKPDEnv("verbose")) cat("setup_pd_plot_ui\n")
  
  tagList(
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Setup plot panels",
      status = "primary",
      DT::dataTableOutput("setup_pd_panels_table"),
      actionButton("add_pd_panel", "Add panel",
                   style = paste0("color: white; background-color: ",
                                  "#001965",";
                                       border-color: ","#001965")),
      actionButton("add_pd_panel_cancel", "Revert to last save",
                   style = paste0("color: white; background-color: ",
                                  "#E64A0E",";
                                       border-color: ","#E64A0E")),
      actionButton("add_pd_panel_save", "Save",
                   style = paste0("color: white; background-color: ",
                                  "#3F9C35",";
                                       border-color: ","#3F9C35"))
    ),
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Setup each plot",
      status = "primary",
      DT::dataTableOutput("setup_pd_plots_table"),
      actionButton("add_pd_plot_cancel", "Revert to last save",
                   style = paste0("color: white; background-color: ",
                                  "#E64A0E",";
                                       border-color: ","#E64A0E")),
      actionButton("add_pd_plot_save", "Save",
                   style = paste0("color: white; background-color: ",
                                  "#3F9C35",";
                                       border-color: ","#3F9C35"))
    ),
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = "Setup color scheme",
      status = "primary",
      DT::dataTableOutput("setup_pd_color_scheme"),
      actionButton("add_pd_scheme_cancel", "Revert to last save",
                   style = paste0("color: white; background-color: ",
                                  "#E64A0E",";
                                       border-color: ","#E64A0E")),
      actionButton("add_pd_scheme_save", "Save",
                   style = paste0("color: white; background-color: ",
                                  "#3F9C35",";
                                       border-color: ","#3F9C35"))
    )
  )
})

#----------------------------------------#
######      Setup PD panels         ######
#----------------------------------------#


observeEvent(pddata(), {
  req(trial_info(), pddata())
  
  pdplotRV$panel_data_param <- panel_data_param <- pddata() %>% dplyr::distinct(paramcd, param)

  if (PKPDEnv("verbose")) cat("setup_pd_panel_data\n")
  
  inst_info <- trial_info()
  
  if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "panel_data.rds"))) {
    
    panel_data <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "panel_data.rds"))
    
    panel_data <- panel_data[, c("panel_name", "paramcd")] #%>%
      #dplyr::filter(panel_data_param$paramcd)
    
    panel_data$paramcd <-
      lapply(panel_data$paramcd,  function(x) {
        unlist(x)[unlist(x) %in% panel_data_param$paramcd]
      })
    
    panel_data$param      <- 
      lapply(panel_data$paramcd, function(x) {
        panel_data_param[match(unlist(x), panel_data_param$paramcd), ]$param
      })
    
    if (nrow(panel_data) && !"panel_id" %in% colnames(panel_data))
      panel_data$panel_id <- paste("panel", seq_len(nrow(panel_data)), sep = "_")

  } else {
    
    pddata_keep <- pddata()
    
    panel_data <- tibble::tibble(panel_name = character(0), paramcd = list(character(0)), param = list(character(0)),
                                 panel_id = character(0))
    
    panel_pos <- unique(pddata_keep$paramcd)
    
    gluc_lev <- c("GLUCOSE LEVEL", "GLUC_LEV", "BG_CLAMPART", "BGCLART", "GLUC")
    
    if (any(gluc_lev %in% panel_pos)) {
      
      gluc_lev <- gluc_lev[gluc_lev %in% panel_pos]
      
      param <- pddata_keep[match(gluc_lev, pddata_keep$paramcd), ]$param
      
      panel_data <- dplyr::bind_rows(panel_data,
                                     tibble::tibble(panel_name = "Glucose Level", paramcd = list(gluc_lev), 
                                                    param = list(param)))
    }
    
    gluc_inf <- c("GLUCOSE INFUSIO RATE", "GLUINFRT", "SMOOTH_GIR", "SM_GIR")
    
    if (any(gluc_inf %in% panel_pos)) {
      
      gluc_inf <- gluc_inf[gluc_inf %in% panel_pos]
      
      param <- pddata_keep[match(gluc_inf, pddata_keep$paramcd), ]$param
      
      panel_data <- 
        dplyr::bind_rows(panel_data,
                         tibble::tibble(panel_name = "Glucose Infusion Rate", paramcd = list(gluc_inf), 
                                        param = list(param)))
    }
    
    ins_inf <- c("INSULIN INFUSIO RATE", "INSINFRT")
    
    if (any(ins_inf %in% panel_pos)) {
      
      ins_inf <- ins_inf[ins_inf %in% panel_pos]
      param <- pddata_keep[match(ins_inf, pddata_keep$paramcd), ]$param
      panel_data <- 
        dplyr::bind_rows(panel_data,
                         tibble::tibble(panel_name = "Insulin Infusion Rate", paramcd = list(ins_inf), 
                                        param = list(param)))
    }
    if (nrow(panel_data))
      panel_data$panel_id <- paste("panel", seq_len(nrow(panel_data)), sep = "_")
  }
  pdplotRV$panel_data <- panel_data
})


observeEvent(input$setup_pd_panels_table_rows_all, {
  pdplotRV$panel_data <- pdplotRV$panel_data[input$setup_pd_panels_table_rows_all, ]
})

setup_pd_panel_data <- reactive({
  req(pddata(), pdplotRV$panel_data)
  req(nrow(pdplotRV$panel_data))
  pdplotRV$panel_data
})



output$setup_pd_panels_table <- DT::renderDataTable({
  req(setup_pd_panel_data())
  
  if (PKPDEnv("verbose")) cat("setup_pd_panels_table\n")

  panel_data <- setup_pd_panel_data() #%>% 
  panel_data <- panel_data[, setdiff(colnames(panel_data), "panel_id")]
  
  Edit <-
    shinyInput2(tags$button, nrow(panel_data), "pd_panel_data_edit_button_",
                type    = "button",
                class   = "btn btn-primary action-button",
                onclick = 'Shiny.onInputChange("pd_panel_data_edit_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-edit"), "")
  
  Delete <-
    shinyInput2(tags$button, nrow(panel_data), "pd_panel_data_delete_button_",
                type    = "button",
                class   = "btn btn-danger action-button",
                onclick = 'Shiny.onInputChange("pd_panel_data_delete_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-trash"), "")
  
  comb <- cbind(panel_data,
                data.frame(Edit = Edit), 
                data.frame(Delete = Delete))
  
  if ("param" %in% colnames(comb)) {
    if (any(sapply(comb$param, length))) {
      comb$param <- comb$param %>% sapply(paste, collapse = ", ")
    } else {
      if (nrow(comb))
        comb$param <- ""
    }
  }
  
  if ("paramcd" %in% colnames(comb)) {
    if (any(sapply(comb$paramcd, length))) {
      comb$paramcd <- comb$paramcd %>% sapply(paste, collapse = ", ")
    } else {
      if (nrow(comb))
        comb$paramcd <- ""
    }
  }
  
  return(comb)
}, colnames = c(ID = 1),
server = FALSE, escape = FALSE, selection = 'none',
extensions = 'RowReorder',
options = list(order = list(c(0, 'asc')),
               rowReorder = TRUE,
               preDrawCallback = DT::JS('function() {
                       Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() {
                    Shiny.bindAll(this.api().table().node()); } ')
))

observeEvent(input$add_pd_panel, {

  pdplotRV$panel_data_param <- panel_data_param <- pddata() %>% dplyr::distinct(paramcd, param)
  
  ui <- tagList(
    h2("Adding panel:"),
    textInput("pd_panel_data_new_panel_name", label = "The panel name:", ""),
    
   
    selectInput(inputId = "pd_panel_data_new_panel_paramcd", "The parameters to be included in the panel",
                choices = structure(panel_data_param$paramcd, names = panel_data_param$param),
                multiple = TRUE)
  )
  
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("save_new_pd_panel", "Add panel", icon = icon("check"),
                 style = paste0("color: white; background-color: ",
                                "#3F9C35","; border-color: ","#3F9C35"))
  )
  
  showModal(modalDialog(ui, title = "New panel", footer = footer))
})





observeEvent(input$save_new_pd_panel, {

  
  if (PKPDEnv("verbose")) cat("save_new_pd_panel\n")
  
  withProgress(message = "Adding panel", value = 0, {
    panel_data <- setup_pd_panel_data()
    panel_data_param <- pdplotRV$panel_data_param 
    
    if (nrow(panel_data)) {
      id_number <- max(as.numeric(strsplit(panel_data$panel_id, "_")[[1]][2])) + 1
    } else { 
      id_number = 1
    }
    new <- tibble::tibble(
      panel_name = input$pd_panel_data_new_panel_name,
      paramcd    = list(input$pd_panel_data_new_panel_paramcd),
      param      = list(panel_data_param[match(input$pd_panel_data_new_panel_paramcd, 
                                               panel_data_param$paramcd), ]$param),
      panel_id   = paste("panel", id_number, sep = "_"))
    
    pdplotRV$panel_data <- dplyr::bind_rows(panel_data, new) 
    
    incProgress(1/1, "Adding", "Done")
  })
removeModal()

if (PKPDEnv("verbose")) cat("save_new_pd_panel - done\n")

})


observeEvent(input$pd_panel_data_delete_button, {
  
  pdplotRV$panel_data_row <- as.numeric(strsplit(input$pd_panel_data_delete_button, "_")[[1]][6])
  
  panel_data_row <- setup_pd_panel_data()[pdplotRV$panel_data_row, ]
  
  pdplotRV$panel_data_param <- pddata <- pddata() %>% dplyr::distinct(paramcd, param)
  
  ui <- tagList(
    h2("Deleting panel:"),
    
    h3(panel_data_row$panel_name)
  )
  
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("delete_pd_panel", "Delete", icon("remove"),
                 style = paste0("color: white; background-color: ",
                                "#E64A0E",";
                                       border-color: ","#E64A0E"))
  )
  
  showModal(modalDialog(ui, title = "Delete panel", footer = footer))
  
  
})



observeEvent(input$delete_pd_panel, {
  
  withProgress(message = "Deleting panel", value = 0, {
    panel_data <- setup_pd_panel_data()
    
    pdplotRV$panel_data <- panel_data[-pdplotRV$panel_data_row, ]
    
    incProgress(1/1, "Deleting", "Done")
  })
  
  removeModal()
})

observeEvent(input$pd_panel_data_edit_button, {
  
  pdplotRV$panel_data_row <- as.numeric(strsplit(input$pd_panel_data_edit_button, "_")[[1]][6])
  
  panel_data_row <- setup_pd_panel_data()[pdplotRV$panel_data_row, ]
  
  pdplotRV$panel_data_param <- pddata <- pddata() %>% dplyr::distinct(paramcd, param) %>% 
    dplyr::filter(!(is.na(paramcd) | is.na(param)))
  
  ui <- tagList(
    h2("Editing panel:"),
    textInput("pd_panel_data_edit_panel_name", label = "The panel name:", panel_data_row$panel_name),
    
    selectInput(inputId = "pd_panel_data_edit_panel_paramcd", "The parameters to be included in the panel",
                  choices = structure(pddata$paramcd, names = pddata$param),
                  selected = unlist(panel_data_row$paramcd), width = "100%", multiple = TRUE)
  )
  
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("save_pd_panel", "Update", icon = icon("check"),
                 style = paste0("color: white; background-color: ",
                                "#3F9C35","; border-color: ","#3F9C35"))
  )
  
  showModal(modalDialog(ui, title = "Edit panel", footer = footer))
})


observeEvent(input$save_pd_panel, {
  withProgress(message = "Updating panel", value = 0, {
    panel_data <- setup_pd_panel_data()
    panel_data_param <- pdplotRV$panel_data_param 
    
    panel_data[pdplotRV$panel_data_row, "panel_name"] <- input$pd_panel_data_edit_panel_name
    panel_data[pdplotRV$panel_data_row, ]$paramcd    <- list(input$pd_panel_data_edit_panel_paramcd)
    panel_data[pdplotRV$panel_data_row, ]$param      <- 
      list(panel_data_param[match(input$pd_panel_data_edit_panel_paramcd, panel_data_param$paramcd), ]$param)
    
    pdplotRV$panel_data <- panel_data
    
    incProgress(1/1, "Updating", "Done")
  })
  
  removeModal()
})


observeEvent(input$add_pd_panel_cancel, {

  if (PKPDEnv("verbose")) cat("add_pd_panel_cancel\n")
  
  withProgress(message = "Reverting panels", value = 0, {
    pdplotRV$panel_data_param <- panel_data_param <- pddata() %>% dplyr::distinct(paramcd, param)
    
    if (PKPDEnv("verbose")) cat("add_pd_panel_cancel\n")
    
    inst_info <- trial_info()
    
    if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "panel_data.rds"))) {
      
      panel_data <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "panel_data.rds"))
      
      panel_data <- panel_data[, c("panel_name", "paramcd")]
      
      panel_data$param      <- 
        lapply(panel_data$paramcd, function(x) {
          panel_data_param[match(unlist(x), panel_data_param$paramcd), ]$param
        })
      
      pdplotRV$panel_data <- panel_data
    } else {
      showModal(h1("Nothing available"))
    }
    
    incProgress(1/1, "Reverting", "Done")
  })
  
  if (PKPDEnv("verbose")) cat("add_pd_panel_cancel - done\n")
})



observeEvent(input$add_pd_panel_save, {
  req(trial_info())
  
  withProgress(message = "Saving panels", value = 0, {
    if (PKPDEnv("verbose")) cat("add_pd_panel_save\n")
    
    inst_info <- trial_info()
    
    dir.create(file.path(inst_info$trial_app_dir, "pd_config"),
               showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(setup_pd_panel_data()[, c("panel_name", "paramcd")], 
            file.path(inst_info$trial_app_dir, "pd_config", "panel_data.rds"))
    
    incProgress(1/1, "Saving", "Done")
  })
  
})

#----------------------------------------#
######      Setup PD plots          ######
#----------------------------------------#

preDefinedPlots <- function(plot_data) {
  
  plot_data$symbol <- character(nrow(plot_data))
  plot_data$line <- character(nrow(plot_data))
  
  points <- c("BG_CLAMPART", "BGCLART", "GLUC")
  line   <- c("GLUCOSE LEVEL", "GLUC_LEV", "GLUINFRT",
              "SMOOTH_GIR", "SM_GIR")
  step   <- c("INSULIN INFUSIO RATE", "INSINFRT")
  
  if (any(plot_data$paramcd %in%  points))
    plot_data[plot_data$paramcd %in%  points, "symbol"] <- "point"
  if (any(plot_data$paramcd %in%  line))
    plot_data[plot_data$paramcd %in%  line, "line"] <- "line"
  if (any(plot_data$paramcd %in%  step))
    plot_data[plot_data$paramcd %in%  step, "line"] <- "step"
  
  return(plot_data)
}


observeEvent(setup_pd_panel_data(), {
  req(setup_pd_panel_data(), trial_info())
  
  if (PKPDEnv("verbose")) cat("setup_pd_plots_data\n")
  
  
  panel_data <- setup_pd_panel_data()
  
  # Get the plot_data
  if (is.null(pdplotRV$plot_data)) {
  
    inst_info <-  trial_info()
    
    if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "panel_plots.rds"))) {
      
      plot_data <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "panel_plots.rds"))
  
      if (!"panel_id" %in% colnames(plot_data))
        plot_data <- preDefinedPlots(tidyr::unnest(panel_data, paramcd, param))
      
    } else {
      
      plot_data <- preDefinedPlots(tidyr::unnest(panel_data, paramcd, param))
    }
  } else {
    plot_data <- pdplotRV$plot_data
  }
  
  plot_data_old <- plot_data
  
  plot_data_ch <- tidyr::unnest(panel_data, paramcd, param)
  
  if (class(plot_data_ch$paramcd) == "vctrs_unspecified") {
    plot_data_ch$paramcd <- as.character(plot_data_ch$paramcd)
    plot_data_ch$param <- as.character(plot_data_ch$param)
  }
  
  plot_data_keep <- plot_data_old[paste(plot_data_old$panel_id, plot_data_old$paramcd) %in%
                                  paste(plot_data_ch$panel_id, plot_data_ch$paramcd), ]
  
  plot_data_keep$panel_name <- 
    plot_data_ch[match(plot_data_keep$panel_id, plot_data_ch$panel_id), ]$panel_name
  
  plot_data_new <- plot_data_ch[!(paste(plot_data_ch$panel_id, plot_data_ch$paramcd) %in%
                                  paste(plot_data_old$panel_id, plot_data_old$paramcd)), ]

  plot_data <- dplyr::bind_rows(plot_data_keep, preDefinedPlots(plot_data_new))
  
  # Save to reactive value in new order
  pdplotRV$plot_data <- plot_data[order(match(plot_data$panel_name, panel_data$panel_name)), ]
})

observeEvent(input$setup_pd_plots_table_rows_all, {
  
  if (PKPDEnv("verbose")) cat("setup_pd_plots_table_rows_all\n")
  
  plot_data <- pdplotRV$plot_data[input$setup_pd_plots_table_rows_all, ]
  
  panel_data <- setup_pd_panel_data()
  pdplotRV$plot_data <- NULL
  pdplotRV$plot_data <- plot_data[order(match(plot_data$panel_name, panel_data$panel_name)), ]
})

setup_pd_plots_data <- reactive({
  req(setup_pd_panel_data())
  validate(need(nrow(setup_pd_panel_data()), "No panels have been set up yet. Go to Setup trial/Setup PD plot"))
  pdplotRV$plot_data
})


output$setup_pd_plots_table <- DT::renderDataTable({
  req(setup_pd_plots_data())
  if (PKPDEnv("verbose")) cat("setup_pd_plots_table\n")
  
  
  plot_data <- setup_pd_plots_data() 
  plot_data <- plot_data[, setdiff(colnames(plot_data), "panel_id")]
  
  Edit <-
    shinyInput2(tags$button, nrow(plot_data), "pd_plot_data_edit_button_",
                type    = "button",
                class   = "btn btn-primary action-button",
                onclick = 'Shiny.onInputChange("pd_plot_data_edit_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-edit"), "")
  
  comb <- cbind(plot_data,
                data.frame(Edit = Edit))
  
  return(comb)
}, colnames = c(ID = 1),
server = FALSE, escape = FALSE, selection = 'none',
extensions = 'RowReorder',
options = list(order = list(c(0, 'asc')),
               rowReorder = TRUE,
               preDrawCallback = DT::JS('function() {
                       Shiny.unbindAll(this.api().table().node()); }'),
               drawCallback = DT::JS('function() {
                    Shiny.bindAll(this.api().table().node()); } ')
))



observeEvent(input$pd_plot_data_edit_button, {
  
  pdplotRV$plot_data_row <- as.numeric(strsplit(input$pd_plot_data_edit_button, "_")[[1]][6])
  
  plot_data_row <- setup_pd_plots_data()[pdplotRV$plot_data_row, ]
  
  data <- pddata() %>% dplyr::filter(paramcd == plot_data_row$paramcd)
  
  line_choice <- c("None", "line", "step", "h-line", "v-line")
  
  if (all(c("upper", "lower") %in% colnames(data)) && any(!is.na(data$upper) & !is.na(data$lower))) {
    line_choice <-  c(line_choice, "region") 
  } 
  
   ui <- tagList(
    h2(glue::glue("Editing plot for: {plot_data_row$param}")),
      selectInput("pd_plot_data_edit_symbol", "Select symbol", choices = c("None", "point"),
                  selected = plot_data_row$symbol),
      selectInput("pd_plot_data_edit_line", "Select line type", choices = line_choice,
                selected = plot_data_row$line)
    )
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("save_pd_plot", "Update", icon = icon("check"),
                 style = paste0("color: white; background-color: ",
                                "#3F9C35","; border-color: ","#3F9C35"))
  )

  showModal(modalDialog(ui, title = "Edit plot", footer = footer))
})


observeEvent(input$save_pd_plot, {
  withProgress(message = "Updating plot", value = 0, {
    plot_data <- setup_pd_plots_data()
    plot_data_row <- pdplotRV$plot_data_row 
    what <- c(point = "point", line = "line", step = "step", "h-line" = "h-line",
              "v-line" = "v-line", region = "region", None = "")
    plot_data[plot_data_row, "symbol"] <- what[input$pd_plot_data_edit_symbol]
    plot_data[plot_data_row, "line"]   <- what[input$pd_plot_data_edit_line]
    
    pdplotRV$plot_data <- plot_data
    
    incProgress(1/1, "Updating", "Done")
  })
  
  removeModal()
})

observeEvent(input$add_pd_plot_cancel, {
  req(setup_pd_panel_data(), trial_info())
  withProgress(message = "Reverting each plot", value = 0, {
    if (PKPDEnv("verbose")) cat("add_pd_plot_cancel\n")
    
    panel_data <- setup_pd_panel_data()
    
    inst_info <- trial_info()
    
    if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "panel_plots.rds"))) {
      
      plot_data <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "panel_plots.rds"))
      
      plot_data_old <-  plot_data
      
      plot_data_ch <- tidyr::unnest(panel_data, paramcd, param)
      
      if (class(plot_data_ch$paramcd) == "vctrs_unspecified") {
        plot_data_ch$paramcd <- as.character(plot_data_ch$paramcd)
        plot_data_ch$param <- as.character(plot_data_ch$param)
      }
      
      plot_data_keep <- plot_data_old[paste(plot_data_old$panel_id, plot_data_old$paramcd) %in%
                                        paste(plot_data_ch$panel_id, plot_data_ch$paramcd), ]
      
      plot_data_keep$panel_name <- 
        plot_data_ch[match(plot_data_keep$panel_id, plot_data_ch$panel_id), ]$panel_name
      
      plot_data_new <- plot_data_ch[!(paste(plot_data_ch$panel_id, plot_data_ch$paramcd) %in%
                                        paste(plot_data_old$panel_id, plot_data_old$paramcd)), ]
      
      plot_data <- dplyr::bind_rows(plot_data_keep, preDefinedPlots(plot_data_new))
      
      # Save to reactive value in new order
      pdplotRV$plot_data <- plot_data[order(match(plot_data$panel_name, panel_data$panel_name)), ]
      
    } else {
      showModal(h1("Nothing available"))
    }
    
    incProgress(1/1, "Reverting", "Done")
  })
  
})

observeEvent(input$add_pd_plot_save, {
  req(trial_info())
  withProgress(message = "Saving plots", value = 0, {
    if (PKPDEnv("verbose")) cat("add_pd_plot_save\n")
    
    inst_info <- trial_info()
    
    dir.create(file.path(inst_info$trial_app_dir, "pd_config"),
               showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(setup_pd_plots_data(), 
            file.path(inst_info$trial_app_dir, "pd_config", "panel_plots.rds"))
    
    incProgress(1/1, "Saving", "Done")
  })
})


#----------------------------------------#
#### Setup color scheme for PD plots  ####
#----------------------------------------#

observeEvent(setup_pd_plots_data(), {
  
  if (PKPDEnv("verbose")) cat("pd_col_scheme\n")
  
  req(nrow(setup_pd_plots_data()))
  
  # Get the plot_data
  if (is.null(pdplotRV$col_scheme)) {
    
    inst_info <-  trial_info()
    
    if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "col_scheme.rds"))) {
      
      col_scheme <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "col_scheme.rds"))
      
      # if (!"fill" %in% colnames(col_scheme)) {
      #   col_scheme$fill <- NA
      # }
      # 
      # if (!"alpha" %in% colnames(col_scheme)) {
      #   col_scheme$alpha <- 1
      # }
      # 
      # if (!"show_legend" %in% colnames(col_scheme)) {
      #   col_scheme$show_legend <- TRUE
      # }
      
    } else {
      pddata <- pddata()
      plot_data <- setup_pd_plots_data()
      
      
      if (nrow(plot_data) == 0) {
        pdplotRV$col_scheme <- NULL
        return(NULL)
      }
      
      
      col_cs <- pddata[,  intersect(c("param", "paramcd", "color_by"), colnames(pddata))] %>% 
        dplyr::distinct() %>% dplyr::left_join(plot_data[, c("paramcd", "symbol", "line")], by = "paramcd")
      
      col_cs <- col_cs[!is.na(col_cs$symbol) | !is.na(col_cs$line), ]
      
      # col_cs[!is.na(col_cs$symbol) & col_cs$symbol != "", "shape"] <- 19 
      # col_cs[!is.na(col_cs$symbol) & col_cs$symbol != "", "size"]  <-  2
      # 
      # col_cs[!is.na(col_cs$line) & col_cs$line != "region" & col_cs$line != "",   "width"] <-  1
      # col_cs[!is.na(col_cs$line) & col_cs$line != "region" & col_cs$line != "",   "lty"]   <- "solid"
 
      # col_cs$alpha <- 1
      # col_cs$show_legend <- TRUE
            
      if ("color_by" %in% colnames(pddata)) {
        n_col <- length(unique(pddata$color_by))
        
        col_data <- tibble::tibble(color_by = unique(pddata$color_by), color =  nncol$company[-1][seq_len(n_col)])
        
        col_cs$color <- col_data[match(col_cs$color_by, col_data$color_by), ]$color
        
        # col_cs[ (!is.na(col_cs$symbol) & col_cs$symbol != "") | col_cs$line == "region",   "fill"] <- 
        #   col_cs[ (!is.na(col_cs$symbol) & col_cs$symbol != "") | col_cs$line == "region",   "color"]
        # col_cs[!is.na(col_cs$line) & col_cs$line != "region",   "fill"] <- 
        #   col_data[match(col_cs$color_by, col_data$color_by), ]$color

        col_cs$label <- paste(col_cs$param, col_cs$color_by, sep = " - ")
        
      } else {
        col_cs$color <- nncol$company[-1][unlist(lapply(table(plot_data$panel_name), seq_len))]
        
        # col_cs[ (!is.na(col_cs$symbol) & col_cs$symbol != "") | col_cs$line == "region",   "fill"] <- 
        #   col_cs[ (!is.na(col_cs$symbol) & col_cs$symbol != "") | col_cs$line == "region",   "color"]
        # col_cs[!is.na(col_cs$line) & col_cs$line != "region",   "fill"] <- 
        #   col_cs[!is.na(col_cs$line) & col_cs$line != "region",   "color"]
        
        col_cs$label <- col_cs$param
      }
      
      col_scheme <- 
        col_cs[, intersect(c("param", "paramcd", "color_by", "label", "color", "fill", "alpha", "symbol", 
                             "shape","size", "line", "lty", "width", "show_legend"), 
                           colnames(col_cs))]
      
    }
  } else {
    col_scheme <- pdplotRV$col_scheme
  }
  
  pddata <- pddata()
  plot_data <- setup_pd_plots_data()

  ground_col <- intersect(c("paramcd", "color_by"), colnames(pddata))
  
  
  # Create new base
  col_cs_base <- pddata[pddata$paramcd %in% plot_data$paramcd,  c("param", ground_col)] %>% 
    dplyr::distinct() %>% dplyr::left_join(plot_data[, c("paramcd", "symbol", "line")], by = "paramcd")
  
  col_cs_base <- col_cs_base[!is.na(col_cs_base$symbol) | !is.na(col_cs_base$line), ]
  
  # Add old col_cheme
  col_scheme <- col_cs_base %>% dplyr::left_join(col_scheme[, setdiff(colnames(col_scheme), c("param", "symbol", "line"))],
                                           by = ground_col)
  
  # add missing symbol and lines
  if (!"shape" %in% colnames(col_scheme))
    col_scheme$shape <- NaN
  
  if (!"size" %in% colnames(col_scheme))
    col_scheme$size <- NaN
  
  if (!"fill" %in% colnames(col_scheme))
    col_scheme$fill <- NA
  
  if (!"width" %in% colnames(col_scheme))
    col_scheme$width <- NaN
  
  if (!"lty" %in% colnames(col_scheme)) {
    col_scheme$lty <- NA
    col_scheme$lty <- as.character(col_scheme$lty)
  }
  
  if (!"alpha" %in% colnames(col_scheme))
    col_scheme$alpha <- NaN
  
  if (!"show_legend" %in% colnames(col_scheme))
    col_scheme$show_legend <- NA
  
  col_scheme[!is.na(col_scheme$symbol) & col_scheme$symbol != "" & is.na(col_scheme$shape), "shape"] <- 19 
  col_scheme[!is.na(col_scheme$symbol) & col_scheme$symbol != "" & is.na(col_scheme$size),  "size"]  <-  2
  col_scheme[!is.na(col_scheme$line) & col_scheme$line != "region" & col_scheme$line != ""   & is.na(col_scheme$width), "width"] <-  1
  col_scheme[!is.na(col_scheme$line) & col_scheme$line != "region" & col_scheme$line != ""   & is.na(col_scheme$lty),   "lty"]   <- "solid"
  
  col_scheme[is.na(col_scheme$alpha), "alpha"] <- 1
  col_scheme[is.na(col_scheme$show_legend), "show_legend"] <- TRUE
  
  # remove values for deleted items
  col_scheme[is.na(col_scheme$symbol) | col_scheme$symbol == "", "shape"] <- NaN 
  col_scheme[is.na(col_scheme$symbol) | col_scheme$symbol == "", "size"]  <- NaN
  col_scheme[ (is.na(col_scheme$symbol) | col_scheme$symbol == "") & col_scheme$line != "region",   "fill"] <- NA
  col_scheme[is.na(col_scheme$line) | col_scheme$line == "" | col_scheme$line == "region",   "width"] <- NaN
  col_scheme[is.na(col_scheme$line) | col_scheme$line == "" | col_scheme$line == "region",   "lty"]   <- NA

  # add missing colours
  
  if ("color_by" %in% colnames(pddata)) {
    n_col <- length(unique(pddata$color_by))
    
    col_data <- tibble::tibble(color_by = unique(pddata$color_by), color =  nncol$company[-1][seq_len(n_col)])
    
    col_scheme[is.na(col_scheme$color), ]$color <- col_data[match(col_scheme[is.na(col_scheme$color), ]$color_by, 
                                                          col_data$color_by), ]$color
    
    col_scheme[is.na(col_scheme$label), ]$label <- paste(col_scheme[is.na(col_scheme$label), ]$param, 
                                                 col_scheme[is.na(col_scheme$label), ]$color_by, sep = " - ")
    
    col_scheme[ ((!is.na(col_scheme$symbol) & col_scheme$symbol != "") | col_scheme$line == "region") & is.na(col_scheme$fill),   "fill"] <- 
      col_scheme[ ((!is.na(col_scheme$symbol) & col_scheme$symbol != "") | col_scheme$line == "region") & is.na(col_scheme$fill),   "color"]
    
  } else {
    if (length(col_scheme[is.na(col_scheme$color), ]$color) > 0)
      col_scheme[is.na(col_scheme$color), ]$color <- sample(nncol$company[-1], 1)
    
    col_scheme[is.na(col_scheme$label), ]$label <- col_scheme[is.na(col_scheme$label), ]$param
    
    col_scheme[ ((!is.na(col_scheme$symbol) & col_scheme$symbol != "") | col_scheme$line == "region") & is.na(col_scheme$fill),   "fill"] <- 
      col_scheme[ ((!is.na(col_scheme$symbol) & col_scheme$symbol != "") | col_scheme$line == "region") & is.na(col_scheme$fill),   "color"]
  }
 
  pdplotRV$col_scheme <- col_scheme[order(match(col_scheme$paramcd, plot_data$paramcd)), intersect(c("param", "paramcd", "color_by", "label", "color", "fill", "alpha", "symbol", 
                                                                                                     "shape","size", "line", "lty", "width", "show_legend"), 
                                                                                                   colnames(col_scheme))]
})


pd_col_scheme <- reactive({
  req(setup_pd_plots_data())
  pdplotRV$col_scheme
})

output$setup_pd_color_scheme <- DT::renderDataTable({
  req(pd_col_scheme())
  if (PKPDEnv("verbose")) cat("setup_pd_color_scheme\n")

  col_scheme <- pd_col_scheme()
  
  Edit <-
    shinyInput2(tags$button, nrow(col_scheme), "pd_plot_col_edit_button_",
                type    = "button",
                class   = "btn btn-primary action-button",
                onclick = 'Shiny.onInputChange("pd_plot_col_edit_button",  this.id, {priority: \"event\"})',
                tags$span(class = "glyphicon glyphicon-edit"), "")
  
  comb <- cbind(col_scheme,
                data.frame(Edit = Edit))
  
  return(comb)
}, 
server = FALSE, escape = FALSE, selection = 'none',
options = list(
  preDrawCallback = DT::JS('function() {
                       Shiny.unbindAll(this.api().table().node()); }'),
  drawCallback = DT::JS('function() {
                    Shiny.bindAll(this.api().table().node()); } ')
))


observeEvent(input$pd_plot_col_edit_button, {
  
  pdplotRV$plot_col_row <- as.numeric(strsplit(input$pd_plot_col_edit_button, "_")[[1]][6])
  
  plot_col_row <- pd_col_scheme()[pdplotRV$plot_col_row, ]
  
  
  
  ui <- tagList(
    h2(glue::glue("Editing color scheme for: {plot_col_row$param}")),
    
    
    # color scheme
    shinyWidgets::prettyRadioButtons("pd_show_NN_col_theme", "Colour scheme", inline = TRUE,
                                     choices = c("All", "NN theme"), selected = "NN theme"),
    # 
    if (!(plot_col_row$line == "region" & plot_col_row$symbol == "")) {
      uiOutput("pd_plot_col_edit_color_ui")
    },
    
    # fill color
    if (plot_col_row$symbol != "" | plot_col_row$line == "region") {
      uiOutput("pd_plot_col_edit_fill_ui")
    },
    # shape
    if (plot_col_row$symbol != "") {
      selectInput("pd_plot_data_edit_shape", "Select shape", selected = plot_col_row$shape,
                  choices = structure(0:25, names = c("0: square",
                                                      "1: circle",
                                                      "2: triangle point up",
                                                      "3: plus",
                                                      "4: cross",
                                                      "5: diamond",
                                                      "6: triangle point down",
                                                      "7: square cross",
                                                      "8: star",
                                                      "9: diamond plus",
                                                      "10: circle plus",
                                                      "11: triangles up and down",
                                                      "12: square plus",
                                                      "13: circle crosn",
                                                      "14: square and triangle down",
                                                      "15: filled square",
                                                      "16: filled circle",
                                                      "17: filled triangle point-up",
                                                      "18: filled diamond",
                                                      "19: solid circle",
                                                      "20: bullet (smaller circle)",
                                                      "21: filled circle (fill)",
                                                      "22: filled square (fill)",
                                                      "23: filled diamond (fill)",
                                                      "24: filled triangle point-up (fill)",
                                                      "25: filled triangle point down (fill)")))
    },
    # 
    # size
    if (plot_col_row$symbol != "") {
      numericInput("pd_plot_data_edit_size", "Select size", value = plot_col_row$size)
    },
    
    # lty
    if (plot_col_row$line != "" & plot_col_row$line != "region") {
      selectInput("pd_plot_data_edit_lty", "Select line type", selected = plot_col_row$lty,
                  choices = c("blank", "solid", "dashed", 
                              "dotted", "dotdash", "longdash", "twodash"))
    },
    # width
    if (plot_col_row$line != "" & plot_col_row$line != "region") {
      numericInput("pd_plot_data_edit_width", "Select line width", value = plot_col_row$width)
    }, 
    

    numericInput("pd_plot_data_edit_alpha", "Select alpha", value = plot_col_row$alpha, min = 0, max = 1),
    
    
    # label
    textInput("pd_plot_data_edit_label", "Write the label", value = plot_col_row$label),
    
    # Show legend in plot
    "Show legend in plot : ",
    shinyWidgets::prettyCheckbox(inputId = "pd_plot_data_edit_show_legend",
                                 label = "Yes",
                                 value = plot_col_row$show_legend, inline = TRUE)

  )
  
  footer = tagList(
    modalButton("Cancel", icon("remove")),
    actionButton("save_colscheme_plot", "Update", icon = icon("check"),
                 style = paste0("color: white; background-color: ",
                                "#3F9C35","; border-color: ","#3F9C35"))
  )
  
  showModal(modalDialog(ui, title = "Edit colour scheme", footer = footer))
})


output$pd_plot_col_edit_color_ui <- renderUI({
  
  pdplotRV$plot_col_row <- as.numeric(strsplit(input$pd_plot_col_edit_button, "_")[[1]][6])
  
  plot_col_row <- pd_col_scheme()[pdplotRV$plot_col_row, ]
  
  if (input$pd_show_NN_col_theme == "All")
    out <- colourpicker::colourInput("pd_plot_col_edit_color", "Select colour:", value = plot_col_row$color)
  
  if (input$pd_show_NN_col_theme == "NN theme")
    out <- colourpicker::colourInput("pd_plot_col_edit_color", "Select colour:", value = plot_col_row$color,
                                     palette = "limited", allowedCols = nncol$company)
  
  out
})

output$pd_plot_col_edit_fill_ui <- renderUI({
  
  pdplotRV$plot_col_row <- as.numeric(strsplit(input$pd_plot_col_edit_button, "_")[[1]][6])
  
  plot_col_row <- pd_col_scheme()[pdplotRV$plot_col_row, ]
  
  if (input$pd_show_NN_col_theme == "All")
    out <- colourpicker::colourInput("pd_plot_col_edit_fill", "Select fill colour:", value = plot_col_row$fill)
  
  if (input$pd_show_NN_col_theme == "NN theme")
    out <- colourpicker::colourInput("pd_plot_col_edit_fill", "Select fill colour:", value = plot_col_row$fill,
                                     palette = "limited", allowedCols = nncol$company)
  
  out
})


observeEvent(input$save_colscheme_plot, {
  withProgress(message = "Updating color scheme", value = 0, {
    edit_row <- pdplotRV$plot_col_row
    
    plot_col_sheme <- pd_col_scheme()
    
    if (plot_col_sheme[edit_row, ]$line != "region"){
      plot_col_sheme[edit_row, "color"] <- input$pd_plot_col_edit_color
    }
    
    if (plot_col_sheme[edit_row, ]$symbol != "") {
      plot_col_sheme[edit_row, "shape"] <- as.numeric(input$pd_plot_data_edit_shape)
      plot_col_sheme[edit_row, "size"]  <- input$pd_plot_data_edit_size
    }
    
    if (plot_col_sheme[edit_row, ]$line != "" & plot_col_sheme[edit_row, ]$line != "region") {
      plot_col_sheme[edit_row, "lty"]    <- input$pd_plot_data_edit_lty
      plot_col_sheme[edit_row, "width"]  <- input$pd_plot_data_edit_width
    }
    
    if (plot_col_sheme[edit_row, ]$symbol != "" | plot_col_sheme[edit_row, ]$line == "region") {
      plot_col_sheme[edit_row, "fill"] <- input$pd_plot_col_edit_fill
    }
    
    plot_col_sheme[edit_row, "label"] <- input$pd_plot_data_edit_label
    plot_col_sheme[edit_row, "alpha"] <- input$pd_plot_data_edit_alpha
    plot_col_sheme[edit_row, "show_legend"] <- input$pd_plot_data_edit_show_legend
    pdplotRV$col_scheme <- plot_col_sheme
    
    incProgress(1/1, "Updating", "Done")
  })
  
  removeModal()
})

observeEvent(input$add_pd_scheme_cancel, {
  req(pd_col_scheme(), setup_pd_plots_data(), trial_info())
  withProgress(message = "Reverting color scheme", value = 0, {
    if (PKPDEnv("verbose")) cat("add_pd_scheme_cancel\n")
    
    plot_data <- setup_pd_plots_data()
    
    plot_col_scheme <- pd_col_scheme()
    
    inst_info <- trial_info()
    
    if (file.exists(file.path(inst_info$trial_app_dir, "pd_config", "col_scheme.rds"))) {
      
      plot_col_scheme_old <- readRDS(file.path(inst_info$trial_app_dir, "pd_config", "col_scheme.rds"))
      
      plot_col_scheme_keep <- plot_col_scheme_old[paste(plot_col_scheme_old$paramcd, plot_col_scheme_old$color_by, plot_col_scheme_old$symbol, plot_col_scheme_old$line) %in%
                                        paste(plot_col_scheme$paramcd, plot_col_scheme$color_by, plot_col_scheme$symbol, plot_col_scheme$line), ]
      
      plot_col_scheme_new <- plot_col_scheme[!(paste(plot_col_scheme$paramcd, plot_col_scheme$color_by, plot_col_scheme$symbol, plot_col_scheme$line) %in%
                                        paste(plot_col_scheme_old$paramcd, plot_col_scheme_old$color_by, plot_col_scheme_old$symbol, plot_col_scheme_old$line)), ]
      
      col_scheme <- dplyr::bind_rows(plot_col_scheme_keep, plot_col_scheme_new)
      
      # Save to reactive value in new order
      pdplotRV$col_scheme <- col_scheme[order(match(col_scheme$paramcd, plot_data$paramcd)), ]
      
    } else {
      showModal(h1("Nothing available"))
    }
    
    incProgress(1/1, "Reverting", "Done")
  })
  
})

observeEvent(input$add_pd_scheme_save, {
  req(trial_info())
  
  withProgress(message = "Saving color scheme", value = 0, {
    if (PKPDEnv("verbose")) cat("add_pd_scheme_save\n")
    
    inst_info <- trial_info()
    
    dir.create(file.path(inst_info$trial_app_dir, "pd_config"),
               showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(pd_col_scheme(), 
            file.path(inst_info$trial_app_dir, "pd_config", "col_scheme.rds"))
    
    incProgress(1/1, "Saving", "Done")
  })
})

#----------------------------------------#
######     plot_pd_profile_ui       ######
#----------------------------------------#

output$pd_profile_buttons <- renderUI({
  req(pddata())
  if (PKPDEnv("verbose")) cat("pd_profile_buttons\n")
  user_type <- isolate(setupRV$user_type)
  
  if (user_type != "Visitor") {
    out <- tags$div( class = "form-group shiny-input-container",
                     splitLayout(
                       actionButton("pd_add_comment", "Add comment", width = "90%", style = paste0("color: white; background-color: ","#009FDA","; border-color: ","#009FDA")),
                       actionButton("pd_profile_reviewed", "Profile reviewed", width = "90%", style = paste0("color: white; background-color: ","#3F9C35","; border-color: ","#3F9C35"))
                     )
    )
  } else {
    return(NULL)
  }
  return(out)
})

output$pd_profile_plot_box_name <- renderUI({
  paste("Pharmacodynamic profile for", input$pd_select_profile)
})

output$plot_pd_profile_ui <- renderUI({
  req(pddata())
  
  if (PKPDEnv("verbose")) cat("plot_pd_profile_ui\n")

  
  # plot_div <- function(ns = "") 
  #   
  #   div(
  #     style = "position:relative",
  #     plotOutput(
  #       paste0(ns, "pd_plot_profile") ,
  #       click = paste0(ns, "pd_plot_profile_click"),
  #       dblclick = paste0(ns,"pd_plot_profile_dblclick"),
  #       brush = brushOpts(id = paste0(ns, "pd_plot_profile_brush"), resetOnNew = TRUE),
  #       hover = hoverOpts(paste0(ns, "pd_plot_profile_hover"), delay = 100, delayType = "debounce"),
  #       height = "auto", width = "95%"
  #     ),
  #     uiOutput(paste0(ns, "pdplot_profile_hover_info"))
  #   )
  
  user_type <- isolate(setupRV$user_type)
  
  
  # If individual zoom is needed this needs to be different plots
  
  fluidRow(
    box(
      solidHeader = TRUE, collapsible = TRUE, width = 12,
      title = uiOutput("pd_profile_plot_box_name"),
      status = "primary",
      uiOutput("pd_actual_plot_ui"),
      uiOutput("pd_zoom_help"),
      uiOutput("pd_axes"),
      
      fluidRow(align = "center",
        
        splitLayout(cellWidths = c("50%", "50%"),
                    actionButton("pd_prev_period", "Previous time", width = "80%"),
                    actionButton("pd_next_period", "Next time", width = "80%")
        )
      )
    ),
    
    conditionalPanel(condition = "input.pd_commentstimeline == 0",
                     box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                         title = tagList(
                           "Profile comments - ",
                           "Show comments as timeline :",
                           shinyWidgets::prettyCheckbox(inputId = "pd_commentstimeline_table",
                                                        label = "Yes",
                                                        value = isTRUE(setupRV$pd_timeline), inline = TRUE)), status = "primary",
                         DT::dataTableOutput("pd_adpcCom_profile")
                     )
    ),
    conditionalPanel(condition = "input.pd_commentstimeline == 1",
                     box(solidHeader = TRUE, collapsible = TRUE, width = 6,
                         title = tagList(
                           "Profile comments - ",
                           "Show comments as timeline :",
                           shinyWidgets::prettyCheckbox(inputId = "pd_commentstimeline_timeline",
                                                        label = "Yes",
                                                        value = isTRUE(setupRV$pd_timeline), inline = TRUE)
                         ),
                         status = "primary",
                         uiOutput("pd_profile_comments_ui")
                     )
    ),
    box(solidHeader = TRUE, collapsible = TRUE, width = 6,
        title = "Plot options", status = "primary",
          uiOutput("pd_commentstimeline"), 
          shinyWidgets::prettyRadioButtons(inputId = "pd_select_times",
                                           label = "Select marked points by:",
                                           choices  =  c("Time interval", "Points"),
                                           selected = "Time interval", inline = TRUE)
        )
  )
})


output$pd_actual_plot_ui <- renderUI({

  req(pddata(), setup_pd_panel_data())

  if (PKPDEnv("verbose")) cat("pd_actual_plot_ui\n")
  individual_plots <- FALSE
  common_legend <- TRUE
  plot_div <- function(ns = "") 
    
   div(
    style = "position:relative",
    plotOutput(
      paste0(ns, "pd_plot_profile") ,
      click = paste0(ns, "pd_plot_profile_click"),
      dblclick = paste0(ns,"pd_plot_profile_dblclick"),
      brush = brushOpts(id = paste0(ns, "pd_plot_profile_brush"), resetOnNew = TRUE),
      hover = hoverOpts(paste0(ns, "pd_plot_profile_hover"), delay = 100, delayType = "debounce"),
      height = "auto", width = "95%"
    ),
    uiOutput(paste0(ns, "pdplot_profile_hover_info"))
  )
  
  
  if (individual_plots) {
    tagList(lapply(c(setup_pd_panel_data()$panel_name, if (common_legend) {"legend"}), plot_div))
  } else {
    plot_div()
  }
})


observeEvent(input$pd_select_times, {
  pdplotRV$highlighted <- NULL
})


observeEvent(input$pd_commentstimeline_timeline, {
  setupRV$pd_timeline <- input$pd_commentstimeline_timeline
})

observeEvent(input$pd_commentstimeline_table, {
  setupRV$pd_timeline <- input$pd_commentstimeline_table
})

observeEvent(input$pd_commentstimeline, {
  setupRV$pd_timeline <- input$pd_commentstimeline
})

output$pd_commentstimeline <- renderUI({
  tagList("Show comments as timeline :",
          shinyWidgets::prettyCheckbox(inputId = "pd_commentstimeline",
                                       label = "Yes",
                                       value = setupRV$pd_timeline, inline = TRUE))
  
})


#----------------------------------------#
######     Profile selector         ######
#----------------------------------------#


pd_profile_choices <- reactive({
  
  req(pd_all_profiles(), !is.null(pd_user_review()), !is.null(pd_comment_status()))

  # cat("test--------------------------------------------------------------test\n")
  # print("pd_user_review")
  # print(pd_user_review())
  # print("pd_comment_status")
  # print(pd_comment_status())
  
  if (PKPDEnv("verbose")) cat("profile_choices\n")
  profiles <- pd_all_profiles()
  comment_status <- pd_comment_status()
  # Only none-reviewed
  if (input$pd_show_profiles == "non-reviewed") {
    user_review <- pd_user_review()
    profiles <- setdiff(pd_all_profiles(),  user_review)
    
    if (!length(profiles)) {
      shinyalert::shinyalert("No none reviewed profiles left", type = "info")
      profiles <- pd_all_profiles()
      shinyWidgets::updatePrettyRadioButtons(session, "show_profiles", selected = "All")
    }
  }
  
  #$comment
  
  # only with comments
  if (input$pd_show_profiles == "with comments") {
    profiles <- intersect(profiles,  comment_status[["with comments"]])
  }
  
  
  # Only with non-final comments
  if (input$pd_show_profiles == "non-final") {
    profiles <- intersect(profiles,  
      unlist(comment_status[setdiff(names(comment_status), c("with comments", "Final", "No action"))]))
  }
  
  # Only with awaiting comments
  if (input$pd_show_profiles == "Awaiting approval") {
    profiles <- intersect(profiles,  comment_status[["Awaiting approval"]])
  }
  
  # Only with re-evaluate comments
  if (input$pd_show_profiles == "Re-evaluate") {
    profiles <- intersect(profiles,  comment_status[["Re-evaluate"]])
  }
  
  if (!length(profiles)) {
    shinyalert::shinyalert("No profiles fulfill the chosen criteria - showing all profiles", type = "info")
    profiles <- pd_all_profiles()
    shinyWidgets::updatePrettyRadioButtons(session, "show_profiles", selected = "All")
  }
  
  #print(profiles)
  profiles
  data_adpc <- pddata()
  
  sub_adpc <- data_adpc[data_adpc$profid %in% profiles, ]
  
  # ord_var <- input$select_profile_order
  # if (isolate(setupRV$add_order) && !is.null(ord_var))
  #   profiles <- dplyr::arrange(sub_adpc, !!! rlang::syms(ord_var)) %>% dplyr::pull("profid") %>% unique()
  
  profiles
})



#----------------------------------------#
######     PD profile selector      ######
#----------------------------------------#


output$pd_profile_select_ui <- renderUI({
  req(pd_profile_choices())
  
  if (PKPDEnv("verbose")) cat("pd_profile_select_ui\n")
  
  profiles <- pd_profile_choices()
  
  isolate({
    if (!is.null(input$pd_select_profile) && input$pd_select_profile %in% profiles) {
      selected <- input$pd_select_profile
    }
    else
      selected <-  profiles[1]
  })
  
  selectizeInput("pd_select_profile", "Select profile", choices = as.list(profiles),
              selected = as.list(selected), options = list(maxOptions = 5000))
  
})

#--------------------------------------------------#
######      Get time ranges for all data      ######
#--------------------------------------------------#

pd_time_range <- reactive({
  req(pddata())
  
  if (PKPDEnv("verbose")) cat("pd_time_range\n")
  
  
  adpc_data <- pddata()
  
  visit_data <- adpc_data %>%
    dplyr::group_by(profid) %>%
    dplyr::summarize(min_x = floor(min(time, na.rm = TRUE)), max_x = ceiling(max(time, na.rm = TRUE)),
                     min_y = min(aval, na.rm = TRUE),   max_y = max(aval, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  return(visit_data)
})


output$pd_axes <- renderUI({
  req(pddata(), pd_time_range())
  if (PKPDEnv("verbose")) cat("pd_axes\n")
  data_adpc <- pddata()
  
  profiles <- unique(data_adpc$profid)
  
  # generate initial range
  isolate({
    if (!is.null(input$pd_select_profile))
      range <- pd_time_range() %>% dplyr::filter(profid == input$pd_select_profile)
    
    else
      range <- pd_time_range() %>% dplyr::filter(profid == profiles[1])
  })
  
  tagList(
    sliderInput("pd_xRange", "Range of x-axis:",
                min = range$min_x, max = range$max_x,
                value = c(range$min_x, range$max_x),
                round = FALSE, ticks = TRUE)
  )
})

# New ranges by profile change
observeEvent(input$pd_select_profile, {
  
  if (PKPDEnv("verbose")) cat("pd_new_ranges_by_prof\n")
  
  pdplotRV$highlighted <- NULL
  
  range <- pd_time_range() %>% dplyr::filter(profid == input$pd_select_profile)
  
  pdplotRV$range <- range
  
  updateSliderInput(session, "pd_xRange",
                    min = range$min_x, max = range$max_x,
                    value = c(range$min_x, range$max_x))
})

# on update of time ranges
observeEvent(input$pd_xRange, {
  
  current_range <- c(pdplotRV$range$min_x, pdplotRV$range$max_x)
  new_range     <- c(input$pd_xRange[1], input$pd_xRange[2])
  
  if (PKPDEnv("verbose")) cat(paste("pd_xRange\n old", current_range[1], current_range[2],
                                    "\n new", input$pd_xRange[1], input$pd_xRange[2], "\n"))
  
  
  if (any(current_range != new_range)) {
    pdplotRV$change_from_input <- TRUE
    pdplotRV$range$min_x <- input$pd_xRange[1]
    pdplotRV$range$max_x <- input$pd_xRange[2]
  }
})

# Get the plot ranges as a reactive
pd_plot_range <- reactive({
  if (PKPDEnv("verbose")) cat("pdplotRV$range updated\n")
  pdplotRV$range
})

#----------------------------------------#
######        plotting pd           ######
#----------------------------------------#


pd_plot_data <- reactive({
  
  req(setup_pd_plots_data(), pddata())
  
  if (PKPDEnv("verbose")) cat("pd_plot_data\n")
  
  plot_data <- setup_pd_plots_data()
  
  pddata_keep <- pddata()
  
  
  pddata_keep$panel_name <- factor(plot_data$panel_name[match(pddata_keep$paramcd, plot_data$paramcd)],
                                   unique(plot_data$panel_name))
  
  pddata_keep$symbol <- plot_data$symbol[match(pddata_keep$paramcd, plot_data$paramcd)]
  pddata_keep$line   <- plot_data$line[match(pddata_keep$paramcd, plot_data$paramcd)]
   
  plot_data_out <- pddata_keep
  plot_data_out
})

pd_plot_profile_R <- reactive({
#observe({  
  
  req(pd_plot_data(), input$pd_select_profile, 
      setup_pd_panel_data(),
      setup_pd_plots_data(), 
      pd_plot_range(), pd_col_scheme(), pd_comments_data_profile())
  
  if (!is.null(pd_col_scheme()))
    req(nrow(pd_col_scheme()) > 0)
  
  if (PKPDEnv("verbose")) cat("pd_plot_profile\n")
 
  panel_data <-  setup_pd_panel_data()
  plot_data <- setup_pd_plots_data()
  

 # col_scheme$lty <- "solid"
  
  plot_data_out <- pd_plot_data() %>% dplyr::filter(!is.na(panel_name))
  col_scheme <- pd_col_scheme()

  highlight <- pd_plot_highlight_time()
    
  if ("color_by" %in% colnames(plot_data_out)) { 
    adpdc_sub <- plot_data_out[plot_data_out$profid == input$pd_select_profile, ] %>%   
      dplyr::left_join(col_scheme[, c("paramcd", "color_by", "label")], by = c("paramcd", "color_by"))
    
    if (!is.null(highlight) && nrow(highlight) > 0 && !identical(highlight, numeric(0)) && any(highlight$time_type == "point"))
      highlight <- highlight %>% dplyr::left_join(col_scheme, by = c("paramcd", "color_by"))
    
    n_legend_rows <- length(unique(plot_data_out$color_by))
    
    col_scheme <- col_scheme[paste(col_scheme$paramcd, col_scheme$color_by) %in% paste(adpdc_sub$paramcd, adpdc_sub$color_by), ]
    
  } else {
    adpdc_sub <- plot_data_out[plot_data_out$profid == input$pd_select_profile, ] %>%   
      dplyr::left_join(col_scheme[, c("paramcd", "label")], by = c("paramcd"))
    
    if (!is.null(highlight) && nrow(highlight) > 0 && !identical(highlight, numeric(0))  && any(highlight$time_type == "point"))
      highlight <- highlight %>% dplyr::left_join(col_scheme, by = c("paramcd"))
    
    n_legend_rows <- 1
    
    col_scheme <- col_scheme[col_scheme$paramcd %in% adpdc_sub$paramcd, ]
  }
  
 
  
  comments <- pd_comments_data_profile()
  
  comments_int <- comments[comments$time_type == "interval" & 
                             !is.na(comments$time_type), ]
  
  comments_point <- comments[comments$time_type == "point" & 
                             !is.na(comments$time_type), ]
  


  range <- range_keep <- pd_plot_range()

  p <- ggplot2::ggplot(adpdc_sub, mapping = ggplot2::aes(time, aval))

  if (!is.null(highlight) && nrow(highlight) > 0 && !identical(highlight, numeric(0))) {
    highlight_int <- highlight[highlight$time_type == "interval", ]
  
    if (nrow(highlight_int) > 0) {
      
      if (any(!is.na(highlight_int$panel_name)))
        p <- p + 
          ggplot2::geom_rect(data =  highlight_int[!is.na(highlight_int$panel_name), ], 
                             ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), 
                             alpha = 0.1, fill = "#3F9C35", inherit.aes = FALSE, show.legend = FALSE)
      
      
      if (any(is.na(highlight_int$panel_name)))
        p <- p + 
          ggplot2::geom_rect(data =  highlight_int[is.na(highlight_int$panel_name), setdiff(colnames(highlight_int), "panel_name")], 
                             ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf), 
                             alpha = 0.1, fill = "#3F9C35", inherit.aes = FALSE, show.legend = FALSE)
    }
  }
  
  
  for (i in seq_len(nrow(col_scheme))) {
    shape <- ifelse(!is.nan(col_scheme$shape[i]), col_scheme$shape[i], NA)
    size <- ifelse(!is.nan(col_scheme$size[i]), col_scheme$size[i], 0)
    lty <- ifelse(!is.na(col_scheme$lty[i]), col_scheme$lty[i], "blank")
    width <- ifelse(!is.nan(col_scheme$width[i]), col_scheme$width[i], 0)
    alpha <- col_scheme$alpha[i]
    fill <- ifelse(!is.na(col_scheme$fill[i]), col_scheme$fill[i], col_scheme$color[i])
    show_legend <- col_scheme$show_legend[i]
    
    if (col_scheme$line[i] == "region") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_ribbon(data = . %>% dplyr::filter(line == "region" & paramcd == param_xx & color_by == color_by_xx),
                                                      mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = label), shape = NaN, linetype = "blank", alpha = alpha, size = NaN, 
                                                      na.rm = TRUE, show.legend = show_legend, key_glyph = "point"),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_ribbon(data = . %>% dplyr::filter(line == "region" & paramcd == param_xx),
                                                      mapping = ggplot2::aes(ymin = lower, ymax = upper, fill = label), shape = NaN, linetype = "blank", alpha = alpha, size = NaN, 
                                                      na.rm = TRUE, show.legend = show_legend, key_glyph = "point"),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
      
    
    if (col_scheme$line[i] == "h-line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_hline(data = . %>% dplyr::filter(line == "h-line" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(yintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, 
                                                     na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_hline(data = . %>% dplyr::filter(line == "h-line" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(yintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, 
                                                     na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
      
    
    if (col_scheme$line[i] == "v-line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_vline(data = . %>% dplyr::filter(line == "v-line" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(xintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, 
                                                     na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_vline(data = . %>% dplyr::filter(line == "v-line" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(xintercept = aval, colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, 
                                                     na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
      
    
    if (col_scheme$line[i] == "line") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_line(data = . %>% dplyr::filter(line == "line" & paramcd == param_xx & color_by == color_by_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_line(data = . %>% dplyr::filter(line == "line" & paramcd == param_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
      
    
    if (col_scheme$line[i] == "step") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_step(data = . %>% dplyr::filter(line == "step" & paramcd == param_xx & color_by == color_by_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_step(data = . %>% dplyr::filter(line == "step" & paramcd == param_xx),
                                                    mapping = ggplot2::aes(colour = label), fill = "transparent", shape = NaN, linetype = lty, alpha = alpha, size = width, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
    
    if (col_scheme$symbol[i] == "point") {
      if ("color_by" %in% colnames(col_scheme) && !is.na(col_scheme$color_by[i])) {
        p <- p + eval(substitute(ggplot2::geom_point(data = . %>% dplyr::filter(symbol == "point" & paramcd == param_xx & color_by == color_by_xx),
                                                     mapping = ggplot2::aes(colour = label), fill = fill, shape = shape, linetype = "blank", alpha = alpha, size = size, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i], color_by_xx = col_scheme$color_by[i])))
      }
      else {
        p <- p + eval(substitute(ggplot2::geom_point(data = . %>% dplyr::filter(symbol == "point" & paramcd == param_xx),
                                                     mapping = ggplot2::aes(colour = label), fill = fill, shape = shape, linetype = "blank", alpha = alpha, size = size, na.rm = TRUE, show.legend = show_legend),
                                 list(param_xx = col_scheme$paramcd[i])))
      }
    }
      
  }
 
 
  # Highlight of selected points
  if (!is.null(highlight) && nrow(highlight) > 0 && !identical(highlight, numeric(0))) {
    highlight_point <- highlight[highlight$time_type == "point", ]
    
    if (nrow(highlight_point) > 0) {
      
      # col_scheme_high <- col_scheme[col_scheme$symbol == "point", ]
      # col_scheme_high$size <-  col_scheme_high$size + 3
      # col_scheme_high$label <- paste("Highlighted", col_scheme_high$label)
      #   
      # col_scheme <- dplyr::bind_rows(col_scheme, col_scheme_high)
      # 
      # highlight_point$label <- paste("Highlighted", highlight_point$label)
      
      col_scheme_point <- col_scheme[col_scheme$symbol == "point", ]
      
      for (i in seq_len(nrow(col_scheme_point))) {
        highlight_shape <- ifelse(!is.nan(col_scheme_point$shape[i]), col_scheme_point$shape[i], NA)
        highlight_size <- max(col_scheme_point$size, na.rm = TRUE) + 3
        highlight_alpha <- col_scheme_point$alpha[i]
        highlight_fill <- ifelse(!is.na(col_scheme_point$fill[i]), col_scheme_point$fill[i], col_scheme_point$color[i])
        
        if ("color_by" %in% colnames(col_scheme_point) && !is.na(col_scheme_point$color_by[i])) {
          
          
          if (any(!is.na(highlight_point$panel_name)))
            p <- p + eval(substitute(ggplot2::geom_point(data = highlight_point %>% dplyr::filter(!is.na(highlight_point$panel_name) & paramcd == param_xx & color_by == color_by_xx),
                                                       mapping = ggplot2::aes(colour = label), fill = highlight_fill, shape = highlight_shape, size = highlight_size, alpha = highlight_alpha, na.rm = TRUE, show.legend = FALSE),
                                   list(param_xx = col_scheme_point$paramcd[i], color_by_xx = col_scheme_point$color_by[i])))
          
          if (any(is.na(highlight_point$panel_name)))
            p <- p + eval(substitute(ggplot2::geom_point(data = highlight_point %>% dplyr::filter(is.na(highlight_point$panel_name) & paramcd == param_xx & color_by == color_by_xx) %>% dplyr::select(-panel_name),
                                                         mapping = ggplot2::aes(colour = label), fill = highlight_fill, shape = highlight_shape, size = highlight_size, alpha = highlight_alpha, na.rm = TRUE, show.legend = FALSE),
                                     list(param_xx = col_scheme_point$paramcd[i], color_by_xx = col_scheme_point$color_by[i])))
            
        }
        else {
          if (any(!is.na(highlight_point$panel_name)))
            p <- p + eval(substitute(ggplot2::geom_point(data = highlight_point %>% dplyr::filter(!is.na(highlight_point$panel_name) & paramcd == param_xx),
                                                       mapping = ggplot2::aes(colour = label), fill = highlight_fill, shape = highlight_shape, size = highlight_size, alpha = highlight_alpha, na.rm = TRUE, show.legend = FALSE),
                                   list(param_xx = col_scheme_point$paramcd[i])))
          
          if (any(is.na(highlight_point$panel_name)))
            p <- p + eval(substitute(ggplot2::geom_point(data = highlight_point %>% dplyr::filter(is.na(highlight_point$panel_name) & paramcd == param_xx) %>% dplyr::select(-panel_name),
                                                         mapping = ggplot2::aes(colour = label), fill = highlight_fill, shape = highlight_shape, size = highlight_size, alpha = highlight_alpha, na.rm = TRUE, show.legend = FALSE),
                                     list(param_xx = col_scheme_point$paramcd[i])))
          
          
        }
      }
    }
  } 
  
  # Highlight of comment intervals
  if (any(sapply(comments_int$time_points, length))) {
    rows <- sapply(comments_int$time_points, function(x) length(x) > 0)
    print(comments_int)
    new_data <- comments_int[rows, ] %>% dplyr::mutate(key = list(c("from", "to"))) %>%
      tidyr::unnest(time_points, key, .preserve = paramcd) %>% 
      tidyr::spread(key = key, value = time_points) %>% 
      tidyr::unnest(paramcd) %>% 
      dplyr::left_join(plot_data[, c("paramcd", "panel_name")], by = "paramcd")
    
    
    new_data$panel_name <- factor(new_data$panel_name, panel_data$panel_name)
    
    new_data$label <- "Review comment "
    
    col_scheme_com_int <- tibble::tibble(label = "Review comment ", line = "region", color = "#009FDA", alpha = 0.2, fill = "#009FDA", lty = "blank", shape = NaN, size = NaN, width = NaN, show_legend = TRUE)
    
    col_scheme <- dplyr::bind_rows(col_scheme, col_scheme_com_int)
    
    if (any(!is.na(new_data$panel_name)))
      p <- p + 
      ggplot2::geom_rect(data =  new_data[!is.na(new_data$panel_name), ], 
                         ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = label),
                         shape = col_scheme_com_int$shape[1],
                         linetype = col_scheme_com_int$lty[1], alpha = col_scheme_com_int$alpha[1],
                         inherit.aes = FALSE, show.legend = col_scheme_com_int$show_legend[1], key_glyph = "point")
    
    
    if (any(is.na(new_data$panel_name)))
      p <- p + 
      ggplot2::geom_rect(data =  new_data[is.na(new_data$panel_name), setdiff(colnames(new_data), "panel_name")], 
                         ggplot2::aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = label),
                         shape = col_scheme_com_int$shape[1],
                         linetype = col_scheme_com_int$lty[1], alpha = col_scheme_com_int$alpha[1],
                         inherit.aes = FALSE, show.legend = col_scheme_com_int$show_legend[1], key_glyph = "point")
  }
  
  # Highlight of comment points
  if (!is.null(comments_point) && nrow(comments_point) > 0) {
    
    if (nrow(comments_point) > 0) {
      
      
      # "%pin%" <- function(x, table) pmatch(x, table, nomatch = 0) > 0
      com_data_point <- adpdc_sub %>% 
        dplyr::filter(paste(round(time, 5), paramcd) %in% 
                        paste(round(unlist(comments_point$time_points), 5), unlist(comments_point$time_point_paramcd)))
      
      if (nrow(com_data_point) > 0){
        com_data_point$label <- "Review comment"
        col_scheme_com <- tibble::tibble(label = "Review comment", symbol = "point", shape = 1, color = "#E64A0E", fill = "transparent", lty = "blank", stroke = 1.5, alpha = 1, show_legend = TRUE)
        col_scheme_com$size <-  max(col_scheme[col_scheme$symbol == "point", ]$size, na.rm = TRUE) + 5
        
        col_scheme <- dplyr::bind_rows(col_scheme, col_scheme_com)
        
        p <- p + 
          ggplot2::geom_point(data = com_data_point, 
                              mapping = ggplot2::aes(colour = label), fill = col_scheme_com$fill[1], shape = col_scheme_com$shape[1], linetype = col_scheme_com$lty[1], alpha = col_scheme_com$alpha[1], size = col_scheme_com$size[1], stroke = col_scheme_com$stroke[1], na.rm = TRUE, show.legend = col_scheme_com$show_legend[1])
      }
    }
  } 
  
  print(col_scheme)
  
  # create axes
  
  time_unit <- ""
  if ("time_unit" %in% colnames(adpdc_sub)) 
    time_unit <- paste0("(", adpdc_sub$time_unit[1], ")")
  
  p <- p + 
    ggplot2::labs(#title = paste("PK-profile for", profile),
      x = paste("Time", time_unit),
      y = paste("Value"))
  
  col_scheme$size[is.nan(col_scheme$size)] <- col_scheme$width[is.nan(col_scheme$size)]
  col_scheme$size[is.nan(col_scheme$size)] <- 0
  col_scheme$lty[is.na(col_scheme$lty)] <- "blank"
  col_scheme$alpha[is.na(col_scheme$alpha)] <- 1
  
  legends <- col_scheme %>% dplyr::filter(!(line == "region" & symbol == "")) %>% 
    dplyr::mutate(stroke = dplyr::case_when(label == "Review comment" ~ 1.5, symbol == "point" ~ 0.7, TRUE ~ NA_real_),
                  size = ifelse(label == "Review comment", 5, max(min(size, width, 4, na.rm = TRUE), 1.25, na.rm = TRUE))) %>% 
    dplyr::select(label, color, lty, alpha, shape, fill, size, stroke) %>%
    dplyr::distinct(label, .keep_all = TRUE) %>% dplyr::arrange(label)
  
  print(legends)
  
  legends_region <- col_scheme %>% dplyr::filter(line == "region") %>% 
    dplyr::select(label, lty, alpha, shape, fill, size) %>%
    dplyr::mutate(color = fill, lty = "blank", shape = 15, size = 6) %>%
    dplyr::distinct(label, .keep_all = TRUE) %>% dplyr::arrange(label)
  
  print(legends_region)
  
  # Apply color scheme
  p <- p + ggplot2::theme_gray(base_size = 14) + 
    ggplot2::facet_grid(rows = ggplot2::vars(panel_name), scales = "free_y") +
    ggplot2::coord_cartesian(xlim = c(range$min_x, range$max_x)) +
    ggplot2::theme(legend.position = "bottom") + 
    ggplot2::guides(size = FALSE, alpha = FALSE, stroke = FALSE, shape = FALSE, linetype = FALSE,
                    colour = ggplot2::guide_legend(title = "", order = 1, nrow = n_legend_rows,
                                                   override.aes = list(fill = structure(legends$fill, names = legends$label),
                                                                       alpha = structure(legends$alpha, names = legends$label),
                                                                       size = structure(legends$size, names = legends$label),
                                                                       stroke = structure(legends$stroke, names = legends$label),
                                                                       shape = structure(legends$shape, names = legends$label),
                                                                       linetype = structure(legends$lty, names = legends$label))),
                    fill = ggplot2::guide_legend(title = "", order = 2, nrow = n_legend_rows,
                                                 override.aes = list(color = structure(legends_region$color, names = legends_region$label),
                                                                     alpha = structure(legends_region$alpha, names = legends_region$label),
                                                                     size = structure(legends_region$size, names = legends_region$label),
                                                                     shape = structure(legends_region$shape, names = legends_region$label),
                                                                     linetype = structure(legends_region$lty, names = legends_region$label)))) +
    ggplot2::scale_colour_manual(values = structure(legends$color, names = legends$label)) +
    ggplot2::scale_fill_manual(values = structure(legends_region$fill, names = legends_region$label))
    
  p
})


output$pd_plot_profile <- renderPlot({
  req(pd_plot_profile_R())
   
  pd_plot_profile_R()
  
}, height = function(){200 * nrow(setup_pd_panel_data()) + 100})


#----------------------------------------#
######      time shift              ######
#----------------------------------------#

observeEvent(input$pd_prev_period, {
  from <- input$pd_xRange[1]
  to   <- input$pd_xRange[2]
  
  new_from <- from - (to - from)
  new_to   <- from
  
  updateSliderInput(session, "pd_xRange",
                    value = c(new_from, new_to)
  )
})

observeEvent(input$pd_next_period, {
  from <-  input$pd_xRange[1]
  to   <-  input$pd_xRange[2]
  
  new_from <- to
  new_to <- to + (to - from)
  
  range <- pd_time_range() %>% dplyr::filter(profid == input$pd_select_profile)
  
  pdplotRV$range$min_x <- floor(max(new_from, range$min_x))
  pdplotRV$range$max_x <- ceiling(min(new_to, range$max_x))
  
  updateSliderInput(session, "pd_xRange",
                    value = c(new_from, new_to)
  )
})

#----------------------------------------#
######      Setup_highlight         ######
#----------------------------------------#


observeEvent(input$pd_plot_profile_brush, {
  output$pd_zoom_help <-
    renderUI(tags$h1("click to choose or double click to zoom",
                     style = "color:blue;"))
})


# Collect into a reactive
pd_plot_highlight_time <- reactive({
  print(head(pdplotRV$highlighted))
  pdplotRV$highlighted
})


# Double click
observeEvent(input$pd_plot_profile_dblclick, {
  output$pd_zoom_help <- NULL
  brush    <- input$pd_plot_profile_brush
  
  if (!is.null(pdplotRV$highlighted) && nrow(pdplotRV$highlighted) > 0 && !identical(pdplotRV$highlighted, numeric(0))) {
    highlighted <- pdplotRV$highlighted
    highlighted_points <- pdplotRV$highlighted %>% dplyr::filter(time_type == "point")
  }
  else {
    highlighted <- NULL
    highlighted_points <- NULL
  }
  
  if (PKPDEnv("verbose")) cat("pd_plot_profile_dblclick\n")
  
  time_type  =  structure(c("interval", "point"), names = c("Time interval", "Points"))[input$pd_select_times]
  
  if (is.null(brush)) {
        
    panel_data <- setup_pd_panel_data()
    plot_data <- setup_pd_plots_data()
    
    plot_data_out <- pd_plot_data()
    plot_data_out <- plot_data_out[plot_data_out$profid == input$pd_select_profile, ]
    
    
    click_at <- input$pd_plot_profile_dblclick
    
    click_at$panelvar1 <- factor(click_at$panelvar1, levels = levels(plot_data_out$panel_name))
    
    
    data_f <- plot_data_out %>% 
      dplyr::filter(paramcd %in% intersect(unlist(panel_data[panel_data$panel_name == click_at$panelvar1, ]$paramcd), 
                                           plot_data[plot_data$symbol != "",]$paramcd)) %>%
      dplyr::mutate(panel_name = click_at$panelvar1)
    
  
    brushed_data <- nearPoints(data_f, click_at, "time", "aval", panelvar1 = "panel_name") %>%
      dplyr::mutate(time_type = time_type) %>% dplyr::distinct_all()
    # subdata <- dplyr::select(brushed_data, time) %>% dplyr::distinct()
    
    
    # if no points near double click highlight and zoom is reinitialized
    if (nrow(brushed_data) == 0 | time_type == "interval") {
      pdplotRV$highlighted <- NULL
      
      # clear zoom
      range <- pd_time_range() %>% dplyr::filter(profid == input$pd_select_profile)
      #pdplotRV$range <- range
      updateSliderInput(session, "pd_xRange",  value = c(range$min_x, range$max_x))
    }
    
    # if some points are already highlighted
    else if (!is.null(highlighted_points) && nrow(highlighted_points) > 0){
        
        # if the point(s) near the click are already selected then they should be deselected 
        if (nrow(plyr::match_df(highlighted_points, brushed_data))) {
          keys <- plyr::join.keys(highlighted_points, brushed_data, 
                                  intersect(names(highlighted_points), names(brushed_data)))
          
          pdplotRV$highlighted <- highlighted_points[!keys$x %in% keys$y,]
        }
        
        # if the point(s) near the click are not already selected then they should be together with the current selected point(s)
        else{
          pdplotRV$highlighted <-  dplyr::bind_rows(brushed_data, highlighted_points)
        }
    }
    # if no points are currently highlighted then the point(s) near the click should be highlighted
    else {
      pdplotRV$highlighted <-  brushed_data
    }
    
    # deselect currently selected comment rows
    DT::selectRows(pd_DTproxy, NULL)
  }
  
  # if there is a brush
  else {
    range <- pd_time_range() %>% dplyr::filter(profid == input$pd_select_profile)
    
    pdplotRV$range$min_x <- floor(max(brush$xmin, range$min_x))
    pdplotRV$range$max_x <- ceiling(min(brush$xmax, range$max_x))
    
    pdplotRV$range$min_y <- brush$ymin
    pdplotRV$range$max_y <- brush$ymax

    updateSliderInput(session, "pd_xRange", value = c(pdplotRV$range$min_x, pdplotRV$range$max_x))
  }
})


# Select brushed points
observeEvent(input$pd_plot_profile_click, {
  output$pd_zoom_help <- NULL
  brush <- input[["pd_plot_profile_brush"]]
  
  if (PKPDEnv("verbose")) cat("pd_plot_profile_click\n")
  
  time_type  =  structure(c("interval", "point"), names = c("Time interval", "Points"))[input$pd_select_times]
  
  panel_data <- setup_pd_panel_data()
  
  plot_data <- setup_pd_plots_data()
  
  if (!is.null(brush)) {
    plot_data_out <- pd_plot_data()
    plot_data_out <- plot_data_out[plot_data_out$profid == input$pd_select_profile, ]
    
    brush$panelvar1 <- factor(brush$panelvar1, levels = levels(plot_data_out$panel_name))
    
    if (time_type == "interval")
      high <- tibble::tibble(time_type = time_type, from = brush$xmin, to = brush$xmax, panel_name = brush$panelvar1)
    
    if (time_type == "point") {
      data_f <- plot_data_out %>% 
        dplyr::filter(paramcd %in% intersect(unlist(panel_data[panel_data$panel_name == brush$panelvar1, ]$paramcd), 
                                             plot_data[plot_data$symbol != "",]$paramcd)) %>%
        dplyr::mutate(panel_name = brush$panelvar1)
      
      high <- brushedPoints(df = data_f, brush = input[["pd_plot_profile_brush"]], 
                    xvar = "time", yvar = "aval", panelvar1 = "panel_name") %>%
        dplyr::mutate(time_type = time_type) %>% dplyr::distinct_all() 
    }
      
    pdplotRV$highlighted <- high
  }
  else {
    return(NULL)
  }
})




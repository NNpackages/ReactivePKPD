
#----------------------------------------#
######         load pd data         ######
#----------------------------------------#


pddata <- reactive({
  
  req(trial_info())
  
  inst_info <- trial_info()
  
  req(inst_info$trial_pd_data_dir)
  if (PKPDEnv("verbose")) cat("creating pd data\n")
  
  if (!is.null(GlobalRV$PDdata[[inst_info$trial_id]])) {
    ds <- GlobalRV$PDdata[[inst_info$trial_id]]
  } else {
    validate(need(length(list.files(inst_info$trial_pd_data_dir, pattern = "app_adpd.rds")) > 0, "No PD data have been uploaded for the trial"))
    
    ds <- readRDS(list.files(inst_info$trial_pd_data_dir, full.names = TRUE, pattern = "*pd.rds")[1])
    GlobalRV$PDdata[[inst_info$trial_id]] <- ds
  }
  
  validate(need(all(c("profid", "paramcd", "param", "time", "aval") %in% colnames(ds)), 
                "required columns not present in data "))
  
  # reset plot_data
  pdplotRV$plot_data  <- NULL
  pdplotRV$col_scheme <- NULL
  pdplotRV$panel_data <- NULL
  
  
  ds
}) %>% dedupe()


#----------------------------------------#
######        all_pd_profiles       ######
#----------------------------------------#

pd_all_profiles <- reactive({
  req(pddata())
  if (PKPDEnv("verbose")) cat("pd_all_profiles\n")
  if ("profid" %in% colnames(pddata())) {
    unique(pddata()$profid)
  } else {
    character(0)
  }
})


#----------------------------------------#
######         load pk data         ######
#----------------------------------------#

# decouple global data from trial data
pkdata <- reactive({
  req(trial_info())
  inst_info <- trial_info()
  
  req(inst_info$trial_pk_data_dir)
  if (PKPDEnv("verbose")) cat("creating pk data\n")
  
  if (!is.null(GlobalRV$PKdata[[inst_info$trial_id]])) {
    ds <- GlobalRV$PKdata[[inst_info$trial_id]]
  } else {
    validate(need(length(list.files(inst_info$trial_pk_data_dir, pattern = "app_adpc.rds")) > 0, "No PK data have been uploaded for the trial"))
    ds <- readRDS(list.files(inst_info$trial_pk_data_dir, full.names = TRUE, pattern = "*pc.rds")[1])
    
    if (!("plot_time" %in% colnames(ds))) {
      if (PKPDEnv("verbose")) cat("plot_time created from time variable\n")
      
      ds$plot_time <- ds$time
    }
    
    GlobalRV$PKdata[[inst_info$trial_id]] <<- ds
  }
  
  setupRV$pk_data <- ds 
  
  
  ds$col_flag <- ""
  if (!"anelfl" %in% colnames(ds)) ds$anelfl <- "Y"
  ds$col_flag[ds$tailfl == "" | ds$tailfl == "N"]  <- "Non-tail"
  ds$col_flag[ds$anelfl != "Y"] <- "Excluded"
  ds$col_flag[ds$tailfl == "Y"] <- "Tail"
  ds
})  %>% dedupe()


#----------------------------------------#
######        all_profiles          ######
#----------------------------------------#

pkplotRV <- reactiveValues(new_profile = TRUE)

all_profiles <- reactive({
  req(pkdata())
  if (PKPDEnv("verbose")) cat("all_profiles\n")
  pkdata_keep <- pkdata()
  unique(pkdata()$profid)
})
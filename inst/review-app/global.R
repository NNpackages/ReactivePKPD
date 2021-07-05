#---------------------------------#
######        Packages       ######
#---------------------------------#
#drat::addRepo("LocalRepo", "file:drat")

#install.packages("NNPKPD", type = "source")
#packrat::init()

# loaded packages
# load.packages <- c("NNPKPD", "shiny" , "shinydashboard", "magrittr")
# for (pack in load.packages) library(pack, character.only = TRUE)
# rm(load.packages, pack)

library("NNPKPD")
library("shiny")
library("shinydashboard")
library("magrittr")
library("promises")
library("future")
#library("future.apply")
plan(multiprocess)


#-------------------------------------#
######    Initiation variables  ######
#-------------------------------------#
#identical(Sys.getenv("ON_DEVOPS"), "TRUE")
if (identical(Sys.getenv("ON_DEVOPS"), "TRUE")) {
  NNPKPD::runPKPDreview(workspace = "shinyServer", username = NULL, admins = c("SFFL", "ABIU", "KFSM"),
                        verbose = TRUE, access_data_path = "/data/NNPKPD/")
} else {

  # only run when when not started via runPKPDreview
  if (is.null(PKPDEnv("runApp"))) {
  
    PKPDEnv("online_user", character(0))
    PKPDEnv("trial", NULL)
    PKPDEnv("username", NULL)
    PKPDEnv("workspace", "server")
    PKPDEnv("verbose", TRUE)
    PKPDEnv("session_info", list())
    PKPDEnv("admins", "SFFL")
    PKPDEnv("access_data_path", pDrive("general", "toolpool", "rug", "nntools",
                                       "pkReviewer", "appData_test2"))
    
    PKPDEnv("data_storage", file.path(PKPDEnv("access_data_path"), "trial_data"))
    PKPDEnv("workspace", "local")
    PKPDEnv("username", unname(Sys.info()["user"]))
  #  PKPDEnv("username", "Peter")
    #PKPDEnv("trial", 4226)
  }
}


#------------------------------------------#
######   Initiations before app run   ######
#------------------------------------------#

# create directories
lapply(file.path(PKPDEnv("access_data_path"), c("trial_setup", "trial_data", "avatars")), dir.create,
           showWarnings = FALSE, recursive = TRUE)



# To avoid NOTEs the R CMD check
utils::globalVariables(".data")

#' Get clock time for a certain time-zone
#'
#' @param tzone The time zone for the time
#'
#' @return The clock time at the specified time zone
#'
#' @examples
#' clockTime()
#' @export
clockTime <- function(tzone = "UTC") {
  time <- Sys.time()
  attr(time, "tzone") <- tzone
  time
}

#' Assign a comment ID to a new comment
#'
#' @param user The initials of the user making the comment
#' @param type Optional, type of id, e.g. comment, reply, status
#' @param time Optional, time to use in the id
#' @param random Optional, random number to ensure uniqueness
#'
#' @return Character string with the id
#'
#' @importFrom stats runif
#' @export
commentID <- function(user, type = "comment",
                      time = clockTime(),
                      random = round(runif(length(user), 1000, 9999))) {
  paste(type, tolower(user), gsub(" ", "_", gsub("[-:]", "", as.character(time))),
        random, sep = "_")
}

#' assign certain attibutes to an object
#'
#' @param object The object to which the attributes are assigned
#' @param new_attributes a list of attributes
#'
#' @return the object with the assigned attributes
attributeAssign <- function(object, new_attributes) {
  for (i in seq_along(new_attributes))
    attr(object, names(new_attributes)[i]) <- new_attributes[[i]]
  object
}

#' Setup the comment folder directory
#'
#' @param path Path to the folder where the comment data should be setup
#' @param expected_folders the folders that should be created if not there
#' @param users vector of initials for which review folders are created
#'
#' @return List with the paths to the various folders
#' @export
#'
#' @examples
#' \dontrun{
#' comment_setup <- setUpCommentFolders()
#' }
setUpCommentFolders <- function(path = file.path(getwd(), "comment_data"),
                                expected_folders = c("flag_data", "comments",
                                                     "replies", "status",
                                                     "edits", "com_delete", "rep_delete",
                                                     "review"),
                                users = "") {

  # expected folders
  folders <- structure(file.path(path, expected_folders), names = expected_folders)

  # create the missing
  missing <- folders[!dir.exists(folders)]
  lapply(missing, dir.create,  recursive = TRUE, showWarnings = FALSE)

  # Add user folders
  missing <- file.path(folders["review"], tolower(users))[
    !dir.exists(file.path(folders["review"], tolower(users)))]
  lapply(missing, dir.create,  recursive = TRUE, showWarnings = FALSE)

  # diffentiate folder types
  review_folder <- folders["review"]
  flag_data     <- folders["flag_data"]
  folders       <- folders[setdiff(names(folders), c("review", "flag_data"))]

  # return folder paths
  return(list(folders = folders, flag_data = flag_data,
              review_folder = review_folder))
}

#' check for comment files
#'
#' @param comment_setup An object from \code{\link{setUpCommentFolders}}
#'
#' @return List with the files in the various deictories
#' @export
#'
#' @examples
#'  \dontrun{
#' comment_setup <- initiateCommentData()
#' checkCommentFolders()
#' }
checkCommentFolders <- function(comment_setup) {
  lapply(comment_setup$folders, list.files)
}


#' Read or write flag data
#'
#' @param comment_setup An object from \code{\link{setUpCommentFolders}}
#' @param flag_data New flag data to save
#' @param rds Logical, should the file be saved as rds
#'
#' @return return the flag data
#' @export
#'
#' @examples
#' \dontrun{
#' comment_setup <- setUpCommentFolders()
#'
#' flagData(comment_setup)
#' }
#' @importFrom tools file_ext
flagData <- function(comment_setup, flag_data = NULL, rds = FALSE) {

  # get the path to the flag_data
  path <- comment_setup$flag_data

  # set the extension
  ext = ifelse(rds, "rds", "csv")

  # if the path is empty the temp data from the app is used to initiate the flag
  # data 
  if (!length(dir(path, pattern = paste0(".", ext, "$")))) {
    temp_path <- system.file("flag_data", package = "NNPKPD")

    file <- sort(dir(temp_path, pattern = paste0(".", ext, "$"),
                     full.names = TRUE), decreasing = TRUE)[1]

    flag_data <- switch( tools::file_ext(file),
                         csv = readTable(file),
                         rds = readRDS(file))
  }

  
  
  if (is.null(flag_data)) {
    
    # Get the newest file
    file <- sort(dir(path, pattern = paste0(".", ext, "$"),
                     full.names = TRUE), decreasing = TRUE)[1]

    flag_data <- switch( tools::file_ext(file),
                   csv = readTable(file),
                   rds = readRDS(file))
    
    # temporary fix to update of data structure
    if ("goto_point" %in% colnames(flag_data)) { 
      temp_path <- system.file("flag_data", package = "NNPKPD")
    
      file <- sort(dir(temp_path, pattern = paste0(".", ext, "$"),
                       full.names = TRUE), decreasing = TRUE)[1]
      
      flag_data <- switch( tools::file_ext(file),
                           csv = readTable(file),
                           rds = readRDS(file))
      
      file_new <- file.path(path, paste0("comment_flags_",
                                     gsub(" ", "_", gsub("[-:]", "", as.character(clockTime()))),
                                     ".", ext))
      
      switch( tools::file_ext(file_new),
              csv = writeTable(flag_data, file_new),
              rds = saveRDS(flag_data, file_new))
    }
    
  } else {
    # Write the file with clock time as time stamp
    file <- file.path(path, paste0("comment_flags_",
                                   gsub(" ", "_", gsub("[-:]", "", as.character(clockTime()))),
                                   ".", ext))

    switch( tools::file_ext(file),
                   csv = writeTable(flag_data, file),
                   rds = saveRDS(flag_data, file))
  }
  flag_data
}




#' Write a comment
#'
#' @param comment_data an object initialised by \code{\link{initCommentData}}.
#' @param user The initials of the user that gives the comment
#' @param profile The profiles which is associated with the comment
#' @param regarding What does the comment concern
#' @param comment What is the comment
#' @param time_points The time points which are associated with the comment
#' @param view_range The current x-axis range
#' @param paramcd The paramcd associated with the comment
#' @param color_by The color_by associated with the comment
#' @param time_type The type of points given to time_points. Can be "interval" or "point" 
#' @param time_point_paramcd The paramcd associated with each time_point
#'
#' @return Adds the comment to the comment data object.
#' @export
writeComment <- function(comment_data,
                         user, profile, regarding, comment,
                         time_points = numeric(0), view_range,
                         paramcd = "", color_by = "", time_type = "point", time_point_paramcd = "") {

  # Create comment and status id
  time   <- clockTime()
  random <- round(runif(length(user), 1000, 9999))

  comment_id <- commentID(user = user, type = "comment", time, random)
  status_id  <- commentID(user = user, type = "status", time, random)

  # Retrieve comment_setup
  comment_setup <- attr(comment_data, "comment_setup")

  # Get flag_data
  flag_data <- attr(comment_data, "flag_data")

  # Get the options data
  options <- attr(comment_data, "options")

 

  # Extract the comment
  s_row <- flag_data[flag_data$regarding == regarding & flag_data$comment == comment & 
                     flag_data$type == ifelse(length(time_points), "point", "profile"), ]

  if (nrow(s_row)) {
    # Get initiated action and status
    action <- s_row$action
    status <- s_row$status
  }  else {
    action = ""
    status = "Awaiting approval"
  }

  # create the complete comment
  complete <- tibble::tibble(id          = comment_id,
                             edit_id     = "",
                             edit_time   = time,
                             profile     = profile,
                             view_range  = list(view_range),
                             user        = user,
                             time        = time,
                             comment     = comment,
                             regarding   = regarding,
                             time_points = list(time_points),
                             action      = action,
                             reply       = list(list()),
                             status      = status,
                             paramcd     = paramcd,
                             color_by    = color_by,
                             time_type   = time_type,
                             time_point_paramcd = list(time_point_paramcd))

  
  # single the comment part
  comment_part <- complete[, setdiff(names(complete), c("reply", "status"))]

  # single the status
  status_part <-  tibble::tibble(comment_id       = comment_id,
                                 user             = user,
                                 status_edit_time = time,
                                 status           = status)

  # save the data
  if (options$save) {
    # get file_track to update the current files
    file_track <- attr(comment_data, "file_track")


    if (options$rds) {
      # save as rds
      saveRDS(comment_part, file.path(comment_setup$folders["comments"], paste0(comment_id, ".rds")))
      saveRDS(status_part, file.path(comment_setup$folders["status"], paste0(status_id, ".rds")))

      # update file_track
      if (!is.null(comment_data))
      file_track[["comments"]] <- c(file_track[["comments"]], paste0(comment_id, ".rds"))
      file_track[["status"]]   <- c(file_track[["status"]], paste0(status_id, ".rds"))

    } else {
      # save as csv
      writeTable(comment_part, file.path(comment_setup$folders["comments"], paste0(comment_id, ".csv")))
      writeTable(status_part, file.path(comment_setup$folders["status"], paste0(status_id, ".csv")))

      # update file_track
      if (!is.null(comment_data))
      file_track[["comments"]] <- c(file_track[["comments"]], paste0(comment_id, ".csv"))
      file_track[["status"]]   <- c(file_track[["status"]], paste0(status_id, ".csv"))
    }
    attr(comment_data, "file_track") <- file_track
  }

  if (nrow(comment_data)) {
    must_be <- structure(c(paramcd, color_by, time_type), names = c("paramcd", "color_by", "time_type"))
    for (char in names(must_be))
      if (!char %in% colnames(comment_data)) comment_data[, char] <- must_be[char]

    if (!"time_point_paramcd" %in% colnames(comment_data)) 
      comment_data[, "time_point_paramcd"] <- list(character(0))
  }
  # combine with comment data
  new_comment <- dplyr::bind_rows(comment_data, complete)

  # assign the new attributes to the
  new_attributes <-
    attributes(comment_data)[setdiff(names(attributes(comment_data)),
                                     c("names", "row.names", "class" ))]
  new_comment <- attributeAssign(new_comment, new_attributes)

  return(new_comment)
}


#' Read the comment part of a comment
#'
#' @param path Path the the folder to read from
#' @param files The files to read, if NULL all files in the directory is read
#'
#' @return A tibble with the comments
#' @export
#' @importFrom dplyr bind_rows
#' @importFrom fasttime fastPOSIXct
#' @importFrom tibble as_tibble tibble
readCommentPart <- function(path, files = NULL) {

 
  # Read the csv files
  csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
  if (!is.null(files)) csv_files <- csv_files[basename(csv_files) %in% files]
  singles_csv <- lapply(csv_files, readTable, which_sep2 = c("view_range", "time_points", "time_point_paramcd"))
  csv_comment_part <- tibble::as_tibble(data.table::rbindlist(singles_csv, fill = TRUE))

  
  
  # Convert to suitable formats
  if (length(csv_files)) {
    if (!"time_point_paramcd" %in% colnames(csv_comment_part))
      csv_comment_part$time_point_paramcd <- list(character(0))
    
    if ("paramcd" %in% colnames(csv_comment_part)) {
      csv_comment_part$paramcd <- as.character(csv_comment_part$paramcd)
    }
      
    if ("color_by" %in% colnames(csv_comment_part))
      csv_comment_part$color_by <- as.character(csv_comment_part$color_by)
    
    if ("profile" %in% colnames(csv_comment_part)){
      csv_comment_part$profile <- as.character(csv_comment_part$profile)
    }
    
    csv_comment_part$edit_id[is.na(csv_comment_part$edit_id)] <- ""
    csv_comment_part$time_points[is.na( csv_comment_part$time_points)] <- list(numeric(0))
    csv_comment_part$time_points <- lapply(csv_comment_part$time_points, as.numeric)
    csv_comment_part$time_point_paramcd[is.na( csv_comment_part$time_point_paramcd)] <- list(character(0))
    csv_comment_part$time_point_paramcd <- lapply(csv_comment_part$time_point_paramcd, as.character)
    csv_comment_part$view_range[is.na( csv_comment_part$view_range)] <- list(numeric(0))
    csv_comment_part$view_range <- lapply(csv_comment_part$view_range, as.numeric)
    csv_comment_part$time <- fasttime::fastPOSIXct(csv_comment_part$time, "UTC")
    csv_comment_part$edit_time <- fasttime::fastPOSIXct(csv_comment_part$edit_time, "UTC")
  }

  # Read the rds files
  rds_files <- list.files(path, pattern = "*.rds", full.names = TRUE)
  if (!is.null(files)) rds_files <- rds_files[basename(rds_files) %in% files]
  singles_rds <- lapply(rds_files, readRDS)
  rds_comment_part <- tibble::as_tibble(data.table::rbindlist(singles_rds, fill = TRUE))

  # Bind the rds and csv data together
  dplyr::bind_rows(csv_comment_part, rds_comment_part)
}


#' Edit a comment
#'
#' @param comment_data An object initialised by \code{\link{initCommentData}}.
#' @param comment_id The id of the comment to change.
#' @param comment The new comment, if NULL the comment text is not changed.
#' @param regarding The updated regarding
#' @param paramcd The updated paramcd
#' @param time_points The new time points, if NULL the time points are not updated.
#' @param view_range The updated view_range, if NULL the view_range is not updated.
#'
#' @return The edited comment data
#' @export
writeEdit <- function(comment_data, comment_id, comment = NULL, regarding = NULL,
                      paramcd = NULL, time_points = NULL, view_range = NULL) {


  # get options from comment_data
  options <- attributes(comment_data)$options

  comment_setup <- attributes(comment_data)$comment_setup

  comment_row <- comment_data$id == comment_id

  if (!is.null(comment) && comment != "") {
    comment_data[comment_row, "comment"] <- comment
  }

  if (!is.null(regarding) && regarding != "") {
    comment_data[comment_row, "regarding"] <- regarding
  }
  
  if (!is.null(paramcd) && length(paramcd)) {
    comment_data[comment_row, "paramcd"][[1]] <- paramcd
  }
  
  if (!is.null(time_points) && length(time_points)) {
    comment_data[comment_row,  "time_points"][[1]]  <- list(time_points)
  }

  if (!is.null(view_range) && length(view_range)) {
    comment_data[comment_row,  "view_range"][[1]]  <- list(view_range)
  }

  # fill in information regarding the edit
  edit_id <- sample(1000:9999, 1)
  time <- clockTime()
  comment_data[comment_row, "edit_id"] <- edit_id
  comment_data[comment_row, "edit_time"] <- time

  # Create file name
  file_time <- gsub(" ", "_", gsub("[-:]", "", as.character(time)))
  filename <- sub("^comment", "comment_edit", paste(comment_id, file_time, edit_id, sep = "_"))


  comment_part <- comment_data[comment_row,
            setdiff(names(comment_data), c("reply", "status"))]


  # save the edit and update file_tracker
  if (options$save) {
    # get file_track to update the current files
    file_track <- attr(comment_data, "file_track")

    if (options$rds) {
      saveRDS(comment_part,
              file.path(comment_setup$folders["edits"], paste0(filename,  ".rds")))
      file_track[["edits"]] <- c(file_track[["edits"]], paste0(filename, ".rds"))
    } else {
      writeTable(comment_part,
                 file.path(comment_setup$folders["edits"], paste0(filename, ".csv")))
      file_track[["edits"]] <- c(file_track[["edits"]], paste0(filename, ".csv"))
    }
    attr(comment_data, "file_track") <- file_track
  }

  # The edited comment data is returned
  return(comment_data)
}


#' Read the comment part of an edited comment
#'
#' @param path Path the the folder to read from
#' @param files The files to read, if NULL all files in the directory is read
#'
#' @return A tibble with the edits
#' @export
readEdit <- function(path, files = NULL) {
  readCommentPart(path = path, files = files)
}


#' Write a reply to a comment
#'
#' @param comment_data An object initialised by \code{\link{initCommentData}}.
#' @param comment_id The id of the comment to change.
#' @param user The initials of the user replying.
#' @param reply Character with the reply.
#'
#' @return returns a tibble with the updated comment
#' @export
#' @importFrom tibble tibble
writeReply <- function(comment_data, comment_id,
                       user, reply) {

  # Get options from comment_data
  options <- attributes(comment_data)$options

  # Get comment setup from comment_data
  comment_setup <- attributes(comment_data)$comment_setup

  # Write the reply
  time   <- clockTime()
  reply_list <- list(list(user, time, reply))

  # Find the row to add the reply
  comment_row <- comment_data$id == comment_id

  # Add the reply to the list of replies
  comment_data[comment_row, "reply"][[1]][[1]] <-
    c(comment_data[comment_row, "reply"][[1]][[1]],
      reply_list)


  # Save the reply
  random <- round(runif(length(user), 1000, 9999))
  reply_id <- commentID(user = user, type = "reply", time, random)

  reply_data <- tibble::tibble(comment_id      = comment_id,
                               reply_id        = reply_id,
                               reply_edit_id   = "",
                               reply_edit_time = time,
                               reply           = reply_list)

  # update the base comment data
  attr(comment_data, "reply_data") <-
    dplyr::bind_rows(attr(comment_data, "reply_data"), reply_data)

  # save the edit and update file_tracker
  if (options$save) {
    # get file_track to update the current files
    file_track <- attr(comment_data, "file_track")

    file_name <- ifelse(options$rds, paste0(reply_id,  ".rds"), paste0(reply_id,  ".csv"))
    
    #dir.create(comment_setup$folders["replies"], showWarnings = FALSE, recursive = TRUE)
    
    if (options$rds) {
      saveRDS(reply_data,
              file.path(comment_setup$folders["replies"], file_name))
    } else {
      writeTable(reply_data,
                 file.path(comment_setup$folders["replies"], file_name),
                 which_sep3 = c("reply"))
    }
    # update file tracker
    file_track[["replies"]] <- c(file_track[["replies"]], file_name)
    attr(comment_data, "file_track") <- file_track
  }

  # return the updated data
  return(comment_data)
}

#' Read the replies
#'
#' @param path path to the replies
#' @param files The files to read, if NULL all files are read
#'
#' @return Returns a tibble with the read read replies.
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom fasttime fastPOSIXct
#' @importFrom data.table rbindlist
readReply <- function(path, files = NULL) {

  # Read the csv files
  csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
  if (!is.null(files)) csv_files <- csv_files[basename(csv_files) %in% files]
  singles_csv <- lapply(csv_files, readTable, which_sep3 = c("reply"))
  csv_reply <- tibble::as_tibble(data.table::rbindlist(singles_csv))

  # convert to suitable formats
  if (length(csv_files)) {
    csv_reply$reply_edit_id[is.na(csv_reply$reply_edit_id)] <- ""
    csv_reply$reply_edit_time <- fasttime::fastPOSIXct(csv_reply$reply_edit_time, "UTC")

    # convert the time to UTC
    editReplyTime <- function(x) {
        list(x[[1]], fasttime::fastPOSIXct(x[[2]], "UTC"), x[[3]])
    }

    csv_reply$reply <-
      lapply(csv_reply$reply, editReplyTime)
  }

  # Read the rds files
  rds_files <- list.files(path, pattern = "*.rds", full.names = TRUE)
  if (!is.null(files)) rds_files <- rds_files[basename(rds_files) %in% files]
  singles_rds <- lapply(rds_files, readRDS)
  rds_reply <- tibble::as_tibble(data.table::rbindlist(singles_rds))

  dplyr::bind_rows(csv_reply, rds_reply)
}

#' Update the status of a comment
#'
#' @param comment_data An object initialised by \code{\link{initCommentData}}.
#' @param comment_id The id of the comment to change.
#' @param user The initials of the user replying.
#' @param status Character with the status.
#'
#' @return A tibble with the updated status.
#' @export
#' @importFrom tibble tibble
writeStatus <- function(comment_data, comment_id,
                        user, status) {


  # Get options from comment_data
  options <- attributes(comment_data)$options

  # Get comment setup from comment_data
  comment_setup <- attributes(comment_data)$comment_setup

   # Get the row number
  comment_row <- comment_data$id == comment_id

  # Set the status
  comment_data[comment_row, "status"] <- status

  # create new file name
  time   <- clockTime()
  file_name  <- commentID(user = user, type = "status", time)

  # Assemble the status data
  status_part <-  tibble::tibble(comment_id       = comment_id,
                                 user             = user,
                                 status_edit_time = time,
                                 status           = status)


  # save the edit and update file_tracker
  if (options$save) {
    # get file_track to update the current files
    file_track <- attr(comment_data, "file_track")


    # Create file name
    file_time <- gsub(" ", "_", gsub("[-:]", "", as.character(time)))
    file_name <- sub("^comment", "status",
                    paste(comment_id, file_time, sample(1000:9999, 1), sep = "_"))
    file_name_comp <- ifelse(options$rds, paste0(file_name,  ".rds"), paste0(file_name,  ".csv"))

    # save the data
    if (options$rds) {
      saveRDS(status_part,
              file.path(comment_setup$folders["status"], file_name_comp))
    } else {
      writeTable(status_part,
                 file.path(comment_setup$folders["status"], file_name_comp))
    }
    # update file tracker
    file_track[["status"]] <- c(file_track[["status"]], file_name_comp)
    attr(comment_data, "file_track") <- file_track
  }

  # return the updated data
  return(comment_data)
}


#' Read status updates to comments
#'
#' @param path The path to the status updates
#' @param files the files t read. If NULL all files are read.
#'
#' @return Tibble with the status
#' @export
readStatus <- function(path, files = NULL) {

  # Read the csv files
  csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
  if (!is.null(files)) csv_files <- csv_files[basename(csv_files) %in% files]
  singles_csv <- lapply(csv_files, readTable)
  csv_status <- tibble::as_tibble(data.table::rbindlist(singles_csv))

  # convert to suitable formats
  if (length(csv_files)) {
    csv_status$status_edit_time <- fasttime::fastPOSIXct(csv_status$status_edit_time, "UTC")
  }

  # Read the rds files
  rds_files <- list.files(path, pattern = "*.rds", full.names = TRUE)
  if (!is.null(files)) rds_files <- rds_files[basename(rds_files) %in% files]
  singles_rds <- lapply(rds_files, readRDS)
  rds_status <- tibble::as_tibble(data.table::rbindlist(singles_rds))

  # bind the rds and csv data together
  dplyr::bind_rows(csv_status, rds_status)
}


#' Delete a comment
#'
#' @param comment_data An object initialised by \code{\link{initCommentData}}.
#' @param comment_id The id of the comment to change.
#' @param user The initials of the user that deletes the comment.
#'
#' @return A tibble with the updated comment data
#' @export
#'
deleteComment <- function(comment_data, comment_id, user) {

  # Get options from comment_data
  options <- attributes(comment_data)$options

  # Get comment setup from comment_data
  comment_setup <- attributes(comment_data)$comment_setup

  # Get the row number
  comment_row <- comment_data$id == comment_id

  # delete the comment
  comment_data <- comment_data[!comment_row, ]

  # create new file name
  time   <- clockTime()
  file_name  <- commentID(user = user, type = "com_delete", time)

  # Assemble the status data
  status_part <-  tibble::tibble(comment_id       = comment_id,
                                 user             = user,
                                 status_edit_time = time)


  # save the edit and update file_tracker
  if (options$save) {
    # get file_track to update the current files
    file_track <- attr(comment_data, "file_track")


    # Create file name
    file_time <- gsub(" ", "_", gsub("[-:]", "", as.character(time)))
    file_name <- sub("^comment", "com_delete",
                     paste(comment_id, file_time, sample(1000:9999, 1), sep = "_"))
    file_name_comp <- ifelse(options$rds, paste0(file_name,  ".rds"), paste0(file_name,  ".csv"))

    # save the data
    if (options$rds) {
      saveRDS(status_part,
              file.path(comment_setup$folders["com_delete"], file_name_comp))
    } else {
      writeTable(status_part,
                 file.path(comment_setup$folders["com_delete"], file_name_comp))
    }
    # update file tracker
    file_track[["com_delete"]] <- c(file_track[["com_delete"]], file_name_comp)
    attr(comment_data, "file_track") <- file_track
  }

  # return the updated data
  return(comment_data)
}

#' Read the comment deletions
#'
#' @param path path to the deletions
#' @param files The files to read, if NULL all files are read
#'
#' @return Returns a tibble with the read read replies.
#' @export
#' @importFrom tibble tibble as_tibble
#' @importFrom fasttime fastPOSIXct
#' @importFrom data.table rbindlist
readComDelete <- function(path, files = NULL) {

  # Read the csv files
  csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
  if (!is.null(files)) csv_files <- csv_files[basename(csv_files) %in% files]
  singles_csv <- lapply(csv_files, readTable)
  csv_delete <- tibble::as_tibble(data.table::rbindlist(singles_csv))
  
  # convert to suitable formats
  if (length(csv_files)) {
    csv_delete$status_edit_time <- fasttime::fastPOSIXct(csv_delete$status_edit_time, "UTC")
  }

   # Read the rds files
  rds_files <- list.files(path, pattern = "*.rds", full.names = TRUE)
  if (!is.null(files)) rds_files <- rds_files[basename(rds_files) %in% files]
  singles_rds <- lapply(rds_files, readRDS)
  rds_delete <- tibble::as_tibble(data.table::rbindlist(singles_rds))

  dplyr::bind_rows(csv_delete, rds_delete)
}


#' Read and assembe the comment data
#'
#' @param comment_setup comment_setup An object from
#'   \code{\link{setUpCommentFolders}}
#' @param remove_duplicates Should duplicates be removed. This is especially for
#'   edited comments where only the newest are kept in data if TRUE other wise all edits are kept
#' @param read_files The files to read.
#'
#' @return The current comment status
#' @export
readComments <- function(comment_setup, remove_duplicates = TRUE,
                         read_files = checkCommentFolders(comment_setup)) {


  if (any(sapply(read_files, length))) {
    # get comment_part data
    commentpart <- readCommentPart(path = comment_setup$folders["comments"],
                                   files = read_files$comments)

    # get edits
    edits <- readEdit(path = comment_setup$folders["edits"],
                      files = read_files$edits)

    # combine comments and edits
    comed <- dplyr::bind_rows(commentpart, edits)
    comment_data <- dplyr::arrange(comed, dplyr::desc(.data$edit_time))
    if (remove_duplicates)
      comment_data <- dplyr::distinct(comment_data, .data$id , .keep_all = TRUE)

    # get status
    status <- readStatus(path = comment_setup$folders["status"],
                         files = read_files$status)
    status <- dplyr::arrange(status, dplyr::desc(.data$status_edit_time))
    if (remove_duplicates)
      status <- dplyr::distinct(status, .data$comment_id , .keep_all = TRUE)

    comment_data <- dplyr::left_join(comment_data,
                                     status[, c("comment_id", "status")],
                                     by = c("id" = "comment_id"))

    # get replies
    if (length(dir(comment_setup$folders["replies"]))) {
      reply_data <- readReply(comment_setup$folders["replies"],
                           files = read_files$replies)
      replies <- reply_data[, c("comment_id", "reply")] %>%
        dplyr::group_by(.data$comment_id) %>%
        dplyr::summarise(reply = list(.data$reply))

      comment_data <- dplyr::left_join(comment_data, replies, by = c("id" = "comment_id"))

      comment_data$reply[unlist(lapply(comment_data$reply, is.null))] <- list(list())
    } else {
      reply_data <- NULL
      comment_data$reply <- rep(list(list()), nrow(comment_data))
    }

    # delete comments
    com_delete <- readComDelete(path = comment_setup$folders["com_delete"],
                                files = read_files$com_delete)

    if (nrow(com_delete))
      comment_data <- comment_data[comment_data$id %in%
                     setdiff(comment_data$id, com_delete$comment_id), ]

  } else {
    reply_data <- NULL
    comment_data <- tibble::tibble()
  }

  # Return the data
  attr(comment_data, "reply_data") <- reply_data
  attr(comment_data, "file_track") <- read_files

  attr(comment_data, "comment_setup") <- comment_setup

  return(comment_data)
}


#' set review status for a profile
#'
#' @param comment_data The comment data created by \cite{initCommentData}
#' @param user character with the initials of the user
#' @param profile character with the name of the profile
#'
#' @return Returns the updated comment_data
#' @export
reviewProfile <- function(comment_data, user, profile) {

  comment_setup <- attributes(comment_data)$comment_setup

  review_status <- attr(comment_data, "review_status")

  if (user %in% attr(comment_data, "users")) {
    file.create(file.path(comment_setup$review, tolower(user), gsub("/", "\u0023\u00A4\u0023", profile)),
                showWarnings = FALSE)

    review_status[[tolower(user)]] <- unique(c(review_status[[tolower(user)]], profile))

    attr(comment_data, "review_status") <- review_status
  }
  return(comment_data)
}

#' Get the review status
#'
#' @param comment_data The comment_data to update with review status
#'
#' @return returns a list with the users progress
#' @export
reviewStatus <- function(comment_data){

  # get the comment folders
  comment_setup <- attributes(comment_data)$comment_setup

  review_folders <- structure(file.path(comment_setup$review,
            tolower(attributes(comment_data)$users)),
            names = tolower(attributes(comment_data)$users))

  list.files.rename <- function(...) { gsub("\u0023\u00A4\u0023", "/", list.files(...)) }
  
  out <- lapply(review_folders, list.files.rename)
  
  return(out)
}


#' Initialise the comment data
#'
#' @param path The path where the comment setup should be initialised
#' @param users The user that are expected to review
#' @param update_flag_data  Should the \code{flag_data} be updated in the data
#' @param save_flag_data Should the flag_data be updated on the disk
#' @param save Should the elements of the comments be saved
#' @param rds Should the files be saved as rds. The rds files are much faster to
#'   work with and lower risk of errors. However, saving the files as csv has
#'   the benefit of being readable outside R.
#' @param folder_name Name of the folder to store the review status and comments
#' @param profiles Names of the profiles under review.
#'
#' @return The comment data
#' @export
initCommentData <- function(path = getwd(), users = "", profiles = "",
                            update_flag_data = FALSE, save_flag_data = TRUE,
                            save = TRUE, rds = TRUE,
                            folder_name = "comment_data") {

  # Set up the comment folder in the specified path
  comment_setup <- setUpCommentFolders(path = file.path(path, folder_name),
                                       users = users)

  # Read the current comments
  comment_data <- readComments(comment_setup)

  attr(comment_data, "users") <- users
  attr(comment_data, "profiles") <- profiles
  attr(comment_data, "review_status") <- reviewStatus(comment_data)

  # Add flag data as an attribute
  attr(comment_data, "flag_data") <- flagData(comment_setup, rds = rds)

  # Add options as an attribute
  attr(comment_data, "options") <- list(update_flag_data = update_flag_data,
                                      save_flag_data = save_flag_data,
                                      save = save, rds = rds)

  # Return the comments
  return(comment_data)
}



#' Check for files not currently in comment data
#'
#' @param comment_data The comment data object to check for updates
#' @param current_files optional, list of the the files currently loaded
#'
#' @return Return a list of the folders
#' @export
updateCommentDataCheck <- function(comment_data,
                               current_files = checkCommentFolders(attributes(comment_data)$comment_setup)) {

  data_files <- attributes(comment_data)$file_track

  missing <- lapply(structure(names(current_files), names = names(current_files)),
                    function(x) setdiff(current_files[[x]], data_files[[x]]))

  return(missing)
}



#' Update comment data according to current folder status
#'
#' @param comment_data The comment data to update
#'
#' @return The updated comment data
#' @export
updateCommentData <- function(comment_data) {

  comment_data_keep <- comment_data

  # check current files
  current_files = checkCommentFolders(attributes(comment_data)$comment_setup)

  # Files that needs to be updated
  read_files <- updateCommentDataCheck(comment_data, current_files)

  # get comment_setup
  comment_setup <- attributes(comment_data)$comment_setup

  # First, we add the new comments
  if (length(read_files$comments)) {
    commentpart <- readCommentPart(path = comment_setup$folders["comments"],
                                   files = read_files$comments)

    comment_data <- dplyr::bind_rows(comment_data, commentpart)
  }

  # Secondly, any new edits are added
  if (length(read_files$edits)) {
    edits <- readEdit(path = comment_setup$folders["edits"],
                      files = read_files$edits)

    # combine comments and edits
    comed <- dplyr::bind_rows(comment_data, edits)
    comed <- dplyr::arrange(comed, dplyr::desc(.data$edit_time))
    comment_data <- dplyr::distinct(comed, .data$id , .keep_all = TRUE)

    # get the old status and reply
    comment_data <-
      dplyr::left_join(comment_data[, setdiff(names(comment_data), c("status", "reply"))],
                       comment_data_keep[, c("id", "status", "reply")], by = "id")
  }

  # Thirdly, the status updates are accounted for
  if (length(read_files$status)) {
    status <- readStatus(path = comment_setup$folders["status"],
                         files = read_files$status) %>%
      dplyr::arrange(status, dplyr::desc(.data$status_edit_time)) %>%
      dplyr::distinct(status, .data$comment_id , .keep_all = TRUE)


    comment_data[match(status$comment_id, comment_data$id), "status"] <- status$status
  }

  # Fourth, new replies are added
  if (length(read_files$replies)) {
    replies <- readReply(path <- comment_setup$folders["replies"],
                         files = read_files$replies)

    # insert old replies, usefull for when edits of replies are allowed.
    old_replies <- attributes(comment_data_keep)$reply_data
    if (!is.null(old_replies))
      replies <- dplyr::bind_rows(old_replies, replies)

    replies <- replies[, c("comment_id", "reply")] %>%
      dplyr::group_by(.data$comment_id) %>%
      dplyr::summarise(reply = list(.data$reply))

    comment_data <- dplyr::left_join(comment_data[,setdiff(names(comment_data), c("reply"))],
                                     replies, by = c("id" = "comment_id"))
  }

  # fifth deleted comments are removed
  if (length(read_files$com_delete)) {
    com_delete <- readComDelete(path = comment_setup$folders["com_delete"],
                                files = read_files$com_delete)


    comment_data <- comment_data[comment_data$id %in%
                   setdiff(comment_data$id, com_delete$comment_id), ]

  }


  # reassign the old attributes
  new_attributes <-
    attributes(comment_data_keep)[setdiff(names(attributes(comment_data_keep)),
                                          c("names", "row.names", "class" ))]


  comment_data <- attributeAssign(comment_data, new_attributes)

  # update the file_tracker
  attr(comment_data, "file_track") <- current_files

  comment_data$reply[unlist(lapply(comment_data$reply, is.null))] <- list(list())

  # return the updated comment data
  class(comment_data) <- c("comments",  class(comment_data))
  return(comment_data)
}


#system.time({
#path <- NNR::pDrive("nn1436/nn1436-4226/current/share/data/reactive_pkpd")

# path <- "c:/test/csv"
#
# comment_data <- initCommentData(path = path, users = c("SFFL", "CDN", "KFSM"),
#                                rds = FALSE,
#                                folder_name = "comment_data_new")
#
# comment_data_clean <- comment_data
#
#
# initials <- "sffl"
# profile <- "profile_1"
# regarding <-  "Excl. data point(s)"
# comment = "Unphysoligical"
# selected_points = numeric(0)
# view_range = c(43, 45)
#
#
# comment_data <-  writeComment(comment_data, user = initials, profile = profile,
#              regarding = regarding,
#              comment = comment, time_points = selected_points,
#              view_range = view_range)
#
# comment_data <- writeComment(comment_data, user = "sffl", profile = profile,
#                              regarding = "Excl. data point(s)",
#                              comment = "Unphysoligical", time_points = c(34, 56),
#                              view_range = c(43, 45))
#
#
# comment_data <- writeComment(comment_data, user = "sffl", profile = "profile_1", regarding = "Excl. data point(s)",
#                              comment = "Unphysoligical", time_points = c(34),
#                              view_range = c(43, 45))
#
#
# comment_data <- writeComment(comment_data, user = "sffl", profile = "profile_2",
#                              regarding = "Excl. data point(s)",
#                              comment = "Unphysoligical", time_points = c(34, 56),
#                              view_range = c(43, 45))
#
#
# comment_data <- writeComment(comment_data, user = "sffl", profile = "profile_3", regarding = "Excl. data point(s)",
#                              comment = "Unphysoligical", time_points = numeric(0),
#                              view_range = c(43, 45))
#
#
# comment_data <- writeEdit(comment_data, comment_id = comment_data$id[1],
#                           comment = "An edited comment")
#
#
# comment_data <- writeEdit(comment_data, comment_id = comment_data$id[2],
#                           time_points = c(45, 46, 47))
#
#
#
# comment_data <- writeReply(comment_data, comment_id = comment_data$id[4],
#                            user = "AIKP", reply = "some reply")
#
# comment_data <- writeReply(comment_data, comment_id = comment_data$id[4],
#                            user = "KFSM", reply = "some other reply")
#
# comment_data <- writeReply(comment_data, comment_id = comment_data$id[4],
#                            user = "KRRD", reply = "some other reply")
#
# comment_data <- writeStatus(comment_data, comment_id = comment_data$id[2],
#                             user = "KFSM", status = "Approved")
#
#
# comment_data <- deleteComment(comment_data, comment_id = comment_data$id[4],
#                            user = "AIKP")
#
# comment_data <- deleteComment(comment_data, comment_id = comment_data$id[4],
#                               user = "AIKP")
#
#
#
#
# comment_data_old <- comment_data
#
# comment_data <- writeComment(comment_data, user = "sffl", profile = "profile_3",
#                              regarding = "Excl. data point(s)",
#                              comment = "Unphysoligical", time_points = c(34, 56),
#                              view_range = c(43, 45))
#
#
# comment_data <- writeReply(comment_data, comment_id = comment_data$id[4],
#                            user = "KFSM", reply = "some other reply")
#
# comment_data <- writeReply(comment_data, comment_id = comment_data$id[4],
#                            user = "KRRD", reply = "some other reply")
#
#
# comment_data <- writeEdit(comment_data, comment_id = comment_data$id[2],
#                           time_points = c(45, 46, 47, 48))
#
# })
#
# # check for updates
# updateCommentDataCheck(comment_data_old)
#
# comment_data_new <- comment_data
# comment_data <- comment_data_old
#
#
# system.time(updateCommentData(comment_data))
#
# updateCommentData(comment_data_clean)
#
# up_data <- updateCommentData(comment_data)
#
# names <- sapply(comment_data_new, class)[sapply(comment_data_new, class) != "list"]
#
# dplyr::all_equal(comment_data_new[match(up_data$id, comment_data_new$id),names(names)],
#                  up_data[, names(names)])
#
#
# names <- sapply(comment_data_new, class)[sapply(comment_data_new, class) == "list"]
#
# all.equal(unlist(comment_data_new[match(up_data$id, comment_data_new$id), "view_range"]),
#           unlist(up_data[,"view_range"]))
#
# all.equal(unlist(comment_data_new[match(up_data$id, comment_data_new$id), "time_points"]),
#           unlist(up_data[,"time_points"]))
#
# # We tolerate a one second deviation for the time points For rds we complete equality
# all.equal(comment_data_new[match(up_data$id, comment_data_new$id), ]$reply,
#           up_data$reply)#, tolerance = 1)





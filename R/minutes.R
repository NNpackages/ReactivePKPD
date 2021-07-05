
#' Create header for minutes
#'
#' @param x The word minutes template 
#' @param project The project number
#' @param trial The Trial number 
#' @param participants The participants
#' @param excused Those excused
#' @param ref The minute keeper
#' @param title Titles for the header
#' @param date The date for the minutes
#'
#' @return An object of class rdocx
#' @export 
#' @importFrom officer headers_replace_all_text
#' @importFrom glue glue_collapse
headerMinutes <- function(x, 
                          project, trial, 
                          participants = "", excused = "", ref = "",
                          title = c(paste0("NN", project, "-", trial), 
                                    "Profile review meeting"),
                          date = format(Sys.Date(), "%d %b %Y")) {

  names(title) <- paste0("#header_title", seq_along(title))

  for (i in seq_along(title)) {
    x <- x %>% officer::headers_replace_all_text(names(title)[i], title[i], warn = FALSE)
  }
  
  x <- x %>% officer::headers_replace_all_text("#participants", 
                                      glue::glue_collapse(participants, sep = ", ", last = " and "), warn = FALSE)
  
  x <- x %>% officer::headers_replace_all_text("#excused", 
                                      glue::glue_collapse(excused, sep = ", ", last = " and "), warn = FALSE)
  
  x <- x %>% officer::headers_replace_all_text("#ref", 
                                               glue::glue_collapse(ref, sep = ", ", last = " and "), warn = FALSE)
  
  x <- x %>% officer::headers_replace_all_text("#date", date, warn = FALSE)
  
  return(x)
}



#' Create minutes for a PK meeting
#'
#' @param project The project number
#' @param trial The Trial number 
#' @param participants The participants
#' @param excused Those excused
#' @param ref The minute keeper
#' @param comments The minutes 
#' @param overall_desc over all description for minutes
#'
#' @return An object of class rdocx
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # Get example comments
#' data(pk_comments)
#' 
#' # Create the minutes
#' doc <- minutes(project = "1436", trial = "4225", 
#'                participants = c("SFFL", "LSBD"),
#'                excused = c(""),
#'                ref = "",
#'                comments = pk_comments,
#'                overall_desc = "Overall the PK profiles looks good")
#' 
#' # Write to a temp file
#' file <- tempfile(fileext = ".docx") 
#' print(doc, target = file)
#' 
#' # Open the file
#' system(paste("open", file))
#' 
#' # Delete the temporary file again
#' unlink(file)
#' 
#' }
#' 
#' @importFrom officer read_docx
#' @importFrom glue glue
#' 
minutes <- function(project, trial, 
                    participants = "",
                    excused = "",
                    ref = "",
                    comments,
                    overall_desc = NULL) {
  
  
  if (is.null(participants)) participants = ""
  if (is.null(excused)) excused = ""
  if (is.null(ref)) ref = ""
  
  
  # read the minutes template
  my_empty_min <- officer::read_docx(system.file(file.path("word_template",
                  "template_minutes.docx"), package = "NNPKPD"))
  
  
  # add header info
  doc <- my_empty_min %>% headerMinutes(project = project, trial = trial, 
                                        participants = participants,
                                        excused = excused,
                                        ref = ref)  
  
  # add title 
  title <- glue::glue("NN{project}-{trial}: PK profile review meeting")
  
  doc %<>% officer::body_remove() 
  doc %<>% officer::body_add_par(title, style = "heading 1")
  
  if (!is.null(overall_desc)) {
    doc %<>% officer::body_add_par("Overall decisions:", style = "heading 2")
    doc %<>% officer::body_add_par(overall_desc, style = "Normal")
  }

  
  replies <- tidyr::unnest(comments, .data$reply, .preserve = .data$time_points)
  #replies <- unnest[sapply(unnest$reply, length), ]
  
  replies$user <-  unlist(sapply(replies$reply, function(x) x[1]))  
  replies$comment <- unlist(sapply(replies$reply, function(x) x[3]))  
  if (nrow(replies))
    replies$status <- "Reply"
  
  comments2 <- dplyr::bind_rows(comments, replies) %>% dplyr::arrange(.data$regarding, .data$profile, .data$id)
  
  colnames(comments2)[match(c("profile", "time_points", "comment", "status"), colnames(comments2)) ] <- 
    c("Profile", "Nominal time", "Comment", "Status")
  
  # Add comments that regards the whole profile
  
  whole_prof <- comments2[sapply(comments2$`Nominal time`, length) == 0, ]
  if (any(whole_prof$Status == "Reply"))
    whole_prof[whole_prof$Status == "Reply", ]$Profile <- ""
  
  
  
  if (nrow(whole_prof)) {
    doc %<>% officer::body_add_par("Comments regarding the entire PK profile", style = "heading 2")
    
    for (regards in unique(whole_prof$regarding)) {
      wh_reg <- whole_prof[whole_prof$regarding == regards, ]
  
      doc %<>% officer::body_add_par(regards, style = "heading 3")
      
      table <- wh_reg[, c("Profile", "Comment", "Status") ]
      
      flex <- flextable::flextable(table, cwidth = c(3.43, 10.43, 2.51)/2.54) %>% 
        flextable::font(fontname = "Cambria", part = "all") %>% 
        flextable::align(align = "right", part = "all") %>%
        flextable::bold(part = "header")
      doc %<>% flextable::body_add_flextable(flex)
    }
  }
  
  # Add comments that regards specific points
  
  spec_point <- comments2[sapply(comments2$`Nominal time`, length) != 0 , ]
  if (any(spec_point$Status == "Reply")) {
    spec_point[spec_point$Status == "Reply", ]$"Nominal time" <- 
      lapply(spec_point[spec_point$Status == "Reply", ]$"Nominal time", function(x) c(numeric(0)))
    spec_point[spec_point$Status == "Reply", "Profile"] <- ""
  }
  
  if (nrow(spec_point)) {
    doc %<>% officer::body_add_par("Comments regarding specific time points for a PK profile", style = "heading 2")
    
    for (regards in unique(spec_point$regarding)) {
      sp_reg <- spec_point[spec_point$regarding == regards, ]
      
      doc %<>% officer::body_add_par(regards, style = "heading 3")
      
      table <- sp_reg[, c("Profile", "Nominal time", "Comment", "Status") ]
      
      flex <- flextable::flextable(table, cwidth =  c(3.43, 3.43, 7, 2.51)/2.54) %>% 
        flextable::font(fontname = "Cambria", part = "all") %>% 
        flextable::align(align = "right", part = "all") %>%
        flextable::bold(part = "header")
      doc %<>% flextable::body_add_flextable(flex)
    }
  }
  
  # return 
  return(doc)
}


#' Create minutes for a PD meeting
#'
#' @param project The project number
#' @param trial The Trial number 
#' @param participants The participants
#' @param excused Those excused
#' @param ref The minute keeper
#' @param comments The minutes 
#' @param overall_desc over all description for minutes
#'
#' @return An object of class rdocx
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' # Get example comments
#' data(pd_comments)
#' 
#' # Create the minutes
#' doc <- minutes(project = "1436", trial = "4225", 
#'                participants = c("SFFL", "LSBD"),
#'                excused = c(""),
#'                ref = "",
#'                comments = pd_comments,
#'                overall_desc = "Overall the PD profiles looks good")
#' 
#' # Write to a temp file
#' file <- tempfile(fileext = ".docx") 
#' print(doc, target = file)
#' 
#' # Open the file
#' system(paste("open", file))
#' 
#' # Delete the temporary file again
#' unlink(file)
#' 
#' }
#' 
#' @importFrom officer read_docx
#' @importFrom glue glue
#' 
minutesPD <- function(project, trial, 
                    participants = "",
                    excused = "",
                    ref = "",
                    comments,
                    overall_desc = NULL) {
  
  
  if (is.null(participants)) participants = ""
  if (is.null(excused)) excused = ""
  if (is.null(ref)) ref = ""
  
  
  # read the minutes template
  my_empty_min <- officer::read_docx(system.file(file.path("word_template",
                                                           "template_minutes.docx"), package = "NNPKPD"))
  
  
  # add header info
  doc <- my_empty_min %>% headerMinutes(project = project, trial = trial, 
                                        participants = participants,
                                        excused = excused,
                                        ref = ref)  
  
  # add title 
  title <- glue::glue("NN{project}-{trial}: PD profile review meeting")
  
  doc %<>% officer::body_remove() 
  doc %<>% officer::body_add_par(title, style = "heading 1")
  
  if (!is.null(overall_desc)) {
    doc %<>% officer::body_add_par("Overall decisions:", style = "heading 2")
    doc %<>% officer::body_add_par(overall_desc, style = "Normal")
  }
  
  
  replies <- tidyr::unnest(comments, .data$reply, .preserve = .data$time_points)
  #replies <- unnest[sapply(unnest$reply, length), ]
  
  replies$user <-  unlist(sapply(replies$reply, function(x) x[1]))  
  replies$comment <- unlist(sapply(replies$reply, function(x) x[3]))  
  if (nrow(replies))
    replies$status <- "Reply"
  
  comments2 <- dplyr::bind_rows(comments, replies) %>% dplyr::arrange(.data$regarding, .data$profile, .data$id)
  
  colnames(comments2)[match(c("profile", "comment", "status"), colnames(comments2)) ] <- 
    c("Profile", "Comment", "Status")
  
  # Add comments that regards the whole profile
  
  whole_prof <- comments2[comments2$time_type == "profile" | comments2$time_type == "", ]
  
  if (any(whole_prof$Status == "Reply"))
    whole_prof[whole_prof$Status == "Reply", ]$Profile <- ""
  
  
  
  if (nrow(whole_prof)) {
    doc %<>% officer::body_add_par("Comments regarding the entire PD profile", style = "heading 2")
    
    for (regards in unique(whole_prof$regarding)) {
      wh_reg <- whole_prof[whole_prof$regarding == regards, ]
      
      doc %<>% officer::body_add_par(regards, style = "heading 3")
      
      table <- wh_reg[, c("Profile", "Comment", "Status") ]
      
      flex <- flextable::flextable(table, cwidth =  c(3.43, 10.43, 2.51)/2.54) %>% 
        flextable::font(fontname = "Cambria", part = "all") %>% 
        flextable::align(align = "right", part = "all") %>%
        flextable::bold(part = "header")
      doc %<>% flextable::body_add_flextable(flex)
    }
  }
  
  
  # Add comments that regards specific time intervals
  
  spec_int <- comments2[comments2$time_type == "interval" & !is.na(comments2$time_type), ] %>% dplyr::rowwise() %>%
    dplyr::mutate(time = ifelse(length(.data$time_points),
                                glue::glue_collapse(round(unlist(.data$time_points), 2), sep = " to "),
                                ""))
  
  colnames(spec_int)[match(c("time"), colnames(spec_int)) ] <- c("Time interval")
  
  if (any(spec_int$Status == "Reply")) {
    spec_int[spec_int$Status == "Reply", "Time interval"] <- ""
    spec_int[spec_int$Status == "Reply", "Profile"] <- ""
  }
  
  if (nrow(spec_int)) {
    doc %<>% officer::body_add_par("Comments regarding specific time intervals for a PD profile", style = "heading 2")
    
    for (regards in unique(spec_int$regarding)) {
      sp_reg <- spec_int[spec_int$regarding == regards, ]
      
      doc %<>% officer::body_add_par(regards, style = "heading 3")
      
      table <- sp_reg[, c("Profile", "Time interval", "Comment", "Status") ]
      
      flex <- flextable::flextable(table, cwidth =  c(3.43, 3.43, 7, 2.51)/2.54) %>% 
        flextable::font(fontname = "Cambria", part = "all") %>% 
        flextable::align(align = "right", part = "all") %>%
        flextable::bold(part = "header")
      doc %<>% flextable::body_add_flextable(flex)
    }
  }
  
  
  # Add comments that regards specific points
  
  spec_point <- comments2[comments2$time_type == "point" & !is.na(comments2$time_type), ] %>% dplyr::rowwise() %>%
    dplyr::mutate(time = ifelse(length(.data$time_points),
                                glue::glue_collapse(round(unlist(.data$time_points), 2), sep = ", ", last = " and "),
                                ""))
  
  colnames(spec_point)[match(c("time"), colnames(spec_point)) ] <- c("Time")
  
  if (any(spec_point$Status == "Reply")) {
    spec_point[spec_point$Status == "Reply", "Time"] <- ""
    spec_point[spec_point$Status == "Reply", "Profile"] <- ""
  }
  
  if (nrow(spec_point)) {
    doc %<>% officer::body_add_par("Comments regarding specific time points for a PD profile", style = "heading 2")
    
    for (regards in unique(spec_point$regarding)) {
      sp_reg <- spec_point[spec_point$regarding == regards, ]
      
      doc %<>% officer::body_add_par(regards, style = "heading 3")
      
      table <- sp_reg[, c("Profile", "Time", "Comment", "Status") ]
      
      flex <- flextable::flextable(table, cwidth =  c(3.43, 3.43, 7, 2.51)/2.54) %>% 
        flextable::font(fontname = "Cambria", part = "all") %>% 
        flextable::align(align = "right", part = "all") %>%
        flextable::bold(part = "header")
      doc %<>% flextable::body_add_flextable(flex)
    }
  }
  
  
  # return 
  return(doc)
}




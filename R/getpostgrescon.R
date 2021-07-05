
#' open connection to database
#'
#' @param dw A list with connection details 
#'
#' @return A database connection
#' @export 
#'
#' @examples
#' \dontrun{
#' # open connection in accordance with details stored in datawarehouse
#' dw <- config::get("datawarehouse")
#' con <- getPostgresCon(dw)
#' }
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
getPostgresCon <- function(dw){
  return(dbConnect(RPostgres::Postgres(),
                   dbname = dw$dbname, 
                   host = dw$host,
                   port = dw$port,
                   user = dw$user,
                   password = dw$pwd)
  )
}

showTable <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  result <- dbGetQuery(conn, paste("SELECT * FROM ", name))
  if (name == "Students") {
    result$date <- as.Date(result$date, origin = "1970-01-01")
    result$confirmed <- as.logical(result$confirmed)
  }
  result
}

editTable <- function(name) {
  table <- showTable(name)
  table <- edit(table)
  replaceTable(name, table)
  invisible(table)
}


showTable <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  extra <- ""
  if (tolower(name) == "students")
    extra <- "ORDER BY name"
  result <- dbGetQuery(conn, paste("SELECT * FROM ", name, extra))
  if (tolower(name) == "students") {
    result$date <- as.Date(result$date, origin = "1970-01-01")
    result$confirmed <- as.logical(result$confirmed)
  }
  result
}

editTable <- function(name) {
  table <- showTable(name)
  if (name == "Students") {
    table$date <- as.character(table$date)
  }
  table <- edit(table)
  replaceTable(name, table)
  invisible(table)
}

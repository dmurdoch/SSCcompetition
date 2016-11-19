
showTable <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  result <- dbGetQuery(conn, paste("SELECT * FROM ", name))
  if (is.numeric(result$date))  # FIXME: find a better test!
    result$date <- as.Date(result$date, origin = "1970-01-01")
  result
}

editTable <- function(name) {
  table <- showTable(name)
  if (is.numeric(table$date))  # FIXME: find a better test!
    table$date <- as.Date(table$date, origin = "1970-01-01")
  table <- edit(table)
  replaceTable(name, table)
  invisible(table)
}

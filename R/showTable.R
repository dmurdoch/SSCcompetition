
showTable <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  result <- dbGetQuery(conn, paste("SELECT * FROM ", name))
  if (is.numeric(result$date))  # FIXME: find a better test!
    result$date <- as.Date(result$date, origin = "1970-01-01")
  result
}

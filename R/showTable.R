showTable <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  dbGetQuery(conn, paste("SELECT * FROM ", name))
}

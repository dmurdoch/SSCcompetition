newStudent <- function(name, affiliation = "", date = Sys.Date()) {
  df <- data.frame(name = name, affiliation = affiliation,
             date = date)
  conn <- getConn()
  RSQLite::dbWriteTable(conn, "Students", value = df, append =TRUE)
  doneWith(conn)
}

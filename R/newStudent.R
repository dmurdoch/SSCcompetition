newStudent <- function(name, affiliation = "", date = Sys.Date()) {
  df <- data.frame(name = name, affiliation = affiliation,
                   date = as.numeric(date))
  conn <- getConn()
  dbWriteTable(conn, "Students", value = df, append =TRUE)
  doneWith(conn)
}

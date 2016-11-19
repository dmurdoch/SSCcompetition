replaceTable <- function(table, df) {
  if (table == "Students")
    df$date <- as.numeric(df$date)
  conn <- getConn()
  dbWriteTable(conn, table, value = df, overwrite = TRUE)
  doneWith(conn)
}

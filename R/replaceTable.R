replaceTable <- function(table, df) {
  df$date <- as.numeric(df$date)
  conn <- getConn()
  dbWriteTable(conn, table, value = df, overwrite = TRUE)
  doneWith(conn)
}

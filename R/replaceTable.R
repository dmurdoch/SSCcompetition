replaceTable <- function(table, df) {
  if (table == "Students")
    df$date <- as.numeric(df$date)
  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, table, value = df, overwrite = TRUE)
}

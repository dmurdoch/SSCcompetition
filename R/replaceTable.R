replaceTable <- function(table, df) {
  if (tolower(table) == "students") {
    df$date <- as.numeric(as.Date(df$date))
    df$confirmed <- as.numeric(as.logical(df$confirmed))
  }
  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, table, value = df, overwrite = TRUE)
}

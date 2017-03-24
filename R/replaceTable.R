replaceTable <- function(table, df) {
  if (tolower(table) == "students") {
    df$date <- as.numeric(as.Date(df$date))
    df$confirmed <- as.numeric(as.logical(df$confirmed))
  } else if (tolower(table) == "sessions") {
    df$datetime <- as.numeric(as.POSIXct(df$datetime, tz = "CST6CDT", origin = "1970-01-01"))
    df$contributed <- as.numeric(as.logical(df$contributed))
  }
  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, table, value = df, overwrite = TRUE)
}

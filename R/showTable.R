
showTable <- function(name, where = NA, orderby = NA, params) {
  conn <- getConn()
  on.exit(doneWith(conn))
  extra <- ""
  if (!is.na(where))
    extra <- paste(extra, "WHERE", where)
  if (missing(orderby) && tolower(name) == "students")
    orderby <- "lower(name)"
  if (!is.na(orderby))
    extra <- paste(extra, "ORDER BY", orderby)
  query <- paste("SELECT * FROM ", name, extra)
  if (missing(params))
    result <- dbGetQuery(conn, query)
  else
    result <- dbGetQuery(conn, query, param = params)
  if (tolower(name) == "students") {
    result$date <- as.Date(result$date, origin = "1970-01-01")
    result$confirmed <- as.logical(result$confirmed)
  } else if (tolower(name) == "sessions") {
    result$datetime <- as.POSIXct(result$datetime, origin = "1970-01-01",
                                  tz = "CST6CDT")
    result$contributed <- as.logical(result$contributed)
  }
  result
}

editTable <- function(name) {
  table <- showTable(name)
  if (tolower(name) == "students") {
    table$date <- as.character(table$date)
  } else if (tolower(name) == "sessions") {
    table$datetime <- as.character(table$datetime)
  }
  table <- edit(table)
  replaceTable(name, table)
  invisible(table)
}

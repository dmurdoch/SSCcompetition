newStudent <- function(name, affiliation = "",
                       date = Sys.Date(), title = NA_character_,
                       abstract = NA_character_, confirmed = FALSE,
                       email = "",
                       summaryFile = NA_character_,
                       datetime = NA, sessionId = NA) {
  idnum <- getID(name, unique = FALSE)
  if (length(idnum)) {
    warning("Student ", dQuote(name), " already in table, not added.")
    return()
  }

  idnum <- newIdnum("Students")
  date <- as.Date(date)
  if (!is.na(datetime))
    datetime <- as.POSIXct(datetime, tz = "CST6CDT")

  df <- data.frame(idnum = idnum, name = name, affiliation = affiliation,
                   date = as.numeric(date),
                   title = as.character(title),
                   abstract = as.character(abstract),
                   confirmed = as.logical(confirmed),
                   summaryFile = as.character(summaryFile),
                   email = as.character(email),
                   stringsAsFactors = FALSE,
                   datetime = as.numeric(datetime),
                   sessionId = as.numeric(sessionId))
  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, "Students", value = df, append =TRUE)
}

newIdnum <- function(table) {
  conn <- getConn()
  on.exit(doneWith(conn))
  id <- try(dbGetQuery(conn, paste("SELECT max(idnum) as max from", table, ";")))
  if (inherits(id, "try-error"))
    id <- 1
  else
    id <- id$max + 1
  id
}

getID <- function(pattern, table = c("Students", "Judges", "Sessions"),
                  unique = TRUE) {
  table <- match.arg(table)
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- sqlInterpolate(conn,
                          paste("select idnum from", table, "where name like ?pattern"),
                          pattern = pattern)
  results <- dbGetQuery(conn, query)
  if (nrow(results) != 1 && unique)
    stop(nrow(results), " records match pattern ", dQuote(pattern))
  results$idnum
}


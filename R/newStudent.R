newStudent <- function(name, affiliation = "", date = Sys.Date()) {
  idnum <- getID(name, unique = FALSE)
  if (length(idnum)) {
    warning("Student ", dQuote(name), " already in table, not added.")
    return()
  }

  idnum <- newIdnum("Students")
  df <- data.frame(idnum = idnum, name = name, affiliation = affiliation,
                   date = as.numeric(date))
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


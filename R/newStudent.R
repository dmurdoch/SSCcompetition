newStudent <- function(name, affiliation = "", date = Sys.Date()) {
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

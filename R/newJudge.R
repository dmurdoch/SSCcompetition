newJudge <- function(name, expertise, affiliation) {
  conn <- getConn()
  id <- try(dbGetQuery(conn, "SELECT max(idnum) as max from Judges;"))
  if (inherits(id, "try-error"))
    id <- 1
  else
    id <- id$max + 1
  df <- data.frame(name = name, idnum = id, expertise = expertise, affiliation = affiliation)

  dbWriteTable(conn, "Judges", value = df, append =TRUE)
  doneWith(conn)
}

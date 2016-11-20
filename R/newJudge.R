newJudge <- function(name, expertise, affiliation) {
  id <- newIdnum("Judges")
  df <- data.frame(name = name, idnum = id, expertise = expertise, affiliation = affiliation)

  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, "Judges", value = df, append =TRUE)
}

newJudge <- function(name, expertise, affiliation, email, note = "") {
  idnum <- getID(name, "Judges", unique = FALSE)
  if (length(idnum)) {
    warning("Judge ", dQuote(name), " already in table, not added.")
    return()
  }
  id <- newIdnum("Judges")
  df <- data.frame(name = name, idnum = id, expertise = expertise, affiliation = affiliation,
                   email = email, note = note)

  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, "Judges", value = df, append =TRUE)
}

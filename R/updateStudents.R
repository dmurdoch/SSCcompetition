updateStudents <- function(db) {
  conn <- getConn()
  on.exit(doneWith(conn))
  for (i in seq_len(nrow(db))) {
    old <- showTable("Students", where = paste("idnum =", db$idnum[i]))
    if (!nrow(old))
      stop("idnum ", db$idnum, " not found.")
    if (nrow(old) > 1)
      stop("Multiple records with idnum=", db$idnum)
    newnames <- names(db)
    newval <- is.na(old[,newnames]) | db[i, newnames] != old[,newnames]
    if (any(newval)) {
      sets <- paste0(newnames[newval], " = :", newnames[newval], collapse = ", ")
      query <- paste("UPDATE Students SET", sets, "WHERE idnum=", db$idnum[i])
      dbGetQuery(conn, query, params = db[i, newval])
    }
  }
}

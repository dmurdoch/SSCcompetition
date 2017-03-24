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

getStudent <- function(...) {
  conn <- getConn()
  on.exit(doneWith(conn))
  names <- names(list(...))
  values <- list(...)
  query <- paste("SELECT * FROM Students WHERE", paste0(names, " = :", names))
  dbGetQuery(conn, query, params = values)
}

loadTiming <- function(csv = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/ContributedPresentations-Schedule-final-2.csv") {
  talks <- read.csv(csv, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(talks))) {
    talk <- talks[i,]
    if (talk$Type.of.Presentation == "Poster")
      talk$Session <- "Poster Session"
    session <- getID(talk$Session, "Sessions")
    if (nchar(talk$Date))
      datetime <- as.POSIXct(paste(talk$Date, talk$SpeakerStart), tz="CST6CDT")
    else
      datetime <- NA
    student <- getStudent(submission = talk$Node.ID)
    if (nrow(student)) {
      student$datetime <- datetime
      student$session <- session
      updateStudents(student[,c("idnum", "datetime", "session")])
    }
  }
}

assignJudges <- function(student, judge1Id, judge2Id) {
  len <- max(length(student), length(judge1Id), length(judge2Id))
  student <- rep(student, length.out=len)
  judge1Id <- rep(judge1Id, length.out=len)
  judge2Id <- rep(judge2Id, length.out=len)
  conn <- getConn()
  on.exit(doneWith(conn))
  for (i in seq_len(len)) {
    studentRec <- getTalks(student = student[i])
    if (nrow(studentRec) > 1)
      stop("Multiple student matches:", paste(studentRec$name, collapse=","))
    if (nrow(studentRec) == 0)
      stop("No student match to ", student[i])
    query <- "update Students set judge1Id = ?judge1, judge2Id = ?judge2
                     where name like ?student"
    query <- sqlInterpolate(conn, query, student = student,
                                         judge1 = judge1Id,
                                         judge2 = judge2Id)
    dbGetQuery(conn, query)
  }
}

getTalks <- function(student, judgeId, sessionId) {
  conds <- character()
  interp <- list()
  if (!missing(student)) {
    conds <- c(conds, "Students.name like ?student")
    interp$student <- student
  }
  if (!missing(judgeId)) {
    conds <- c(conds, "(judge1Id = ?judgeId or judge2Id = ?judgeId)")
    interp$judgeId <- judgeId
  }
  if (!missing(sessionId)) {
    conds <- c(conds, "sessionId = ?sessionId")
    interp$sessionId <- sessionId
  }
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- "select Students.name as studentName, Students.affiliation as studentAffiliation,
                   date as dateReceived, Judges.name as judgeName,
                   expertise, Judges.affiliation as judgeAffiliation from Students, Judges"
  if (length(conds)) {
    query <- paste(query, "where", paste(conds, collapse = " and "))
    query <- do.call(DBI::sqlInterpolate,
                     c(list(conn, query), interp))
  }
  result <- dbGetQuery(conn, query)
  result$dateReceived <- as.Date(result$dateReceived, origin = "1970-01-01")
  result
}

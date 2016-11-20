assignJudges <- function(studentID, judgeID, append = TRUE) {
  len <- max(length(studentID), length(judgeID))
  studentID <- rep(studentID, length.out=len)
  judgeID <- rep(judgeID, length.out=len)
  conn <- getConn()
  on.exit(doneWith(conn))
  if (!append) {
    students <- unique(studentID)
    for (i in seq_along(students)) {
      query <- sqlInterpolate(conn,
                              "delete from Judging where studentId = ?student",
                              student = students[i])
      dbGetQuery(conn, query)
    }
  }
  for (i in seq_len(len)) {
    query <- sqlInterpolate(conn,
                            "select * from Judging where studentId = ?student and
                             judgeId = ?judge",
                            student = studentID[i], judge = judgeID[i])
    existing <- dbGetQuery(conn, query)
    if (!nrow(existing)) {
      query <- sqlInterpolate(conn,
                              "insert into Judging (StudentId, JudgeId)
                                values (?student, ?judge)",
                              student = studentID[i], judge = judgeID[i])
      dbGetQuery(conn, query)
    }
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

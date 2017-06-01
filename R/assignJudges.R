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
    conds <- c(conds, "judgeId = ?judgeId")
    interp$judgeId <- judgeId
  }
  if (!missing(sessionId)) {
    conds <- c(conds, "sessionId = ?sessionId")
    interp$sessionId <- sessionId
  }
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- "select studentId, Students.name as studentName, Students.affiliation as studentAffiliation,
                   judgeId, Judges.name as judgeName,
                   Judges.affiliation as judgeAffiliation,
                   session, Students.datetime as datetime,
                   Students.title as title, room, Sessions.name as sessionName, abstract, summaryFile
                   from Students, Judges, Judging, Sessions
                   where Students.idnum = studentId and Judges.idnum = judgeId and Sessions.idnum = Students.session"
  if (length(conds)) {
    query <- paste(query, "and", paste(conds, collapse = " and "))
    query <- do.call(DBI::sqlInterpolate,
                     c(list(conn, query), interp))
  }
  query <- paste(query, "order by judgeId, datetime")
  result <- dbGetQuery(conn, query)
  result
}

# Initialize the "judging" table using the timetable.

initJudging <- function() {
  judging <- showTable("judging")
  if (nrow(judging)) stop("judging table is not empty")
  judges <- showTable("judges")
  students <- subset(showTable("students"), !is.na(summaryFile) & confirmed & grepl("General", competition))
  for (j in judges$idnum) {
    judge <- subset(judges, idnum == j)
    sessions <- sub(":.*$", "", judge$note)
    for (session in strsplit(sessions, ",")[[1]]) {
      s <- as.numeric(session)
      if (!is.na(s)) {
        students1 <- subset(students, session == s)
        assignJudges(students1$idnum, j, append = TRUE)
      } else {
        s <- as.numeric(sub("(", "", sub(")", "", session, fixed = TRUE), fixed = TRUE))
        if (!is.na(s))
          cat(sprintf("Judge %d (%s) is partial for session %d; not added.\n",
                      j, judge$name, s))
        else
          cat(sprintf("Session '%s' not recognized for judge %d (%s).\n",
                      session, j, judge$name))
      }
    }
  }
}


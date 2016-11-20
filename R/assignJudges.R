assignJudges <- function(studentId, judge1Id, judge2Id) {
  len <- max(length(studentId), length(judge1Id), length(judge2Id))
  studentId <- rep(studentId, length.out=len)
  judge1Id <- rep(judge1Id, length.out=len)
  judge2Id <- rep(judge2Id, length.out=len)
  for (i in seq_len(len)) {
    talk <- getTalks(studentId = studentId)
    if (

getTalks <- function(studentId = "", judgeId = "", sessionId = "") {
  conds <- character()
  if (!missing(studentId))
    conds <- c(conds, "studentId = ?studentId")
  if (!missing(judgeId))
    conds <- c(conds, "(judge1Id = ?judgeId or judge2Id = ?judgeId)")
  if (!missing(sessionId))
    conds <- c(conds, "sessionId = ?sessionId")
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- "select * from Talks, Students, Judges"
  if (length(conds)) {
    query <- paste(query, "where", paste(conds, collapse = " and "))
    query <- do.call(DBI::sqlInterpolate,
                     list(conn, query,
                          studentId = studentId,
                          judgeId = judgeId,
                          sessionId = sessionId))
  }
  dbGetQuery(conn, query)
}

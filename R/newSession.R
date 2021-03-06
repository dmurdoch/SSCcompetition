newSession <- function(name, date = NA, time = NA, contributed = TRUE, room = NA) {
  idnum <- getID(name, "Sessions", unique = FALSE)
  if (length(idnum)) {
    warning("Session ", dQuote(name), " already in table, not added.")
    return()
  }

  idnum <- newIdnum("Sessions")
  if (is.na(date))
    datetime <- NA
  else
    datetime <- as.POSIXct(paste(date, time), tz="CST6CDT", origin = "1970-01-01")
  if (name == "")
    name <- "Poster Session"
  df <- data.frame(idnum = idnum, name = name,
                   datetime = as.numeric(datetime),
                   contributed = as.numeric(contributed),
                   room = as.character(room),
                   stringsAsFactors = FALSE)
  conn <- getConn()
  on.exit(doneWith(conn))
  dbWriteTable(conn, "Sessions", value = df, append =TRUE)
  invisible(idnum)
}

loadSessions <- function(csv = paste0("~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/",
                                      "ContributedPresentations-ForKevin-final.csv")) {
  talks <- read.csv(csv, stringsAsFactors = FALSE)
  sessions <- showTable("sessions")
  for (i in seq_len(nrow(talks))) {
    talk <- talks[i,]
    if (talk$Type.of.Presentation == "Poster")
      talk$Session <- "Poster Session"
    session <- getID(talk$Session, "Sessions", unique = FALSE)
    if (length(session) == 0) {
      session <- newSession(talk$Session,
                 date = if (nchar(talk$Date)) talk$Date else NA,
                 time = if (nchar(talk$StartTime)) talk$StartTime else NA,
                 contributed = talk$Type.of.Presentation != "Invited")
      sessions <- rbind(sessions, data.frame(idnum = session, name = talk$Session,
                                             datetime = as.POSIXct(paste(talk$Date, talk$StartTime), tz="CST6CDT", origin = "1970-01-01"),
                                             contributed = talk$Type.of.Presentation != "Invited"),
                                             room = NA)

    } else if (length(session) == 1) {
      sessions[sessions$idnum == session, "datetime"] <- as.POSIXct(paste(talk$Date, talk$StartTime), tz="CST6CDT", origin = "1970-01-01")
    } else if (length(session) > 1)
      stop("Too many session records: ", dQuote(talk$Session))
  }
  replaceTable("sessions", sessions)
}

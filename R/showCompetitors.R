getSession <- function(...) {
  conn <- getConn()
  on.exit(doneWith(conn))
  names <- names(list(...))
  values <- list(...)
  query <- paste("SELECT * FROM Sessions WHERE", paste0(names, " = :", names, collapse = " and "))
  result <- dbGetQuery(conn, query, params = values)
  result$datetime <- as.POSIXct(result$datetime, origin = "1970-01-01",
                                tz = "CST6CDT")
  result
}

showCompetitors <- function(output = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/competitors.pdf") {
  allstudents <- showTable("Students")
  allstudents <- subset(allstudents, confirmed & !is.na(summaryFile) & grepl("General", competition) &
                          (is.na(datetime) | datetime < as.POSIXct("2017-06-14", tz="CST6CDT")))
  o <- order(allstudents$session)
  allstudents <- allstudents[o,]
  md <- file(paste0(tools::file_path_sans_ext(output), ".Rmd"), open = "wt")
  writeLines('---
title: "Competitors in SSC General Competition"
author: "Duncan Murdoch"
date: "3/24/2017"
output: pdf_document
---

The entries below are organized according to their tentative
session assignment.
', con=md)
  for (s in unique(allstudents$session)) {
    session <- getSession(idnum = s)
    writeLines(c(paste0("## ", s, ".  ", session$name), ""), con = md)
    students <- subset(allstudents, session == s)
    for (i in seq_len(nrow(students))) {
      student <- students[i,]
      writeLines(c(paste0(student$submission, "\\.  ", student$name, "   "),
                   paste0("Affiliation:  ", student$affiliation, "    "),
                   dQuote(student$title), ""), con = md)
    }
  }
  close(md)
}

showSchedule <- function(alltalks = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/ContributedPresentations-ForKevin-final.csv",
output = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/schedule.pdf",
                         judge = NULL) {

  judges <- showTable("judges")
  sessiondb <- showTable("sessions")

  md <- file(paste0(tools::file_path_sans_ext(output), ".Rmd"), open = "wt")
  writeLines('---
title: "Schedule Summary"
author: "Duncan Murdoch"
output: pdf_document
---

This is based on a preliminary version of the schedule, not the final one.
             ', con=md)

  alltalks <- read.csv(alltalks)
  for (d in unique(alltalks$Date)) {
    day <- subset(alltalks, Date == d)
    for (s in unique(day$StartTime)) {
      slot <- subset(day, StartTime == s)
      writeLines(c(paste0("## ", d, "  ", s, " -- ", slot$EndTime[1]), ""), con = md)
      sessions <- unique(slot[, c("Type.of.Presentation", "Session")])
      o <- order(sessions$Type.of.Presentation, decreasing = TRUE)
      sessions <- sessions[o, "Session"]
      for (sess in unique(sessions)) {
        session <- subset(slot, Session == sess)
        chair <- session$SessionChair[1]
        speakers <- session$Speaker
        speakers <- speakers[nchar(speakers) > 0]
        if (sess %in% sessiondb$name) {
          idnum <- sessiondb$idnum[sess == sessiondb$name]
          sess <- paste0(idnum, ".  ", sess)
          regexp <- paste0("(^|[^0-9])", idnum, "([^0-9]|$)")
          assignment <- sub(":.*", "", judges$note)
          thisjudge <- grep(regexp, assignment)
        } else thisjudge <- numeric()

        writeLines(c(paste0(session$Type.of.Presentation[1], ": **", sess, "**   "),
                     paste0("Chair:  ", chair, "    "),
                   if (length(speakers)) paste0("Speakers:  ", paste(speakers, collapse = ", "), "    "),
                   if (length(thisjudge)) paste0("**Judges:  ", paste(judges[thisjudge, "name"], collapse = ", "), "**    "),
                   ""), con = md)
      }
    }
  }
  close(md)
}

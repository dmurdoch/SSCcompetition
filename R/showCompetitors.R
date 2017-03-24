getSession <- function(...) {
  conn <- getConn()
  on.exit(doneWith(conn))
  names <- names(list(...))
  values <- list(...)
  query <- paste("SELECT * FROM Sessions WHERE", paste0(names, " = :", names))
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
      writeLines(c(paste0(student$submission, ".  ", student$name, "   "),
                   paste0("Affiliation:  ", student$affiliation, "    "),
                   dQuote(student$title), ""), con = md)
    }
  }
  close(md)
}

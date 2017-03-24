getSession <- function(...) {
  conn <- getConn()
  on.exit(doneWith(conn))
  names <- names(list(...))
  values <- list(...)
  query <- paste("SELECT * FROM Sessions WHERE", paste0(names, " = :", names))
  dbGetQuery(conn, query, params = values)
}

showCompetitors <- function() {
  students <- showTable("Students")
  students <- subset(students, confirmed & !is.na(summaryFile) & grepl("General", competition))
  o <- order(students$session)
  students <- students[o,]
}

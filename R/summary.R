globalVariables(c("competition", "summaryFile", "confirmed", "idnum"))

entrySummary <- function(abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts",
                         summaryDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Summaries") {
  abstracts <- list.files(abstractDir)
  summaries <- list.files(summaryDir)
  table <- showTable("students")
  for (comp in unique(table$competition)) {
    cat("\n", comp, "\n")
    for (hasSummary in c(TRUE, FALSE)) {
      cat("\nStudents ", ifelse(hasSummary, "with", "without"), " summary file:\n")
      nosummary <- with(table, is.na(summaryFile) | nchar(summaryFile) < 3)
      group <- subset(table, competition == comp & nosummary == !hasSummary)
      if (!nrow(group)) cat("(no entries)\n")
      for (i in seq_len(nrow(group))) {
        student <- group[i,]
        if (hasSummary) {
          if (student$summaryFile %in% summaries)
            summaries <- summaries[summaries != student$summaryFile]
          else
            warning("summary file ", student$summaryFile, " missing!")
        }
        cat(student$submission, ".  ", student$name, if (student$poster) " (poster)", if (student$language == "French") " (French)", "\n", sep="")
      }
    }
  }
  if (length(summaries)) {
    cat("\nSummary files not in database:\n")
    cat(summaries, sep = "\n")
  }
}

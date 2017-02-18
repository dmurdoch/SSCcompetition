mergeSSC <- function(SSCfile, abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts") {
  allSSC <- read.csv(SSCfile)
  for (i in seq_len(nrow(allSSC))) {
    SSC <- allSSC[i,, drop = FALSE]
    lastname <- SSC$Last.Name
    firstname <- sub(" .*", "", SSC$First.Name)
    SSCname <- paste0(lastname, ", ", firstname)
    db <- showName(SSCname)
    if (nrow(db) > 1) {
      db <- showName(SSCname, exact = TRUE)
      if (nrow(db) != 1) {
        message("SSC name ", SSCname, " matches multiple records:")
        print(db)
        message("No merge done")
        next
      }
    } else if (!nrow(db)) {
      message("SSC name ", SSCname, " does not match any names.  Adding to DB")
      newStudent(SSCname)
      db <- showName(SSCname)
    } else {
      message("SSC name ", SSCname, " matches ", db$name, ".  Updating.")
    }
    db$submission <- SSC$Node.ID
    db$name <- paste0(lastname, ", ", SSC$First.Name)
    if (nchar(SSC$Middle.Name))
      db$name <- paste(db$name, SSC$Middle.Name)
    db$email <- SSC$E.mail
    db$language <- SSC$Language
    db$competition <- SSC$Consideration.for.Student.Presentation.Award
    db$poster <- SSC$Type.of.Presentation == "poster"
    db$title <- SSC$Title
    affiliation <- db$affiliation
    if (is.na(affiliation)) affiliation <- ""
    if (!nchar(affiliation)) affiliation <- SSC$University..abstract.submission.travel.
    if (!nchar(affiliation)) affiliation <- SSC$My.Institution..abstract.submission.
    if (!nchar(affiliation)) affiliation <- SSC$University..membership.
    db$affiliation <- affiliation
    db$abstract <- paste0("abstract", SSC$Node.ID, ".txt")
    f <- file(file.path(abstractDir, db$abstract), "w")
    writeLines(paste("Submission: ", SSC$Node.ID), f)
    writeLines(paste("Name:       ", db$name), f)
    writeLines(paste("Affiliation:", db$affiliation), f)
    writeLines(strwrap(db$title, 70, exdent = 13,
           initial = "Title:       "), f)
    writeLines("\nAbstract:\n", f)
    writeLines(strwrap(SSC$Abstract, 60), f)
    close(f)
    updateStudents(db)
  }
}


judgePackage <- function(judge, dir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017") {
  # Produce package for one judge:
  #  - Schedule of judging assignments
  #  - Evaluation sheets for each talk
  #  - Abstract and cover info for each talk
  #  - Short summary paper for each talk

  if (!file.exists(dir))
    stop("Directory ", dir, " does not exist.")

  talks <- getTalks(judgeId = judge)
  judgeName <- gsub("[[:space:]]", "", talks$judgeName[1])
  olddir <- setwd(file.path(dir, "Judging"))
  pdf <- file.path(dir, "Judging", paste0(judgeName, ".pdf"))
  file.copy(file.path(dir, "Summaries", talks$summaryFile), ".")
  rmarkdown::render(system.file("doc/judging.Rmd", package = "SSCcompetition"),
                    output_file = pdf,
                    intermediates_dir = file.path(dir, "Judging"),
                    params = list(judge = judge, dir = dir, summaries = talks$summaryFile))
  unlink(talks$summaryFile)
  setwd(olddir)
}

judgePackages <- function(dir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017") {
  judges <- showTable("judges")
  for (i in seq_len(nrow(judges))) {
    judge <- judges[i,]
    judgePackage(judge$idnum, dir)
    cat(judge$name, " <", judge$email, ">\n", sep="")
  }
}

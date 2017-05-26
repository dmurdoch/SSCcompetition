judgePackage <- function(judge, dir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/JudgePackages") {
  # Produce package for one judge:
  #  - Schedule of judging assignments
  #  - Evaluation sheets for each talk
  #  - Abstract and cover info for each talk
  #  - Short summary paper for each talk

  if (!file.exists(dir))
    stop("Directory ", dir, " does not exist.")

}

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
                    clean = FALSE,
                    params = list(judge = judge, dir = dir, summaries = talks$summaryFile))
  unlink(talks$summaryFile)
  setwd(olddir)
}

scoresheet <- function(talk) {
  cat("\\begin{center}
\\textbf{Statistical Society of Canada Annual Meeting  / Congrès annuel de la Société statistique du Canada} \\\\
\\textbf{Student Research Presentation Score Sheet / Fiche d’évaluation de la présentation de recherche étudiante} \\\\
\\end{center}
\\vspace{0.2in}

Name of student / Nom de l’étudiant(e):  \\textbf{", talk$studentName, "}   \n",
"Title of paper / Titre de l’article:  \\textbf{", talk$title, "}

\\vspace{0.2in}
\\textit{Please give a score from 1 to 5 (1=lowest, 5=highest) to each of the following:}

\\textit{Veuillez attribuer une note de 1 à 5 (1=min, 5=max) pour chacun des critères suivants:}
\\vspace{0.2in}
\\renewcommand{\\arraystretch}{1.1}
\\begin{tabular}{lccccc}
\\textbf{A. RESEARCH / RECHERCHE} & \\multicolumn{5}{c}{\\textbf{SCORE/NOTE}} \\\\
Interest and importance of research / Intérêt et importance de la recherche & 1 & 2 & 3 & 4 & 5 \\\\
Originality of research / Originalité de la recherche & 1 & 2 & 3 & 4 & 5 \\\\
Difficulty and sophistication of methods / Complexité et élégance des méthodes & 1 & 2 & 3 & 4 & 5 \\\\
Student mastery of material / Maîtrise du contenu de la part de l’étudiant(e) & 1 & 2 & 3 & 4 & 5 \\\\
Overall quality of research / Qualité globale de la recherche & 1 & 2 & 3 & 4 & 5 \\\\
\\\\
& \\multicolumn{5}{r}{\\textbf{Total (A):}} \\\\
\\\\
\\textbf{B. DELIVERY / PRÉSENTATION} & \\multicolumn{5}{c}{\\textbf{SCORE/NOTE}} \\\\
Clarity and pace of presentation / Clarité et rythme de la présentation & 1 & 2 & 3 & 4 & 5 \\\\
Selection of material from summary / Sélection de contenu à partir du résumé & 1 & 2 & 3 & 4 & 5 \\\\
Use of graphics / Utilisation de graphiques & 1 & 2 & 3 & 4 & 5 \\\\
Ability to answer questions / Habilité à répondre aux questions & 1 & 2 & 3 & 4 & 5 \\\\
Overall quality / Qualité globale & 1 & 2 & 3 & 4 & 5 \\\\
\\\\
& \\multicolumn{5}{r}{\\textbf{Total (B):}} \\\\
\\\\
\\\\
\\textbf{Final score (from 10 to 50) / Note finale (de 10 à 50)} & \\multicolumn{5}{r}{\\textbf{TOTAL (A+B):}} \\\\
\\\\
\\textbf{Comments / Commentaires:}
\\end{tabular}
\\renewcommand{\\arraystretch}{1}
\\vspace{2in}
Judge/Juge:  ", talk$judgeName, "\\hspace{0.5in}Signature:
")
}

judgePackages <- function(dir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017") {
  judges <- showTable("judges")
  for (i in seq_len(nrow(judges))) {
    judge <- judges[i,]
    judgePackage(judge$idnum, dir)
  }
  for (i in seq_len(nrow(judges))) {
    judge <- judges[i,]
    cat(judge$name, " <", judge$email, ">\n", sep="")
  }
}

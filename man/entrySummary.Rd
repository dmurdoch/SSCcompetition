\name{entrySummary}
\alias{entrySummary}
\title{
Summarize status of entries
}
\description{
Prints list of students categorized according to the
status of their entry:  which award, presence or absence
of summary file.
}
\usage{
entrySummary(
  abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts",
  summaryDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Summaries")
}
\arguments{
  \item{abstractDir, summaryDir}{
Where are the summaries and abstracts stored.
}
}
\value{
No useful return value; called for the side effect
of printing the report.
}

\examples{
entrySummary()
}

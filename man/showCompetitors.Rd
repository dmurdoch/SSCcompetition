\name{showCompetitors}
\alias{showCompetitors}
\alias{showSchedule}
\title{
Produce a report on all competitors or all sessions.
}
\description{
Writes out R Markdown file to be processed into PDF file
listing info from the database.
}
\usage{
showCompetitors(output =
"~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/competitors.pdf")
showSchedule(alltalks =
"~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/ContributedPresentations-ForKevin-final.csv",
output =
"~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/schedule.pdf")
}
\arguments{
  \item{output}{
The desired name of the PDF output file.
}
  \item{alltalks}{
CSV file for schedule.
}
}
\details{
Writes an R Markdown file to produce the report on all
competitors.  Currently does not render it.
}
\value{
Called for the side effect of producing the report.
}

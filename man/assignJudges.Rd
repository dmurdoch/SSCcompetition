\name{assignJudges}
\alias{assignJudges}
\title{
Assign judges to a student
}
\description{
Assign judges by id to a student.
}
\usage{
assignJudges(studentID, judgeID, append = TRUE)
}
\arguments{
  \item{studentID}{
Student ID number(s)
}
  \item{judgeID}{
Numeric id of judge(s)
}
  \item{append}{
Whether to add judges, or replace them
}
}
\examples{
\dontrun{
  assignJudges(getID("\%Trump\%"), getID("\%Murdoch\%", "Judges"))
}
}

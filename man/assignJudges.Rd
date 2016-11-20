\name{assignJudges}
\alias{assignJudges}
\title{
Assign judges to a student
}
\description{
Assign judges by id to a student.
}
\usage{
assignJudges(student, judge1Id, judge2Id)
}
\arguments{
  \item{student}{
Student name (or an SQL pattern matching one uniquely)
}
  \item{judge1Id}{
Numeric id of one judge.
}
  \item{judge2Id}{
Numeric id of another judge.
}
}
\examples{
assignJudges("\%Trump\%", 1,2)
}

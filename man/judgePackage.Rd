\name{judgePackage}
\alias{judgePackage}
\alias{judgePackages}
\title{
Produce package for one judge.
}
\description{
\code{judgePackage} produces a PDF file for one judge containing the following:   - Schedule of judging assignments,
 - Evaluation sheets for each talk,
 - Abstract and cover info for each talk,
 - Short summary paper for each talk

\code{judgePackages} does it for all judges.
}
\usage{
judgePackage(judge, dir =
"~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017")
judgePackages(dir =
"~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017")
}
\arguments{
  \item{judge}{
Name of the judge.
}
  \item{dir}{
Directory in which to place the file.
}
}

\value{
Called for the side effect of producing the PDF package.
}

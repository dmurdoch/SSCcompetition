\name{mergeSSC}
\alias{mergeSSC}
\title{
Merge data from SSC spreadsheet.
}
\description{
Reads a CSV format spreadsheet from the SSC abstract database,
and merges the data into the Students table.
}
\usage{
mergeSSC(SSCfile,
abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts")
}
\arguments{
  \item{SSCfile}{
The CSV file from the SSC.
}
  \item{abstractDir}{
Where should abstracts be written?
}
}
\value{
No useful value.
}

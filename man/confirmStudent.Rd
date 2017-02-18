\name{confirmStudent}
\alias{setConfirmed}
\title{
Set the "confirmed" field for a student.
}
\description{
Looks for a student, and sets the confirmed field if
there's a unique match.
}
\usage{
setConfirmed(name, confirmed = TRUE, ignore.case = TRUE,
             exact = FALSE)
}

\arguments{
  \item{name}{
The exact name, or a partial string in SQL style, e.g. \code{"\%partial\%"}.
}
  \item{confirmed}{
The desired value for the field.
}
  \item{ignore.case, exact}{
How to treat \code{name}.  See \code{\link{showName}}.
}
}
\value{
The previous value is returned invisibly.
}

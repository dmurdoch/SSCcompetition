\name{showName}
\alias{showName}
\title{
Show a student record by name.
}
\description{
Displays one or more students matching a search
on the name.
}
\usage{
showName(name, ignore.case = TRUE, exact = FALSE)
}
\arguments{
  \item{name}{
An exact or partial match to the name.
}
  \item{ignore.case}{
Whether to ignore capitalization.
}
  \item{exact}{
Whether \code{name} is the full name or a substring.
}
}
\value{
A dataframe containing students matching the
requested name.
}

\examples{
showName("trump")
}

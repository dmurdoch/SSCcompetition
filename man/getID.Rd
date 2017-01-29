\name{getID}
\alias{getID}
\title{
Get student, judge or session ID number from name.
}
\description{
Converts name to ID number.
}
\usage{
getID(pattern, table = c("Students", "Judges", "Sessions"),
          unique = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern}{
An SQL pattern to match.
}
  \item{table}{
Which table to look in?  Partial matches allowed.
}
  \item{unique}{
Is a unique answer needed?  If TRUE, anything else is an error.
}
}
\value{
A vector of ID numbers (one by default).
}
\examples{
\dontrun{
getID("\%Trump\%")
}
}

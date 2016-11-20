\name{getTalks}
\alias{getTalks}
\title{
Get talks by student, judge, or session
}
\description{
Search for talks matching a pattern.
}
\usage{
getTalks(student, judgeId, sessionId)
}
\arguments{
  \item{student}{
A student name or SQL pattern matching one.
}
  \item{judgeId}{
A numerical judge ID.
}
  \item{sessionId}{
A numerical session ID.
}
}
\examples{
getTalks("\%Trump\%")
}

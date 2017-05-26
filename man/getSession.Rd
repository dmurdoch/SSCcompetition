\name{getSession}
\alias{getSession}
\alias{getStudent}
\title{
Select records from the database.
}
\description{
Constructs and runs a query to find sessions or students.
}
\usage{
getSession(...)
getStudent(...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{
Named values for fields in the sessions table.
}
}

\details{
Current fields in the sessions table are  "idnum", "name", "datetime", "contributed".

Current fields in the students table are "idnum",       "name",        "affiliation", "date",
"title",       "abstract",    "confirmed",   "summaryFile",
"email",       "submission",  "language",    "competition",
"poster",      "datetime",    "session".
}
\value{
A dataframe containing the results of the query.
}

\examples{
getSession(idnum = 2)
getStudent(poster = TRUE, confirmed = TRUE)
}

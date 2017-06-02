\name{newSession}
\alias{newSession}
\title{
Add a session to the database.
}
\description{
Adds a session to the database.
}
\usage{
newSession(name, date = NA, time = NA, contributed = TRUE, room = NA)
}
\arguments{
  \item{name, date, time, contributed, room}{
Characteristics of the session.
}
}
\value{
Called for the side effect of adding the session.
}

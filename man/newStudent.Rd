\name{newStudent}
\alias{newStudent}
\title{
Add a new student
}
\description{
Add a new student to the database
when the registration message is received.
}
\usage{
newStudent(name,
          affiliation = "",
          date = Sys.Date(),
          title = NA_character_,
          abstract = NA_character_,
          confirmed = FALSE,
          email = "",
          summaryFile = NA_character_,
          datetime = NA, sessionId = NA)
}
\arguments{
  \item{name}{
The student's name, surname first.
}
  \item{affiliation}{
The university of the student.
}
  \item{date}{
Date of registration.
}
  \item{title, abstract}{Title and abstract of talk/poster}
  \item{confirmed}{Has confirmation been received from supervisor?}
  \item{email}{The student's email address}
  \item{summaryFile}{Filename of summary document received from student.}
  \item{datetime, sessionId}{When is the talk?}
}

\value{
A data frame containing all student records,
in order of registration.
}

\examples{
\dontrun{
newStudent("Trump, Donald", "Trump University")
}
}

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
newStudent(name, affiliation = "", date = Sys.Date())
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
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A data frame containing all student records,
in order of registration.
}

\examples{
newStudent("Trump, Donald", "Trump University")
}

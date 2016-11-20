\name{newJudge}
\alias{newJudge}
\title{
Add a judge to the table.
}
\description{
Adds a judge with a unique id to the table.
}
\usage{
newJudge(name, expertise, affiliation)
}
\arguments{
  \item{name}{
The name of the judge.
}
  \item{expertise}{
A text description of the self-described expertise of the judge.
}
  \item{affiliation}{
Employer of the judge.
}
}
\examples{
newJudge("Duncan Murdoch", "everything", "UWO")
}

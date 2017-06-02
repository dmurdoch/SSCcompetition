\name{scoresheet}
\alias{scoresheet}
\title{
Output code for a scoresheet.
}
\description{
Produces Markdown (mostly LaTeX) code for a scoresheet.
}
\usage{
scoresheet(talk, oral = TRUE)
}
\arguments{
  \item{talk}{
A row from \code{\link{getTalks}}.
}
  \item{oral}{
Whether to produce the oral presentation sheet or the poster
sheet.
}
}

\value{
Called for the side effect of printing the
contents of the sheet, customized to the \code{talk}.
}

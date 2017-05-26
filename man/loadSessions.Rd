\name{loadSessions}
\alias{loadSessions}
\alias{loadTiming}
\title{
Load SSC data from CSV file.
}
\description{
Loads data into the table from files obtained from the
SSC database.
}
\usage{
loadSessions(
csv = paste0("~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/",
             "ContributedPresentations-ForKevin-final.csv"))
loadTiming(
csv = paste0("~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/",
             "ContributedPresentations-Schedule-final-2.csv"))
}
\arguments{
  \item{csv}{
The file to load from.
}
}
\details{
The sessions file is assumed to have these column
headings:  "Date, StartTime, EndTime, SpeakerStart,
SpeakerEnd, Node ID, Type of Presentation, Speaker,
Session, Consideration for Student Presentation Award,
Sponsor, SessionChair".

The timings file is assumed to have these column headings:  "Date, StartTime, EndTime, SpeakerStart, SpeakerEnd,
Node ID, Speaker, Presentation Title, Abstract, Type of Presentation,
Keywords, Co-authors, Additional Speakers, Session,
Consideration for Student Presentation Award, Updated,
Translation State, Sponsor, SessionChair".
}
\value{
Called for the side effect of updating the database.
}

\name{entrySummary}
\alias{entrySummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
entrySummary(abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts", summaryDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Summaries")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{abstractDir}{
%%     ~~Describe \code{abstractDir} here~~
}
  \item{summaryDir}{
%%     ~~Describe \code{summaryDir} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (abstractDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Abstracts", 
    summaryDir = "~/work/SSC/StudentAwards/Student Presentation Awards/Competition 2017/Summaries") 
{
    abstracts <- list.files(abstractDir)
    summaries <- list.files(summaryDir)
    table <- showTable("students")
    for (comp in unique(table$competition)) {
        cat("\n", comp, "\n")
        for (hasSummary in c(TRUE, FALSE)) {
            cat("\nStudents ", ifelse(hasSummary, "with", "without"), 
                " summary file:\n")
            nosummary <- with(table, is.na(summaryFile) | nchar(summaryFile) < 
                3)
            group <- subset(table, competition == comp & nosummary == 
                (!hasSummary))
            if (!nrow(group)) 
                cat("(no entries)\n")
            for (i in seq_len(nrow(group))) {
                student <- group[i, ]
                if (hasSummary) {
                  if (student$summaryFile \%in\% summaries) 
                    summaries <- summaries[summaries != student$summaryFile]
                  else warning("summary file ", student$summaryFile, 
                    " missing!")
                }
                cat(student$submission, ".  ", student$name, 
                  if (student$poster) 
                    " (poster)", if (student$language == "French") 
                    " (French)", "\n", sep = "")
            }
        }
    }
    if (length(summaries)) {
        cat("\nSummary files not in database:\n")
        cat(summaries, sep = "\n")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

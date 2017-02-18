\name{showTable}
\alias{showTable}
\title{
Shows a table for debugging purposes.
}
\description{
Shows a table for debugging purposes.
}
\usage{
showTable(name, where = NA, orderby = NA, params)
}
\arguments{
  \item{name}{
The name of the table to show.
}
  \item{where}{
An SQL expression to be used in WHERE.
}
  \item{orderby}{
An SQL expression to be used in ORDER BY.
}
  \item{params}{
Optional parameters to be used in the query.
}
}
\value{
A data frame containing the table.
}

\examples{
showTable("Students")
}

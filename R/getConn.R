
getConn <- function() {
  dbConnect(SQLite(), "~/work/9864/SSCcompetition.data")
}

doneWith <- function(conn) {
  dbDisconnect(conn)
}

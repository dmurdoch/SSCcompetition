stringQuery <- function(field, s) {
  if (grepl("%", s))
    query <- paste(field, "like ?")
  else
    query <- paste(field, "= ?")
}

showName <- function(name) {
  conn <- getConn()
  on.exit(doneWith(conn))
  name <- tolower(name)
  query <- stringQuery("lower(name)", name)
  dbGetQuery(conn, paste("select * from Students where", query), params=name)
}

confirmStudent <- function(name, confirmed = TRUE) {
  count <- nrow(showName(name))
  if (!count)
    stop("No matching student.")
  if (count > 1)
    stop(count, " matching students.")
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- stringQuery("name", name)
  dbGetQuery(conn, paste("update Students set confirmed =", as.numeric(confirmed),"where", query), params=name)
  invisible(confirmed)
}

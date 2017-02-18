stringQuery <- function(field, ignore.case = FALSE, exact = TRUE) {
  if (ignore.case)
    field <- paste0("lower(", field, ")")
  if (!exact)
    query <- paste(field, "like ?")
  else
    query <- paste(field, "= ?")
}

showName <- function(name, ignore.case = TRUE, exact = FALSE) {
  conn <- getConn()
  on.exit(doneWith(conn))
  if (ignore.case) name <- tolower(name)
  if (!exact) name <- paste0("%", name, "%")
  query <- stringQuery("name", ignore.case, exact)
  showTable("Students", where = query, params = name)
}

setConfirmed <- function(name, confirmed = TRUE, ignore.case = TRUE, exact = FALSE) {
  prev <- showName(name, ignore.case = ignore.case, exact = exact)
  count <- nrow(prev)
  if (!count)
    stop("No matching student.")
  if (count > 1)
    stop(count, " matching students.")
  conn <- getConn()
  on.exit(doneWith(conn))
  query <- stringQuery("name", name)
  dbGetQuery(conn, paste("update Students set confirmed =", as.numeric(confirmed),"where", query), params=name)
  invisible(as.logical(prev$confirmed))
}

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

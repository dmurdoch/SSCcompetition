newStudent <- function(name, affiliation = "", date = Sys.Date()) {
  data.frame(name = name, affiliation = affiliation,
             date = date)
}

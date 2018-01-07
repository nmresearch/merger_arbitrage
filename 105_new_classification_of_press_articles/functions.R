articles.withdrawn <- function(s,   # Data.frame containing SDC data.
                              f) { # Data.frame containing Factiva data.
  ## This function calculates the row positions in 'f' that correspond
  ## to withdrawn deals.
  stopifnot(class(s) == "data.frame", class(f) == "data.frame")
  did <- s[which(s$STAT == "Withdrawn"), "dealID"]
  return(which(f$dealID %in% did))
}

articles.completed <- function(s,   # Data.frame containing SDC data.
                               f) { # Data.frame containing Factiva data.
  ## This function calculates the row positions in 'f' that correspond
  ## to completed deals.
  stopifnot(class(s) == "data.frame", class(f) == "data.frame")
  did <- s[which(s$STAT == "Completed"), "dealID"]
  return(which(f$dealID %in% did))
}

date.year.beg <- function(y) {
  stopifnot(class(y) == "integer")
  b <- as.Date(NA)
  for (i in seq_along(y)) 
    b[i] <- as.Date(paste(as.character(y[i]), "-01-01", sep = ""))
  return(b)
}

date.year.end <- function(y) {
  stopifnot(class(y) == "integer")
  e <- as.Date(NA)
  for (i in seq_along(y)) 
    e[i] <- as.Date(paste(as.character(y[i]), "-12-31", sep = ""))
  return(e)
}

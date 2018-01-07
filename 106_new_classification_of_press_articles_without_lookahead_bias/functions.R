
rm.article <- function(sdc, f, Dt = "DRES") {
  ## For each deal, remove articles from Factiva that appear before
  ## 'Dt'.
  stopifnot(Dt %in% c("DRES", "DA"))
  
  rows <- NULL
  for (i in seq_along(sdc[, 1])) { # Cycle through _rows_ of 'sdc'.
    rows1 <- which(f$dealID == sdc[i, "dealID"])
    rows2 <-
      if (Dt == "DRES") {
        which(f$Date < sdc[i, Dt])
      } else {
        which(f$Date <= sdc[i, Dt])
      }
    rows <- c(rows, intersect(rows1, rows2))
  }
  rows <- sort(unique(rows))

  stopifnot(length(rows) > 0)
  return(f[rows, ])
}

dealID.factiva <- function(sdc, f) {
  ## Add deal status to the Factiva data.frame.
  f$STAT <- rep(sdc[1, "STAT"], nrow(f)); is.na(f$STAT) <- TRUE # Consistent contrasts.
  for (i in seq_along(sdc[, 1])) {
    rows <- which(f$dealID == sdc[i, "dealID"])
    if (length(rows) > 0)
      f[rows, "STAT"] <- sdc[i, "STAT"]
  }
  return(f)
}

rows.for.time <- function(f,   # Factiva data.
                          b,   # Beginning time.
                          e,   # Ending time. 
                          m) { # Maximum allowable time for 'e'.
  ## This function calculates the rows of 'f' that correspond to the
  ## time interval 'b' until 'e'.

  ## Verify function arguments.
  stopifnot(class(f) == "data.frame",
            "Date" %in% colnames(f),
            class(f$Date) == "Date",
            all(!is.na(f$Date)),
            class(b) == "Date", class(e) == "Date", class(m) == "Date", 
            b < e,
            e <= m)

  ret <- 
    if (e < m) {
      which(b <= f$Date & f$Date < e)
    } else {
      which(b <= f$Date & f$Date <= e)
    }
  return(ret)
}



find.match <- function(df1, coln1 = c("dateAnnounced", "tName", "aName"),
                       df2, coln2 = c("Date_Announced", "Target_Name", "Acquiror_Name")) {
  ## ==================================================================
  ## 
  ## This function tries to find the row positions in the data.frame
  ## 'df1' that have matching entries in data.frame 'df2'.
  ## 
  ## ==================================================================

  check <- function(pos) { # Small helper function.
    if (length(pos) > 1) # Defensive programming.
      stop("Found multiple matching deals. This should not occur in the data.")
  }

  pos.match <- NULL
  for (i in seq_along(df2[, 1])) { # Cycle through rows of data.frame 'df2'.
    pos.dt <- grep(df2[i, coln2[1]], df1[, coln1[1]], fixed = TRUE)
    pos.tn <- grep(df2[i, coln2[2]], df1[, coln1[2]], fixed = TRUE)
    pos.an <- grep(df2[i, coln2[3]], df1[, coln1[3]], fixed = TRUE)
    pos <- intersect(pos.dt, intersect(pos.tn, pos.an)) # Try if we can find a 'common denominator'.
    check(pos)
    if (length(pos) == 1)
      pos.match <- c(pos.match, pos)
    
    ## If we could not find a match, leave out one of the criteria above
    ## and see if we still can find a match.
    if (length(pos) == 0) {
      pos <- intersect(pos.dt, pos.tn) # Try something else.
      check(pos)
      if (length(pos) > 0) {
        pos.match <- c(pos.match, pos)
      } else { # Lenght is zero ==> didn't find a match.
        pos <- intersect(pos.dt, pos.an) # Try something else.
        check(pos)
        if (length(pos) > 0) {
          pos.match <- c(pos.match, pos)
        } else { # Lenght is zero ==> didn't find a match.
          pos <- intersect(pos.tn, pos.an) # Try something else.
          check(pos)
          if (length(pos) > 0) {
            pos.match <- c(pos.match, pos)
          }
        }
      }

      ## Visually inspect whether the match makes sense.
      if (length(pos) > 0) {
        print(df2[i, c(coln2[1], coln2[2], coln2[3])])
        print(df1[pos, c(coln1[1], coln1[2], coln1[3])])
        cat("\n\n")
      }
    }  
  } # End of for-loop.
  return(sort(unique(pos.match)))
}

getName <- function(dfSDC,                 # data.frame from SDC.
                    colPERMNO,             # Name of column containing PERMNO.
                    colDtAnn,              # Name of column containing announcement date.
                    con.db,                # Database connection.
                    catProgress = FALSE) { # Whether to show progress of this function.
  ##======================================================================
  ## 
  ## Description of this function
  ## 
  ## For every PERMNO in the column named 'colPERMNO' (from the
  ## data.frame 'dfSDC'), this script tries to find a matching company
  ## name from CRSP. If a PERMNO entry is NA, this function also
  ## outputs NA for the corresponding company name. This function
  ## outputs a character vector that contains the company names in the
  ## same sequence as the PERMNOs occur in 'dfSDC'.
  ## 
  ##======================================================================

  
  ## Character vector containing company names to be returned.
  retNam <- rep(0, nrow(dfSDC))
  is.na(retNam) <- TRUE
  
  for (i in seq_along(dfSDC[, 1])) { # Cycle through *rows* of 'dfSDC'.
    
    ## Get acquirer name from CRSP.
    PERMNO <- dfSDC[i, colPERMNO]
    if (is.na(PERMNO)) # Only continue if PERMNO is not NA.
      next
    
    dtAnn <- dfSDC[i, colDtAnn]
    
    ## Query SQLite.
    statement <- paste("SELECT * FROM ",
                       table.name.CRSP,
                       " WHERE PERMNO='", PERMNO,
                       "' AND DATE BETWEEN '",
                       as.numeric(dtAnn-8), "' AND '", as.numeric(dtAnn+8), "';",
                       sep="")
    dfCRSP <- dbGetQuery(con.db, statement)
    for (j in col.Date.CRSP) # Fix dates.
      class(dfCRSP[, j]) <- "Date"
    
    namCdt <- unique(dfCRSP$COMNAM) # Name candidate(s).
    if (length(namCdt) > 1) {
      print(namCdt)
      cat("For i = ", i, " there were multiple firm name matches for the given PERMNO. ",
          "You should investigate this case!\n\n", sep = "")
    } else if (length(namCdt) == 1) { # we're good.
      retNam[i] <- namCdt
    } else if (length(namCdt) == 0) { # Try less restrictive SQL query.
      statement <- paste("SELECT * FROM ", table.name.CRSP, " WHERE PERMNO='", PERMNO, "';", sep="")
      dfCRSP <- dbGetQuery(con.db, statement)
      for (j in col.Date.CRSP) # Fix dates.
        class(dfCRSP[, j]) <- "Date"
      namCdt <- unique(dfCRSP$COMNAM) # Name candidate(s).
      if (length(namCdt) == 1) { # we're good.
        retNam[i] <- namCdt
      } else {
        cat("For i = ", i, " it was not possible to find a matching firm name. ",
            "This is either because there were multiple matches or no matches at all. ",
            "You should investigate this case!\n\n", sep = "")
      }
    }
    if (catProgress) # show progress.
      cat(i/nrow(dfSDC)*100, "% done (i = ", i, "). \n", sep = "")
  } # Close for-loop.
  return(retNam)
}

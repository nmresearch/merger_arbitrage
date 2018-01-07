#######################################################################
##
## For every PERMNO in 'dfSDC', this function tries to find a matching
## PERMCO from CRSP. In case there is no PERMCO in CRSP, the PERMCO
## gets assigned 'NA'.
## 
#######################################################################

getPERMCO <- function(dfSDC,                 # data.frame containing SDC data.
                      colPERMNO,             # name of column containing PERMNOs in 'dfSDC'.
                      table.name.CRSP,       # name of SQLite table containing CRSP data.
                      con.db,                # database connection to SQLite.
                      catProgress = FALSE) { # whether this function should print its progress.
  
  ## Create empty (i.e. NA) vector of class 'numeric' that will be
  ## filled with the matching PERMCOs.
  retPERMCO <- rep(0, nrow(dfSDC))
  is.na(retPERMCO) <- TRUE
  
  for (i in seq_along(dfSDC[, 1])) { # Cycle through rows of 'dfSDC'.
    
    ## Calculate resolution date.
    resolution.date <-
      if (!is.na(dfSDC[i, "Date_Effective"])) {
        dfSDC[i, "Date_Effective"]
      } else {
        dfSDC[i, "Date_Withdrawn"]
      }
    
    PERMNO <- dfSDC[i, colPERMNO]
    if (is.na(PERMNO)) # Only continue if PERMNO is not NA.
      next
    
    statement <- paste("SELECT * FROM ",
                       table.name.CRSP,
                       " WHERE PERMNO='", PERMNO, "';", sep="")
    CRSP.df <- dbGetQuery(con.db, statement)
    for (j in col.Date.CRSP) # Fix dates.
      class(CRSP.df[, j]) <- "Date"

    ## Extract correct PERMCO and assign result to return vector
    ## 'retPERMCO'.
    CRSP.PERMCO <- unique(CRSP.df$PERMCO)
    if (length(CRSP.PERMCO) == 1)
      retPERMCO[i] <- CRSP.PERMCO
    
    ## Sanity checks.
    if (length(CRSP.PERMCO) == 0)
      cat("For i = ", i, " and PERMNO = ", PERMNO, " there is no matching PERMCO in CRSP.\n", sep = "") 
    if (length(CRSP.PERMCO) > 1) {
      cat("For i = ", i, " and PERMNO = ", PERMNO, " there are several matching PERMCOs in CRSP.\n", sep = "") 
      errStr <- paste("Something is strange since I would expect only one matching PERMCO in CRSP.",
                      "Maybe we also have to check the dates.")
      stop(errStr)
    }
    
    if (catProgress) # show progress.
      cat(i/nrow(dfSDC)*100, "% done (i = ", i, "). \n", sep = "")
  }
  return(retPERMCO)
}

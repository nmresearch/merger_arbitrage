#######################################################################
##
## This function adds PERMNOs contained in 'WRDS.CRSP.Identifiers' to
## 'TOData' (from SDC). If there are more than one PERMNO, this
## function assigns the PERMNO with the highest trading volume in the
## year prior to deal resolution. In case there is no PERMNO in CRSP
## during the time of the takeover, the PERMNO gets assigned 'NA' in
## 'TOData'.
##
#######################################################################

add.PERMNO <-
  function(TOData,                # SDC data
           str.SDC.CUSIP6,        # SDC column name for given CUSIP
           str.SDC.ticker,        # SDC column name for given ticker 
           str.SDC.PERMNO,        # SDC column name for PERMNO to be determined
           WRDS.CRSP.Identifiers) # Matching CUSIPs, tickers, PERMNOs
{

  ## Which rows in 'TOData' corresponds to multiple PERMNOs in
  ## 'WRDS.CRSP.Identifiers' that cannot be easily matched?
  obs.prob <- NULL
  ## Which rows in 'WRDS.CRSP.Identifiers' contain candidate PERMNOs
  ## corresponding to 'obs.prob' that cannot be easily matched?
  PERMNO.prob.pos <- list()
  list.ctr <- 0
  for (i in seq_along(TOData[, 1])) {
    if (!is.na(TOData[i, str.SDC.CUSIP6])) { # Only deal with non-empty entries.
      positions.CUSIP <- # Matching CUSIP positions.
        grep(paste("^", TOData[i, str.SDC.CUSIP6], sep=""),
             WRDS.CRSP.Identifiers[, "NCUSIP"],
             perl = TRUE)
      positions.ticker <- # Matching ticker positions.
        which(TOData[i, str.SDC.ticker] ==
              WRDS.CRSP.Identifiers[, "TICKER"])
      ## Both CUSIP and ticker should match.
      positions <- intersect(positions.CUSIP, positions.ticker)
      if (length(unique(WRDS.CRSP.Identifiers[positions, "PERMNO"])) == 1) {
        TOData[i, str.SDC.PERMNO] <- # We're good.
          WRDS.CRSP.Identifiers[positions[1], "PERMNO"]
      } else if (length(positions.ticker) == 0 &&
                 length(unique(WRDS.CRSP.Identifiers[positions.CUSIP, "PERMNO"])) == 1) {
        TOData[i, str.SDC.PERMNO] <- # We're good.
          WRDS.CRSP.Identifiers[positions.CUSIP[1], "PERMNO"]
      } else {
        obs.prob <- append(obs.prob, i)
        PERMNO.prob.pos[[list.ctr <- list.ctr+1]] <- positions.CUSIP
        ## Print out some information.
        cat("Problems finding unique PERMNO in CRSP. Will later look at trading volume.\n")
        cat("i               ", i, "\n")
        cat("target          ", TOData[i, "Target_Name"], "\n")
        cat("acquirer        ", TOData[i, "Acquiror_Name"], "\n")
        cat("SDC CUSIP       ", TOData[i, str.SDC.CUSIP6], "\n")
        cat("SDC ticker      ", TOData[i, str.SDC.ticker], "\n")
        cat("Matching CUSIPs and tickers found in CRSP:\n")
        print(unique(WRDS.CRSP.Identifiers[positions.CUSIP, "NCUSIP"]))
        print(unique(WRDS.CRSP.Identifiers[positions.ticker, "TICKER"]))
        cat("Candidate PERMNOs:\n")
        print(unique(WRDS.CRSP.Identifiers[positions.CUSIP, "PERMNO"]))
        print(unique(WRDS.CRSP.Identifiers[union(positions.CUSIP, positions.ticker),
                                           "PERMNO"]))
        cat("\n")
        ## Result of visual inspection of this output when searching
        ## for acquirer PERMNO: It's OK to only save 'positions.CUSIP'
        ## in 'PERMNO.prob.pos' to deal with these problematic cases
        ## in this sample.
      }
    }
  }
  rm(i, list.ctr, positions, positions.CUSIP, positions.ticker) # Clean up.

  cat("----------------------\n\n")

  ## Pick the PERMNO from the security that has the highest trading
  ## volume.
  for (k in seq_along(obs.prob)) {
    i <- obs.prob[k]

    ## When did the takeover attempt end (successfully or
    ## unsuccessfully)?  In my SDC JMP sample, "Date_Effective" always
    ## has the same value as "Date_Effective_Unconditional", so it's OK
    ## to use either one of them.
    resolution.date <-
      if (!is.na(TOData[i, "Date_Effective"])) {
        TOData[i, "Date_Effective"]
      } else {
        TOData[i, "Date_Withdrawn"]
      }

    ## Obtain the vector 'PERMNOs.prob' that contains the problematic
    ## PERMNOs that could not be matched by the simplistic procedure
    ## above.
    PERMNO.positions <- PERMNO.prob.pos[[k]]
    PERMNOs.prob <- unique(WRDS.CRSP.Identifiers[PERMNO.positions, "PERMNO"])
    rm(PERMNO.positions) # Clean up.

    ## Find the entry in 'PERMNOs.prob' that has the highest average
    ## trading volume in the year prior to deal resolution.
    PERMNO.best <- PERMNOs.prob[1] # Candidate for PERMNO with highest volume.
    vol.PERMNO.best <- 0 # Volume of best candidate PERMNO. 
    for (PERMNO in PERMNOs.prob) {

      ## Obtain CRSP data for 'PERMNO'.
      statement <- paste("SELECT * FROM ",
                         table.name.CRSP,
                         " WHERE PERMNO='", PERMNO, "';", sep="")
      CRSP.df <- dbGetQuery(con.db, statement)
      for (j in col.Date.CRSP) # Fix dates.
        class(CRSP.df[, j]) <- "Date"
      rm(statement, j) # Clean up.

      ## Calculate the average trading volume over the last year prior to
      ## deal resolution.
      CRSP.df.relevant <-
        subset(CRSP.df, resolution.date-365 <= DATE & DATE <= resolution.date)
      vol.av <-
        if (nrow(CRSP.df.relevant) > 0) {
          mean(CRSP.df.relevant[, "VOL"], na.rm = TRUE)
        } else {
          0
        }
      if (is.na(vol.av))
        stop("'vol.va' is NA.")

      cat("Candidate PERMNOs (i, PERMNO, vol.av):", i, PERMNO, vol.av, "\n")

      ## Check whether we have found a better PERMNO candidate.
      if (vol.av > vol.PERMNO.best) {
        vol.PERMNO.best <- vol.av
        PERMNO.best <- PERMNO
      }
    }

    cat("\n")
    
    ## Enter the best PERMNO into SDC dataframe. If no PERMNO can be
    ## found, leave it as 'NA'.
    if (vol.PERMNO.best > 0) {
      TOData[i, str.SDC.PERMNO] <- PERMNO.best
    } else { # Can't find match in CRSP.
      cat("i               ", i, "\n")
      cat("resolution.date ", resolution.date, "\n")
      cat("target          ", TOData[i, "Target_Name"], "\n")
      cat("acquirer        ", TOData[i, "Acquiror_Name"], "\n")
      cat("SDC CUSIP       ", TOData[i, str.SDC.CUSIP6], "\n")
      cat("SDC ticker      ", TOData[i, str.SDC.ticker], "\n")
      cat("--> Cannot find matching PERMNO, so I leave entry as 'NA' (assuming that it had this value before being passed to this function.\n\n")
      ## Note that in case I wish to delete the observation, I cannot
      ## delete it from 'TOData' at this stage. Otherwise, the next
      ## iteration in the loop will be messed up. I have to save the
      ## rows to delete in a vector and delete it after the loop is
      ## finished.
    }
  }
  ## Clean up.
  rm(k, i, obs.prob, resolution.date,
     PERMNO.prob.pos, PERMNOs.prob, PERMNO.best,
     vol.PERMNO.best, vol.av)

  return(TOData)
} # Close function.

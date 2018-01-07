#######################################################################
##
## This function adds PERMCOs contained in 'WRDS.CRSP.Identifiers.PERMCO'
## to 'TOData' (from SDC with PERMNOs).  In case there is no PERMCO in CRSP
## during the time of the takeover, the PERMCO gets assigned 'NA' in
## 'TOData'.
##
#######################################################################

add.PERMCO <-
  function(TOData,                # SDC data
           str.SDC.CUSIP6,        # SDC column name for given CUSIP
           str.SDC.ticker,        # SDC column name for given ticker 
           str.SDC.PERMCO,        # SDC column name for PERMCO to be determined
           WRDS.CRSP.Identifiers.PERMCO) # Matching CUSIPs, tickers, PERMCOs
{

  ## Which rows in 'TOData' corresponds to multiple PERMCOs in
  ## 'WRDS.CRSP.Identifiers' that cannot be easily matched?
  obs.prob <- NULL
  ## Which rows in 'WRDS.CRSP.Identifiers.PERMCO' contain candidate PERMCOs
  ## corresponding to 'obs.prob' that cannot be easily matched?
  PERMCO.prob.pos <- list()
  list.ctr <- 0
  for (i in seq_along(TOData[, 1])) {
    if (!is.na(TOData[i, str.SDC.CUSIP6])) { # Only deal with non-empty entries.
      positions.CUSIP <- # Matching CUSIP positions.
        grep(paste("^", TOData[i, str.SDC.CUSIP6], sep=""),
             WRDS.CRSP.Identifiers.PERMCO[, "NCUSIP"],
             perl = TRUE)
      positions.ticker <- # Matching ticker positions.
        which(TOData[i, str.SDC.ticker] ==
              WRDS.CRSP.Identifiers.PERMCO[, "TICKER"])
      ## Both CUSIP and ticker should match.
      positions <- intersect(positions.CUSIP, positions.ticker)
      if (length(unique(WRDS.CRSP.Identifiers.PERMCO[positions, "PERMCO"])) == 1) {
        TOData[i, str.SDC.PERMCO] <- # We're good.
          WRDS.CRSP.Identifiers.PERMCO[positions[1], "PERMCO"]
      } else if (length(positions.ticker) == 0 &&
                 length(unique(WRDS.CRSP.Identifiers.PERMCO[positions.CUSIP, "PERMCO"])) == 1) {
        TOData[i, str.SDC.PERMCO] <- # We're good.
          WRDS.CRSP.Identifiers.PERMCO[positions.CUSIP[1], "PERMCO"]
      } else {
        obs.prob <- append(obs.prob, i)
        PERMCO.prob.pos[[list.ctr <- list.ctr+1]] <- positions.CUSIP
        ## Print out some information.
        cat("Problems finding unique PERMCO in CRSP. Will later look at trading volume.\n")
        cat("i               ", i, "\n")
        cat("target          ", TOData[i, "Target_Name"], "\n")
        cat("acquirer        ", TOData[i, "Acquiror_Name"], "\n")
        cat("SDC CUSIP       ", TOData[i, str.SDC.CUSIP6], "\n")
        cat("SDC ticker      ", TOData[i, str.SDC.ticker], "\n")
        cat("Matching CUSIPs and tickers found in CRSP:\n")
        print(unique(WRDS.CRSP.Identifiers.PERMCO[positions.CUSIP, "NCUSIP"]))
        print(unique(WRDS.CRSP.Identifiers.PERMCO[positions.ticker, "TICKER"]))
        cat("Candidate PERMCOs:\n")
        print(unique(WRDS.CRSP.Identifiers.PERMCO[positions.CUSIP, "PERMNO"]))
        print(unique(WRDS.CRSP.Identifiers.PERMCO[union(positions.CUSIP, positions.ticker),
                                           "PERMCO"]))
        cat("\n")
        ## Result of visual inspection of this output when searching
        ## for acquirer PERMCO: It's OK to only save 'positions.CUSIP'
        ## in 'PERMCO.prob.pos' to deal with these problematic cases
        ## in this sample.
      }
    }
  }
  rm(i, list.ctr, positions, positions.CUSIP, positions.ticker) # Clean up.

  cat("----------------------\n\n")

  ## Pick the PERMCO from the security that has the highest trading
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

    ## Obtain the vector 'PERMCOs.prob' that contains the problematic
    ## PERMCOs that could not be matched by the simplistic procedure
    ## above.
    PERMCO.positions <- PERMCO.prob.pos[[k]]
    PERMCOs.prob <- unique(WRDS.CRSP.Identifiers.PERMCO[PERMCO.positions, "PERMCO"])
    rm(PERMCO.positions) # Clean up.

    ## Find the entry in 'PERMCOs.prob' that has the highest average
    ## trading volume in the year prior to deal resolution.
    PERMCO.best <- PERMCOs.prob[1] # Candidate for PERMCO with highest volume.
    vol.PERMCO.best <- 0 # Volume of best candidate PERMCO. 
    for (PERMCO in PERMCOs.prob) {

      ## Obtain CRSP data for 'PERMCO'.
      statement <- paste("SELECT * FROM ",
                         table.name.CRSP,
                         " WHERE PERMCO='", PERMCO, "';", sep="")
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

      cat("Candidate PERMCOs (i, PERMCO, vol.av):", i, PERMCO, vol.av, "\n")

      ## Check whether we have found a better PERMCO candidate.
      if (vol.av > vol.PERMCO.best) {
        vol.PERMCO.best <- vol.av
        PERMCO.best <- PERMCO
      }
    }

    cat("\n")
    
    ## Enter the best PERMCO into SDC dataframe. If no PERMCO can be
    ## found, leave it as 'NA'.
    if (vol.PERMCO.best > 0) {
      TOData[i, str.SDC.PERMCO] <- PERMCO.best
    } else { # Can't find match in CRSP.
      cat("i               ", i, "\n")
      cat("resolution.date ", resolution.date, "\n")
      cat("target          ", TOData[i, "Target_Name"], "\n")
      cat("acquirer        ", TOData[i, "Acquiror_Name"], "\n")
      cat("SDC CUSIP       ", TOData[i, str.SDC.CUSIP6], "\n")
      cat("SDC ticker      ", TOData[i, str.SDC.ticker], "\n")
      cat("--> Cannot find matching PERMCO, so I leave entry as 'NA' (assuming that it had this value before being passed to this function.\n\n")
      ## Note that in case I wish to delete the observation, I cannot
      ## delete it from 'TOData' at this stage. Otherwise, the next
      ## iteration in the loop will be messed up. I have to save the
      ## rows to delete in a vector and delete it after the loop is
      ## finished.
    }
  }
  ## Clean up.
  rm(k, i, obs.prob, resolution.date,
     PERMCO.prob.pos, PERMCOs.prob, PERMCO.best,
     vol.PERMCO.best, vol.av)

  return(TOData)
} # Close function.

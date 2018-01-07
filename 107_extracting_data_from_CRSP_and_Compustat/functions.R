returns <- function(PERMNO,             # character: PERMNO used to search CRSP.
                    DA, DRES,           # Date: Announcement date and resolution date of deal.
                    ds.om.beg = 1, ds.om.end = 0,
                                        # numeric: Days to omit after DA and before DRES.
                    Special.1st.ret = TRUE,
                                        # logical: Special treatment in 1st return
                                        #   TRUE: 1st ret = open-to-close ret on 1st day
                                        #   FALSE: all returns are close-to-prev.close
                    df.include,         # single-row data.frame: Contains the information
                                        #   to be included in EVERY row of output data.frame.
                    df.row,             # single-row data.frame: (for checking only)
                                        #   Should contain CUSIP in first column!
                    table.name.CRSP = get('table.name.CRSP', envir=.GlobalEnv),
                                        # character: Table name containing CRSP data.
                                        # (default assigned in .GlobalEnv by config_SQLite.R)
                    col.Date.CRSP = get('col.Date.CRSP', envir=.GlobalEnv),
                                        # character: Column names in CRSP that contain 'Date'.
                                        # (default assigned in .GlobalEnv by config_SQLite.R)
                    drv, con.db) {      # SQLiteDriver & SQLite Connection: Datebase essentials.
    ## ###################################################################
    ##
    ## This function calculates event-date returns for a given
    ## PERMNO. This function returns 'NA' if it can't calculate
    ## returns. The nice thing about this function is that it
    ## automatically includes delisting returns, which is relevant if
    ## the target is delisted because of the merger.
    ##
    ## 'df.row' is only used for sanity checks and for diagnostic
    ## output. Its first column should contain the CUSIP for the company
    ## whose returns we want to extract from CRSP.
    ##
    ## Returns a data.frame that at least contains the following fields:
    ##   $LOGRET: (numeric) aggregated daily log return for output. It could be NA
    ##            for dates outside 'pos.beg:pos.end', which is set by parameters
    ##            'ds.om.beg' and 'ds.om.end'.
    ##   $DATE: (Date) trading date
    ##   $DA.TD: (integer) number of trading date after DA
    ##   $DA.CD: (integer) number of calendar date after DA
    ##   $DRES.TD: (integer) number of trading date after DRES
    ##   $DRES.CD: (integer) number of calendar date after DRES
    ##   $LOGRET.CC: (numeric) previous-close to current-close daily log return
    ##   $LOGRET.OC: (numeric) same-day open to close daily log return
    ##   $LOGRET.DL: (numeric) delisting log return
    ##   $PRC: (numeric) closing price
    ##   $SHROUT: (numeric) unadjusted number of publicly held shares in thousands
    ## Additional fields in this data.frame depends on parameter 'df.include',
    ## but it should at least contains:
    ##   $dealID: (character) deal ID used in relating other databases
    ##
    ## Attentions:
    ## 1) Sometimes a "NA row" would be returned in situations like empty query result.
    ##    A "NA row" is identified by a NA $LOGRET with ONLY a SINGLE row on that $dealID.
    ## 2) As always, calculations (like 'sum') on $LOGRET should include na.rm = TRUE,
    ##    because NA may be returned even in normal cases.
    ##
    ## ###################################################################

    ## Check function arguments.
    if (!inherits(df.row, 'data.frame') || nrow(df.row) != 1 ||
        !inherits(df.include, 'data.frame') || nrow(df.include) != 1)
        stop("Wrong function argument(s).")

    ## "NA row" to be returned for any subsequent problem.
    NA.row <- data.frame(LOGRET = as.numeric(NA),
                         DATE = as.Date(NA),
                         DA.TD = as.integer(NA),
                         DA.CD = as.integer(NA),
                         DRES.TD = as.integer(NA),
                         DRES.CD = as.integer(NA),
                         LOGRET.CC = as.numeric(NA),
                         LOGRET.OC = as.numeric(NA),
                         LOGRET.DL = as.numeric(NA),
                         PRC = as.numeric(NA),
                         SHROUT = as.numeric(NA) )
    NA.row <- cbind(NA.row, df.include)

    ## NA PERMNO is now accepted, in which case a "NA row" would be returned
    if(is.na(PERMNO)) {
        return(NA.row)
    }

    ## Query CRSP. Note that the 'DATE' range is selected with a buffer of
    ## +/- 14 calendar days. It allows storage of other information before DA
    ## or after DRES (e.g. information of factiva classification).
    statement <- paste("SELECT * FROM ",
                       table.name.CRSP,
                       " WHERE PERMNO='", PERMNO,
                       "' AND DATE>='", as.numeric(DA) - 14,
                       "' AND DATE<='", as.numeric(DRES) + 14,
                       "' ORDER BY DATE ASC;", sep = "")
    dfCR <- dbGetQuery(con.db, statement)

    ## Check if result of query is empty. If so, return NA.
    if (nrow(dfCR) == 0) {
        cat("\nNo data in CRSP. Returning 'NA row'.\n")
        print(df.row)
        return(NA.row)
    }

    ## Fix dates.
    for (j in intersect(col.Date.CRSP, colnames(dfCR)))
        dfCR[, j] <- as.Date(dfCR[, j])

    ## Select equivalent fields such that record conflict is strictly prohibited.
    dfCR <- dfCR[, c("DATE", "NCUSIP", "RET", "DLRET", "PRC", "SHROUT", "OPENPRC")]
    i <- which(duplicated(dfCR))
    if(length(i) > 0) {
        i.to.remove <- NULL
        ## As duplicated() for data.frame is not perfect, check one by one again
        ## using identical() to make sure that there's no conflict.
        for(j in i) {
            temp1 <- dfCR[j, ]; temp2 <- dfCR[j-1, ]
            row.names(temp1) <- NULL; row.names(temp2) <- NULL
            stopifnot(identical(temp1, temp2))
            i.to.remove <- c(i.to.remove, j)
        }
        dfCR <- dfCR[-i.to.remove, ]
        rm(j, i.to.remove, temp1, temp2)
    }
    rm(i)


    ## Throw error for any duplicated CRSP date.
    stopifnot(anyDuplicated(dfCR$DATE) == 0, all(is.finite(dfCR$DATE)))

    ## Numbering of Calendar Days AFTER DA and DRES.
    dfCR$DA.CD <- as.integer(dfCR$DATE) - as.integer(DA)
    dfCR$DRES.CD <- as.integer(dfCR$DATE) - as.integer(DRES)

    ## Numbering of Trading Days AFTER DA and DRES.
    i.DA <- which.min(abs(as.numeric(dfCR$DATE) - as.numeric(DA)))
    dfCR$DA.TD <- 1:nrow(dfCR) - i.DA
    if (dfCR$DATE[i.DA] > DA) {
        ## cat('\n', i.DA, ' later than DA=', format(DA), '\n')
        dfCR$DA.TD[i.DA:nrow(dfCR)] <- dfCR$DA.TD[i.DA:nrow(dfCR)] + 1
    } else if (dfCR$DATE[i.DA] < DA ) {
        ## cat('\n', i.DA, ' earlier than DA=', format(DA), '\n')
        dfCR$DA.TD[1:i.DA] <- dfCR$DA.TD[1:i.DA] - 1
    }
    rm(i.DA)

    i.DRES <- which.min(abs(as.numeric(dfCR$DATE) - as.numeric(DRES)))
    dfCR$DRES.TD <- 1:nrow(dfCR) - i.DRES
    if (dfCR$DATE[i.DRES] > DRES) {
        ## cat('\n', i.DRES, ' later than DRES=', format(DRES), '\n')
        dfCR$DRES.TD[i.DRES:nrow(dfCR)] <- dfCR$DRES.TD[i.DRES:nrow(dfCR)] + 1
    } else if (dfCR$DATE[i.DRES] < DRES ) {
        ## cat('\n', i.DRES, ' earlier than DRES=', format(DRES), '\n')
        dfCR$DRES.TD[1:i.DRES] <- dfCR$DRES.TD[1:i.DRES] - 1
    }
    rm(i.DRES)

    ## Only keep data between DA.TD >= -7 and DRES.TD <= 7.
    dfCR <- dfCR[which(dfCR$DA.TD >= -7 & dfCR$DRES.TD <= 7), ]

    ## Find next trading date on or after the announcement date, and
    ## omit 'ds.om.beg' days after the announcement date.
    pos.beg <- head(which(dfCR$DA.TD >= ds.om.beg), 1)
    ## pos.beg <- head(which(dfCR$DATE >= DA + ds.om.beg), 1)

    ## Check whether we were able to locate a date ON or past 'DA+ds.om.beg'.
    ## If not, skip this deal because the deal's time interval is too short.
    if (length(pos.beg)==0 )             # Detects if which() returned integer(0) before head().
        return(NA.row)

    ## Find last trading date on or before resolution date, and omit
    ## 'ds.om.end' days prior to the resolution date.
    pos.end <- tail(which(-dfCR$DRES.TD >= ds.om.end), 1)
    ## pos.end <- tail(which(dfCR$DATE <= DRES - ds.om.end), 1)

    ## Check whether we were able to locate a date on or before 'DRES-ds.om.end'.
    ## If not, skip this deal because the deal's time interval is too short
    if(length(pos.end)==0 )             # Detects if which() returned integer(0) before tail().
        return(NA.row)

    ## Check if omitting-criteria met finally
    if (pos.beg > pos.end)
        return(NA.row)

    ## Sanity check tests whether the CUSIPs are the same in SDC and
    ## CRSP. Even if CUSIPs don't match, as long as company names are
    ## approximately same, it should be OK. (You should manually check
    ## the output).
    if (!all(grepl("^", rep(paste(df.row[, 1], sep = ""), nrow(dfCR)), dfCR[, "NCUSIP"]))) {
        cat("\nCUSIPs in SDC and CRSP don't coincide.\n", sep = "")
        print(df.row)
        print(dfCR[1, c("COMNAM", "NCUSIP")])
    }

    ## Sanity check for returns.
    if (any(dfCR$RET == -1, na.rm = TRUE) || any(dfCR$DLRET == -1, na.rm = TRUE))
        stop("You should rethink how you deal with log-returns since log(0) is undefined.")
    if (any(dfCR$RET < -1, na.rm = TRUE) || any(dfCR$DLRET < -1, na.rm = TRUE))
        stop("CRSP returns (which are not log-returns and thus cannot be smaller than -1) ",
             "have missing values (i.e. return<1). ",
             "See CRSP variable definitions for more details.")

    ## Calculate log-returns. They are more useful for calculating
    ## cumulative returns at a later stage.
    dfCR$LOGRET.CC <- log(1+dfCR$RET)
    ## dfCR$logret <- log(1+dfCR$RET)

    ## Same day Open to Close return
    dfCR$LOGRET.OC <- log(abs(dfCR$PRC) / abs(dfCR$OPENPRC) )

    ## Log return to be used directly: it may compose of an initial OC return,
    ## then some CC returns, and may end with a CC + DL return.
    ## To faciliate easy call of sum(), LOGRET outside pos.beg:pos.end are left NA.
    dfCR$LOGRET <- as.numeric(NA)
    dfCR$LOGRET[pos.beg:pos.end] <- dfCR$LOGRET.CC[pos.beg:pos.end]

    ## Special treatment to 1st day: report same-day open-to-close return rather than
    ## normally previous-day-close to current-close return.
    if (Special.1st.ret) {
        ## ## Directly record in $logret[1] only if everything is fine.
        ## if(is.finite(dfCR$OPENPRC[1]) && is.finite(dfCR$PRC[1])) {
        ##     dfCR$logret[1] <- log(dfCR$PRC[1] / dfCR$OPENPRC[1])
        ## }

        ## Directly record in $LOGRET[pos.beg] only if it's finite.
        if(is.finite(dfCR$LOGRET.OC[pos.beg])) {
            dfCR$LOGRET[pos.beg] <- dfCR$LOGRET.OC[pos.beg]
        }
    }

    ## Add delisting log-return, if available. In case there are several
    ## delisting returns, use the latest one.
    ## *Note* This version still contains problem for successful merger that
    ## still traded AFTER DRES.
    dfCR$LOGRET.DL <- log(1+dfCR$DLRET) # Delisting log-return.
    ## dfCR$DLlogret <- log(1+dfCR$DLRET) # Delisting log-return.

    if (any(is.finite(dfCR$LOGRET.DL))) { # Check whether there is any delisting return.
        pos <- max(which(is.finite(dfCR$LOGRET.DL)))

        ## Should ADD but NOT be REPLACED by delisting return according to:
        ## www.crsp.com/documentation/product/stkind/definitions/Delisting_Return.html
        dfCR$LOGRET[pos] <- ifelse(is.finite(dfCR$LOGRET[pos]),
                                   dfCR$LOGRET[pos] + dfCR$LOGRET.DL[pos],
                                   dfCR$LOGRET.DL[pos])
    }


    ## Return everything in a data.frame.
    output <- NA.row              # Prepare the data.frame for output.
    if(nrow(dfCR) > 1) {
      for(i in 2:nrow(dfCR)) {
        output <- rbind(output, NA.row)
      }
    }

    output$LOGRET <- dfCR$LOGRET
    output$DATE <- dfCR$DATE
    output$DA.TD <- dfCR$DA.TD
    output$DA.CD <- dfCR$DA.CD
    output$DRES.TD <- dfCR$DRES.TD
    output$DRES.CD <- dfCR$DRES.CD
    output$LOGRET.CC <- dfCR$LOGRET.CC
    output$LOGRET.OC <- dfCR$LOGRET.OC
    output$LOGRET.DL <- dfCR$LOGRET.DL
    output$PRC <- abs(dfCR$PRC)
    output$SHROUT <- dfCR$SHROUT

    row.names(output) <- NULL
    return(output)
}


######################################################################
## This function queries SQLite to obtain all the data from
## CCM/Compustat that pertains to a given 'PERMNO'. If there is no
## entry in CCM/Compustat, this functions uses 'CUSIP9' as the search
## variable instead. 'date.cutoff' is needed to help determine whether
## PERMNO or CUSIP9 should be used.
######################################################################

query.CCM <- function(PERMNO) {

  ## Get data from SQLite and fix dates.
  statement <- paste("SELECT * FROM ", table.name.CCM.PERMNO,
                     " WHERE lpermno='", PERMNO, "';", sep="")
  CCM.df <- dbGetQuery(con.db, statement)
  CCM.df <- convert.CCM.df(CCM.df)

  ## ## Deal with potential duplicate 'datadate' entries. These could
  ## ## occur if a company changes its fiscal period end for reporting
  ## ## purposes in the middle of the fiscal period itself. See 'fyr',
  ## ## 'fqtr', 'fyearq', 'DATACQTR', and 'DATAFQTR' in addition to
  ## ## 'datadate'.
  ## if (nrow(CCM.df) > 0 & anyDuplicated(CCM.df$datadate) > 0) {
  ##   print(CCM.df[, c("lpermno", "cusip", "conm", "fyr", "fqtr", "FYRC",
  ##                    col.DateYr.CCM, col.Date.CCM, col.DateQtr.CCM)])
  ##   browser()
  ##   stop("Duplicate dates for a single firm.")
  ## }

  return(CCM.df)
}



######################################################################
## This function searches 'CCM.df' for the latest non-NA entry on or
## before 'date.cutoff' of the CCM variable named
## 'name.CCM.variable'. If it can't find a matching entry, it searches
## 'CCM.df' for the next non-NA entry after 'date.cutoff'. It returns
## the CCM variable in question.
######################################################################

return.matching.CCM.entry <- function(CCM.df,
                                      date.cutoff,
                                      name.CCM.variable) {

  if (any(CCM.df$lpermno != CCM.df$lpermno[1])) {
    stop("Compustat data.frame contains more than one company.")
  }
  stopifnot(nrow(CCM.df) > 0,
            class(date.cutoff) == "Date",
            class(name.CCM.variable) == "character",
            length(name.CCM.variable) == 1)

  ## Extract 'datadate' column and the column containing the variable
  ## of interest for the dates on or before the cutoff date. Then
  ## remove rows containing NAs. Note that for sorting, 'datadate' is
  ## put into the first column.
  CCM.cutoff.df <- na.omit(CCM.df[which(CCM.df[, "datadate"] <= date.cutoff),
                                  c("datadate", name.CCM.variable)])

  if (nrow(CCM.cutoff.df) > 0) { # It was possible to find some entries in CCM.

    ## Deal with potential duplicate 'datadate' entries. These could
    ## occur if a company changes its fiscal period end for reporting
    ## purposes in the middle of the fiscal period itself. See 'fyr',
    ## 'fqtr', 'fyearq', 'DATACQTR', and 'DATAFQTR' in addition to
    ## 'datadate'.
    if (anyDuplicated(CCM.cutoff.df$datadate) > 0) {
      pos <- which(duplicated(CCM.cutoff.df$datadate))
      ## Check whether the variable 'name.CCM.variable' has different
      ## values for different times.
      for (i in pos) {
        dupl <- sort(which(CCM.cutoff.df$datadate == CCM.cutoff.df$datadate[i]))
        hlp <- CCM.cutoff.df[dupl, name.CCM.variable]
        if (any(hlp != hlp[1])) { # Check if entries differ.
          if (length(hlp) == 2) { # It's probably a change in fiscal year.
            newval <- mean(hlp) # Take average value.
            stopifnot(!is.null(newval), is.finite(newval)) # Sanity check.
            ## Note that the following entry will not be removed later
            ## on. (We only remove those positions where a duplicate
            ## occurs, but not where the entry occurs first.)
            CCM.cutoff.df[dupl[1], name.CCM.variable] <- newval
            ## Sanity check whether it's really not going to be removed.
            stopifnot(!dupl[1] %in% pos)
          } else { # It's not a change in fiscal year and we should investigate.
            print(CCM.df[, c("lpermno", "cusip", "conm", "fyr", "fqtr", "FYRC",
                             col.DateYr.CCM, col.Date.CCM, col.DateQtr.CCM)])
            print(CCM.cutoff.df)
            ## browser()
            stop("Duplicate entries in CCM/Compustat.")
          }
        }
      }
      CCM.cutoff.df <- CCM.cutoff.df[-pos, ] # Remove duplicates.
    }

    ## Sort along "datadate".
    CCM.cutoff.df <- CCM.cutoff.df[do.call(order, CCM.cutoff.df), ]
    k <- nrow(CCM.cutoff.df) # Start with last date.
    ## Find next non-NA entry.
    while (is.na(CCM.cutoff.df[k, name.CCM.variable]))
      k <- k - 1
    if (k >= 1) {
      return(CCM.cutoff.df[k, name.CCM.variable])
    } else {
      stop("Can't find suitable entry in CCM.")
    }
  } else { # This case occurs if there is no data in 'CCM.df' before 'date.cutoff'.
    ## Keep class and, if class is 'factor', the contrasts.
    ret <- CCM.df[1, name.CCM.variable]
    is.na(ret) <- TRUE
    return(ret)
  }
  ## else { # Find next-largest date.
  ##   cat("Potential problem: Can't find non-forward-looking date in CCM.\n")
  ##   ## Extract variables of interest from time-unrestricted
  ##   ## data.frame.
  ##   CCM.df <- CCM.df[, c("datadate", name.CCM.variable)]
  ##   ## Sort along "datadate".
  ##   CCM.df <- CCM.df[do.call(order, CCM.df), ]
  ##   return(CCM.df[1, name.CCM.variable])
  ## } # Closing 'else'.

  ## This function should never end up here because previously it
  ## either should have returned something or it should have been
  ## stopped.
  stop("Something is wrong with this function.")
}





add.CCM.to.takeover.df <- function(takeover.df,
                                   name.takeover.df.variable,
                                   target,
                                   name.CCM.variable) {

  ## ##################################################################
  ## This function cycles through all takeover observations for which
  ## a PERMNO exists. It assigns the variable 'name.CCM.variable' from
  ## CCM/Compustat. 'target' is a logical value indicating whether the
  ## data is for a target or acquirer
  ## firm. 'name.takeover.df.variable' is the new name of the takeover
  ## data.frame column that is being added.
  ##
  ## This script assumes that the data to be extracted from Compustat
  ## is of class 'numeric'. In case you want to extract data that is
  ## of other class, you should modify this function (and maybe the
  ## functions called by this function).
  ## ##################################################################

  stopifnot(class(takeover.df) == "data.frame",
            nrow(takeover.df) > 0,
            class(target) == "logical")

  ## Add new column to takeover data.frame that has the column name
  ## 'name.takeover.df.variable'.
  NAs <- numeric(nrow(takeover.df))
  is.na(NAs) <- TRUE
  NAs <- as.data.frame(NAs, stringsAsFactors = FALSE)
  names(NAs) <- name.takeover.df.variable
  takeover.df <- cbind(takeover.df, NAs, stringsAsFactors = FALSE)
  rm(NAs) # Clean up.

  ## Find accounting information from CCM/Compustat that is valid on the
  ## announcement date.
  rows <-
    if (target) {
      which(!is.na(takeover.df[, "TPERMNO"]))
    } else {
      which(!is.na(takeover.df[, "APERMNO"]))
    }
  for (pos.deal in rows) {
    ## cat("Extracting ", name.CCM.variable,
    ##     " for observation ", pos.deal, "\n", sep="")

    DA <- takeover.df[pos.deal, "DA"]
    DRES <- takeover.df[pos.deal, "DRES"]

    PERMNO <-
      if (target) {
        takeover.df[pos.deal, "TPERMNO"]
      } else {
        takeover.df[pos.deal, "APERMNO"]
      }
    ## CUSIP9 <-
    ##   if (target) {
    ##     takeover.df[pos.deal, "TCUSIP9"]
    ##   } else {
    ##     takeover.df[pos.deal, "ACUSIP9"]
    ##   }

    ## Query SQLite for 'PERMNO'.
    CCM.df <- query.CCM(PERMNO)

    if (nrow(CCM.df) > 0) { # Try to extract information from Compustat.
      takeover.df[pos.deal, name.takeover.df.variable] <-
        return.matching.CCM.entry(CCM.df = CCM.df,
                                  date.cutoff = DA,
                                  name.CCM.variable = name.CCM.variable)
    }
  } # Close for loop.

  return(takeover.df)
}



extract.CCM <- function(name.CCM.variable,  # Compustat variable name to extract.
                        target,             # does it pertain to the target (or acquirer).
                        sdc) {              # data.frame that contains takeover data.
  ## ##################################################################
  ##
  ## This function generates a suitable variable name for use in the
  ## 'sdc' data.frame. It then adds variables taken from Compustat for
  ## both the target and the acquirer.
  ##
  ## ##################################################################

  ## Verify function arguments.
  stopifnot(class(name.CCM.variable) == "character",
            length(name.CCM.variable) == 1,
            class(target) == "logical",
            class(sdc) == "data.frame",
            nrow(sdc) > 0)

  ## Create name for inclusion in data.frame containing deals.
  name.SDC.variable <-
    if (target) {
      paste("Target.", name.CCM.variable, ".CCM", sep = "")
    } else {
      paste("Acquiror.", name.CCM.variable, ".CCM", sep = "")
    }
  ## Add the variable from Compustat to 'sdc' and return the resulting
  ## data.frame.
  return(add.CCM.to.takeover.df(sdc,
                                name.SDC.variable,
                                target,
                                name.CCM.variable))
}

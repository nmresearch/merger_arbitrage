TAret <- function(Tret, # data.frame containing CRSP returns and
                        # essential SDC information for target.
                  Aret, # data.frame for acquirer (same structure as Tret)
                  factiva) {  # data.frame containing article classification.   
  dealID.set <- sort(unique(Tret$dealID));

  ## Check consistency between Tret and Aret.
  stopifnot(identical( dealID.set, sort(unique(Aret$dealID)) ) )

  ## Ouptut data.frame to be expanded by rbind() in the main for() loop.
  output.all <- NULL
  
  for (dealID in dealID.set) {
    ## cat(dealID, '\n')

    ## First extract currently relevant subsets by 'dealID'.
    Tret.curr <- Tret[which(Tret$dealID == dealID ), ]
    Aret.curr <- Aret[which(Aret$dealID == dealID ), ]
    factiva.curr <- factiva[which(factiva$dealID == dealID), ]
    factiva.curr <- factiva.curr[which(is.finite(factiva.curr$classification)), ]    
    ## Simply skip this dealID as there's nothing to be handled.
    if (all(!is.finite(Tret.curr$LOGRET)) )  {
      ## cat("\n dealID=", dealID,
      ##     " No finite LOGRET found in Tret",
      ##     "\n This deal is SKIPPED. \n")
      next;
    }

    ## Map 'Aret.curr' into 'Tret.curr'. Most of the time they should have the
    ## same set of 'DATE'. But if the sets are not identical, follow the set of
    ## 'Tret.curr'. In this case, NA is padded to 'Aret.curr' if needed (which
    ## would be caught by the checks following).
    if (!identical(Tret.curr$DATE, Aret.curr$DATE) ) {
      i <- match(Tret.curr$DATE, Aret.curr$DATE)
      Aret.curr <- Aret.curr[i, ]
      Aret.curr$DATE <- Tret.curr$DATE
      rm(i)
    }
    ## Assing zero to EXRATIO of a cash merger.
    if (!is.na(Tret.curr[1, "EXRATIO"])) {
      Tret.curr$EXRATIO <- Tret.curr[1, "EXRATIO"]
    } else {
      Tret.curr$EXRATIO <- 0
    }
    ## Calculate the exchange ratio when EXRATIO=0, yet COLRATIO is finite.
    ExchangeRatio <- 0
    if (Tret.curr[1, "SWAP"] == "Yes"){
      if (!is.finite(Tret.curr[1, "EXRATIO"])){
        if (!is.finite(Tret.curr[1, "COLRATIOH"]) || !is.finite(Tret.curr[1, "COLRATIOL"])){
          ExchangeRatio <- 0
        } else {
          ExchangeRatio <- mean(c(Tret.curr[1, "COLRATIOH"], Tret.curr[1, "COLRATIOL"]))
        }
      } else {
        ExchangeRatio <- Tret.curr[1, "EXRATIO"]
      }
    }
   
    ## Warns if there's any NA 'LOGRET' in 'Aret.curr' but finite in 'Tret.curr'
    ## Cases found with dealID = 'gxwwvuqc', 'hqinsasa', 'roonefiv'.
    if(Tret.curr$EXRATIO[1] != 0) {
      i <- which(is.finite(Tret.curr$LOGRET))
      if (any(!is.finite(Aret.curr$LOGRET[i])) ) {
        cat("\n dealID=", dealID,
            " Aret contains NA LOGRET which are finite in Tret on:\n",
            format(Aret.curr$DATE[which(!is.finite(Aret.curr$LOGRET) & is.finite(Tret.curr$LOGRET))]),
            "\n These 'LOGRET.LongShort' would be 'NA' \n")
      }
      rm(i)
    }
    ## Construct media measures with lag1. 
    Tret.curr$Content.lag1 <- as.numeric(NA)
    Tret.curr$Coverage.lag1 <- as.numeric(NA)
    i.Tret <- findInterval(factiva.curr$Date, Tret.curr$DATE)
    i.Tret <- i.Tret + 1
    if (length(i.Tret) > 0 ) {
      i.Tret.set <- sort(unique(i.Tret[which(is.finite(i.Tret) & i.Tret<=nrow(Tret.curr)) ]  ))
      for (i.Tret.this in i.Tret.set ) {
        i.factiva <- which(i.Tret == i.Tret.this)
        ## Could be inspected by:
        ## factiva.curr$Date[i.factiva]; Tret.curr$DATE[i.Tret.this]
        Tret.curr$Content.lag1[i.Tret.this] <- mean(factiva.curr$classification[i.factiva])
        Tret.curr$Coverage.lag1[i.Tret.this] <- length(i.factiva)
      }
      rm(i.Tret.set, i.Tret.this, i.factiva)
    }
    rm(i.Tret)
    ## Construct the contemporaneous media measures.
    Tret.curr$Content <- as.numeric(NA)
    Tret.curr$Coverage <- as.numeric(NA)
    i.Tret <- findInterval(factiva.curr$Date, Tret.curr$DATE+1)
    i.Tret <- i.Tret + 1
    if (length(i.Tret) > 0 ) {
      i.Tret.set <- sort(unique(i.Tret[which(is.finite(i.Tret) & i.Tret<=nrow(Tret.curr)) ]  ))
      for (i.Tret.this in i.Tret.set ) {
        i.factiva <- which(i.Tret == i.Tret.this)
        ## Could be inspected by:
        ## factiva.curr$Date[i.factiva]; Tret.curr$DATE[i.Tret.this]
        Tret.curr$Content[i.Tret.this] <- mean(factiva.curr$classification[i.factiva])
        Tret.curr$Coverage[i.Tret.this] <- length(i.factiva)
      }
      rm(i.Tret.this, i.factiva)
    }
    rm(i.Tret)

    ## Combine 'Tret.curr' and 'Aret.curr' into 'output' data.frame
    ## Then add the fields 'VALUE', 'Content.prior', 'Coverage.prior',
    ## 'ExchangeRatio' which are held constant in all rows.
    names(Aret.curr) <- paste('A.', names(Aret.curr), sep='')
    output <- cbind(Tret.curr, Aret.curr)
    output$ExchangeRatio <- ExchangeRatio
    rm(Tret.curr, Aret.curr, ExchangeRatio)
        
    ## Calculate adjusted acquirer return, where 'A.LOGRET.adj' is adjusted
    ## for the deal's exchange ExchangeRatio.
    output$A.LOGRET.adj1 <- output$A.LOGRET * output$EXRATIO
    output$A.LOGRET.adj2 <- output$A.LOGRET * output$ExchangeRatio
    
    ## Log return for Long-target Short-acquirer only for stock merger
    if (output$EXRATIO[1] > 0) {
      output$LOGRET.LongShort1 <- output$LOGRET - output$A.LOGRET.adj1
    } else {
      output$LOGRET.LongShort1 <- output$LOGRET
    }
    ## Log return for Long-target Short-acquirer only for stock merger
    if (output$ExchangeRatio[1] > 0) {
      output$LOGRET.LongShort2 <- output$LOGRET - output$A.LOGRET.adj2
    } else {
      output$LOGRET.LongShort2 <- output$LOGRET
    }
    
    ## Trim 'output' for relevant fields in appropiate order.
    ## Then combine with the whole 'output.all' data.frame.
    output <- output[, c("dealID", "DATE", "DA.TD", "DA.CD",
                         "LOGRET", "A.LOGRET",
                         "LOGRET.LongShort1", "LOGRET.LongShort2",
                         "Content", "Coverage",
                         "Content.lag1", "Coverage.lag1",
                         "EXRATIO", "ExchangeRatio") ]
    output.all <- rbind(output.all, output)
    
    rm(output); gc()
  }
  
  row.names(output.all) <- NULL
  return(output.all)
  
}


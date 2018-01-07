combine.TAret.factiva <- function(Tret, # data.frame containing CRSP returns and
                                        # essential SDC information for target.
                                  Aret, # data.frame for acquirer (same structure as Tret).
                                  factiva) { # data.frame from Factiva including
                                        # press article classification.
    ## ####################################################################
    ## (former function buildRet() ).
    ## This function maps information of Aret (the acquire returns data.frame)
    ## and factiva (the press article classification data.frame) onto the dates
    ## of Tret (the target returns data.frame).
    ##
    ## Output fields:
    ##   $dealID: character, ID of the deal.
    ##   $LOGRET: numeric, daily log return of target.
    ##   $LOGRET.LongShort: numeric, when $ExchangeRatio>0, the daily log return
    ##                      of long-target and short-acquirer.
    ##                      *Note* Same as $LOGRET when $ExchangeRatio is 0.
    ##   $Content.prior: numeric, average magnitude of media classification between
    ##                   $DA.TD==-8 and $DA.TD==0.
    ##   $Coverage.prior: numeric, average number of media mentioning between
    ##                    $DA.TD==-8 and $DA.TD==0.
    ##   $Content.lag0: numeric, average magnitude of media classification
    ##                           AFTER previous $DATE, and ON or before $DATE.
    ##   $Coverage.lag0: numeric, average number of media mentioning
    ##                            AFTER previous $DATE, and ON or before $DATE.
    ##   $Content.lag1: numeric, average magnitude of media classification
    ##                           ON or after previous $DATE, and BEFORE $DATE.
    ##   $Coverage.lag1: numeric, average number of media mentioning
    ##                            ON or after previous $DATE, and BEFORE $DATE.
    ##   $DATE: Date, trading date of target.
    ##   $DA.TD: numeric, number of trading days after DA.
    ##   $DA.CD: numeric, number of calendar days after DA.
    ##   $DRES.TD: numeric, number of trading days after DRES.
    ##   $DRES.CD: numeric, number of calendar days after DRES.
    ##   $VALUE: numeric, trading day closing market value in thousand dollars.
    ##   $A.LOGREG: numeric, daily log return of acquirer.
    ##   $ExchangeRatio: numeric, effective exchange ratio for stock mergers.
    ##                   It should be 0 for cash mergers.
    ##   $A.LOGREG.adj: numeric, acquire daily log return adjusted for $ExchangeRatio.
    ##
    ## *Note* only $LOGRET, $Content.prior, $Coverage.prior, $DATE are
    ##       'output' fields currently. But fields like $LOGRET.LongShort, $Content
    ##       and $Coverage may also be used as output in future design.
    ##       Other fields are just informative and may be useful in debugging.
    ## ####################################################################

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

        ## Calculate the exchange ratio (a cash merger will be assigned an
        ## exchange ratio of zero).
        ## *Note* From now on 'ExchangeRatio' is assumed to contain all cash/stock merger
        ## related information.
        ExchangeRatio <- 0
        if (Tret.curr[1, "SWAP"] == "Yes"){
            if (!is.finite(Tret.curr[1, "EXRATIO"])){
                if (!is.finite(Tret.curr[1, "COLRATIOH"]) || !is.finite(Tret.curr[1, "COLRATIOL"])){
                    ExchangeRatio <- 0
                } else {
                    ExchangeRatio <- (Tret.curr[1, "COLRATIOH"] + Tret.curr[1, "COLRATIOL"]) / 2
                }
            } else {
                ExchangeRatio <- Tret.curr[1, "EXRATIO"]
            }
        }

        ## Warns if there's any NA 'LOGRET' in 'Aret.curr' but finite in 'Tret.curr'.
        ## Cases found with dealID 'gxwwvuqc', 'hqinsasa', 'roonefiv'.
        if(ExchangeRatio != 0) {
            i <- which(is.finite(Tret.curr$LOGRET))
            if (any(!is.finite(Aret.curr$LOGRET[i])) ) {
                cat("\n dealID=", dealID,
                    " Aret contains NA LOGRET which are finite in Tret on:\n",
                    format(Aret.curr$DATE[which(!is.finite(Aret.curr$LOGRET) & is.finite(Tret.curr$LOGRET))]),
                    "\n These 'LOGRET.LongShort' would be 'NA' \n")
            }
            rm(i)
        }

        ## Remove all non-finite Factiva classifications so as to simply the codes following.
        factive.curr <- factiva.curr[which(is.finite(factiva.curr$classification)), ]

        ## Calculate the average classification and the media coverage from
        ## 8 to 1 days before the 1st trading day after DA (i.e. 'DA.TA==1').
        ## These summaries are called '.prior' to distinguish from the daily summary.
        ## !!! DANGEROUS !!! The following logic works only for 'Tret.01.00' or
        ## 'Tret.00.00', i.e., NOT works if 'ds.om.beg' >= 2 in 107xxx.RData
        DA.TD1 <- Tret.curr$DATE[which(Tret.curr$DA.TD == 1)];
        if (length(DA.TD1) == 0 ) {
            Content.prior <- as.numeric(NA)
            Coverage.prior <- as.numeric(NA)
        } else {
            i <- which(factiva.curr$Date >= DA.TD1 - 8 &
                       factiva.curr$Date <= DA.TD1 - 1)
            Content.prior <- ifelse(length(i) == 0,
                                    as.numeric(NA),
                                    mean(factiva.curr$classification[i]))
            Coverage.prior <- length(i)

            rm(i)
        }
        rm(DA.TD1)

        ## #####################################################################
        ## Daily summary of Factiva information into 'Tret.curr'.

        ## Media information lagged by 1 day.
        ## Includes media info. FROM previous trading day to 1 day BEFORE current trading day.
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

        ## Instantaneous media information, i.e. lagged by 0 day.
        ## Includes media info. AFTER previous trading day TO current trading day.
        Tret.curr$Content.lag0 <- as.numeric(NA)
        Tret.curr$Coverage.lag0 <- as.numeric(NA)
        i.Tret<- findInterval(factiva.curr$Date, Tret.curr$DATE + 1)
        i.Tret <- i.Tret + 1
        if (length(i.Tret) > 0 ) {
            i.Tret.set <- sort(unique(i.Tret[which(is.finite(i.Tret) & i.Tret<=nrow(Tret.curr)) ]  ))
            for (i.Tret.this in i.Tret.set ) {
                i.factiva <- which(i.Tret == i.Tret.this)
                ## Could be inspected by:
                ## factiva.curr$Date[i.factiva]; Tret.curr$DATE[i.Tret.this]
                Tret.curr$Content.lag0[i.Tret.this] <- mean(factiva.curr$classification[i.factiva])
                Tret.curr$Coverage.lag0[i.Tret.this] <- length(i.factiva)
            }
            rm(i.Tret.this, i.factiva)
        }
        rm(i.Tret)

        ## #####################################################################

        ## Combine 'Tret.curr' and 'Aret.curr' into 'output' data.frame
        ## Then add the fields 'VALUE', 'Content.prior', 'Coverage.prior',
        ## 'ExchangeRatio' which are held constant in all rows.
        names(Aret.curr) <- paste('A.', names(Aret.curr), sep='')
        output <- cbind(Tret.curr, Aret.curr)
        output$VALUE <- output$PRC * output$SHROUT  # Market value of target.
        output$Content.prior <- Content.prior
        output$Coverage.prior <- Coverage.prior
        output$ExchangeRatio <- ExchangeRatio
        rm(Tret.curr, Aret.curr, factiva.curr, Content.prior, Coverage.prior, ExchangeRatio)

        ## Calculate adjusted acquirer return, where 'A.LOGRET.adj' is adjusted
        ## for the deal's exchange ExchangeRatio.
        output$A.LOGRET.adj <- output$A.LOGRET * output$ExchangeRatio

        ## Log return for Long-target Short-acquirer only for stock merger
        if (output$ExchangeRatio[1] > 0) {
            output$LOGRET.LongShort <- output$LOGRET - output$A.LOGRET.adj
        } else {
            output$LOGRET.LongShort <- output$LOGRET
        }

        ## Trim 'output' for relevant fields in appropiate order.
        ## Then combine with the whole 'output.all' data.frame.
        output <- output[, c("dealID",
                             "LOGRET", "LOGRET.LongShort",
                             "Content.prior", "Coverage.prior",
                             "Content.lag0", "Coverage.lag0",
                             "Content.lag1", "Coverage.lag1",
                             "DATE", "DA.TD", "DA.CD", "DRES.TD", "DRES.CD",
                             "VALUE",
                             "A.LOGRET", "ExchangeRatio", "A.LOGRET.adj") ]
        output.all <- rbind(output.all, output)

        rm(output); gc()
    }

    row.names(output.all) <- NULL
    return(output.all)

}



map.TAret.factiva.FF <- function(ret,   # data.frame generated by combine.TAret.factiva()
                                 ff) {  # data.frame of Fama-French factor returns.
    ## ######################################################################
    ## This function maps the 'LOGRET' (target daily log return) onto the dates in ff.
    ## If there're multiple 'LOGRET's to be mapped on the same day, they're averaged
    ## by equal-weight (EW) or value-weight (VW).
    ##
    ## The same averaging procedure is done on 'LOGRET.LongShort'. To distinguish
    ## the averaged obtained from 'LOGRET' and 'LOGRET.LongShort', the results
    ## are named as 'LO' (Long target Only) and 'LS' (Long target, Short acquirer).
    ##
    ## The output data.frame contains the daily returns on any M&A activity days
    ## with the following new fields:
    ##   $EWLO: numeric, the equally weighted Long target Only daily log-return.
    ##   $VWLO: numeric, the value weighted Long target Only daily log-return.
    ##   $EWLS: numeric, the equally weighted Long target, Short acquirer daily log-return.
    ##   $VWLS: numeric, the value weighted Long target, Short acquirer daily log-return.
    ##
    ## *Note* 'LOGRET' may actually equal 'LOGRET.LongShort' when 'ExchangeRatio' is 0.
    ##        For details, please refer to 'combine.TAret.factiva()'.
    ## ######################################################################

    ## Initialize columns 'EWLO', 'VWLO', 'EWLS', and 'VWLS'.
    ff$EWLO <- ff$VWLO <- ff$EWLS <- ff$VWLS <- as.numeric(NA)

    ## Only remains the dates in 'ff' that have matches in 'ret'.
    i <- match(ff$date, ret$DATE)
    ff <- ff[which(is.finite(i),), ]
    if(nrow(ff)==0 ) return(ff)
    rm(i)

    ## Take appropiate average for duplicated 'ret$DATE' on the same 'ff$date'.
    i.ff <- match(ret$DATE, ff$date)
    for(i.ff.this in 1:nrow(ff) ) {
        i.ret <- which(i.ff == i.ff.this)
        ## all(ret$DATE[i.ret] == ff$date[i.ff.this] )  # Always TRUE.

        ff$EWLO[i.ff.this] <- mean(ret$LOGRET[i.ret], na.rm = TRUE)
        ff$VWLO[i.ff.this] <- weighted.mean(ret$LOGRET[i.ret], ret$VALUE[i.ret], na.rm = TRUE)

        ff$EWLS[i.ff.this] <- mean(ret$LOGRET.LongShort[i.ret], na.rm = TRUE)
        ff$VWLS[i.ff.this] <- weighted.mean(ret$LOGRET.LongShort[i.ret], ret$VALUE[i.ret], na.rm = TRUE)
    }
    rm(i.ff, i.ff.this, i.ret)

    ## Only take the subset where any of the output fields is finite.
    ff$EWLO[which(!is.finite(ff$EWLO))] <- NA
    ff$VWLO[which(!is.finite(ff$VWLO))] <- NA
    ff$EWLS[which(!is.finite(ff$EWLS))] <- NA
    ff$VWLS[which(!is.finite(ff$VWLS))] <- NA
    ff <- ff[which(is.finite(ff$EWLO) | is.finite(ff$VWLO) |
                   is.finite(ff$EWLS) | is.finite(ff$VWLS) ), ]

    return(ff)
}





TSRegression <- function(ff) {          # data.frame generated by map.TAret.factiva.FF()
    ## ###############################################################
    ## (former function calTSR() ).
    ## Carry out various form of regressions.
    ##
    ## The output picks regression results into an matrix of
    ## 'information' X 'regression'. The current version supports
    ## 3 kinds of information ('Alpha', 'p.value', 'adj.r.squared') and
    ## 8 kinds of regression.
    ## ###############################################################

    ## *Note* all lm() are bracketed inside try() to avoid error throwing with zero-number of
    ##        samples. (It happens for some heavily pruned 'ff' and especially in 'Low_EWLO' and
    ##        'Low_VWLO').

    ## CAPM using EWLO and VWLO respectively.
    CAPM_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log, data = ff), silent=TRUE)
    CAPM_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log, data = ff), silent=TRUE)

    ## Fama-French regression using EWLO and VWLO respectively.
    FF_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)
    FF_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)

    ## Piecewise CAPM-type using EWLO and VWLO respectively.
    High_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log, data = ff,
                      subset = which(ff$mktrf_log > 0.02435029)), silent=TRUE)
    Low_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log, data = ff,
                    subset = which(ff$mktrf_log <= 0.02435029)), silent=TRUE)
    High_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log, data = ff,
                    subset = which(ff$mktrf_log > -0.02805339)), silent=TRUE)
    Low_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log, data = ff,
                    subset = which(ff$mktrf_log <= -0.02805339)), silent=TRUE)

    ## CAPM using EWLS and VWLS respectively.
    CAPM_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log, data = ff), silent=TRUE)
    CAPM_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log, data = ff), silent=TRUE)

    ## Fama-French regression using EWLS and VWLS respectively.
    FF_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)
    FF_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)

    ## Piecewise CAPM-type using EWLS and VWLS respectively.
    High_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log, data = ff,
                      subset = which(ff$mktrf_log > -0.02126956)), silent=TRUE)
    Low_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log, data = ff,
                      subset = which(ff$mktrf_log <= -0.02126956)), silent=TRUE)
    High_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log, data = ff,
                      subset = which(ff$mktrf_log > -0.02050606)), silent=TRUE)
    Low_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log, data = ff,
                      subset = which(ff$mktrf_log <= -0.02050606)), silent=TRUE)


    ## All the following '_with_rf' means 'WITH Risk-Free', i.e., WITHOUT subtracting rf_log in LHS.

    ## CAPM using EWLO and VWLO respectively.
    CAPM_EWLO_with_rf <- try( lm(EWLO ~ mktrf_log, data = ff), silent=TRUE)
    CAPM_VWLO_with_rf <- try( lm(VWLO ~ mktrf_log, data = ff), silent=TRUE)

    ## Fama-French regression using EWLO and VWLO respectively.
    FF_EWLO_with_rf <- try( lm(EWLO ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)
    FF_VWLO_with_rf <- try( lm(VWLO ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)

    ## Piecewise CAPM-type using EWLO and VWLO respectively.
    High_EWLO_with_rf <- try( lm(EWLO ~ mktrf_log, data = ff,
                            subset = which(ff$mktrf_log > 0.02435029)), silent=TRUE)
    Low_EWLO_with_rf <- try( lm(EWLO ~ mktrf_log, data = ff,
                            subset = which(ff$mktrf_log <= 0.02435029)), silent=TRUE)
    High_VWLO_with_rf <- try( lm(VWLO ~ mktrf_log, data = ff,
                            subset = which(ff$mktrf_log > -0.02805339)), silent=TRUE)
    Low_VWLO_with_rf <- try( lm(VWLO ~ mktrf_log, data = ff,
                            subset = which(ff$mktrf_log <= -0.02805339)), silent=TRUE)

    ## CAPM using EWLS and VWLS respectively.
    CAPM_EWLS_with_rf <- try( lm(EWLS ~ mktrf_log, data = ff), silent=TRUE)
    CAPM_VWLS_with_rf <- try( lm(VWLS ~ mktrf_log, data = ff), silent=TRUE)

    ## Fama-French regression using EWLS and VWLS respectively.
    FF_EWLS_with_rf <- try( lm(EWLS ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)
    FF_VWLS_with_rf <- try( lm(VWLS ~ mktrf_log + smb_log + hml_log, data = ff), silent=TRUE)

    ## Piecewise CAPM-type using EWLS and VWLS respectively.
    High_EWLS_with_rf <- try( lm(EWLS ~ mktrf_log, data = ff,
                              subset = which(ff$mktrf_log > -0.02126956)), silent=TRUE)
    Low_EWLS_with_rf <- try( lm(EWLS ~ mktrf_log, data = ff,
                              subset = which(ff$mktrf_log <= -0.02126956)), silent=TRUE)
    High_VWLS_with_rf <- try( lm(VWLS ~ mktrf_log, data = ff,
                              subset = which(ff$mktrf_log > -0.02050606)), silent=TRUE)
    Low_VWLS_with_rf <- try( lm(VWLS ~ mktrf_log, data = ff,
                              subset = which(ff$mktrf_log <= -0.02050606)), silent=TRUE)

    ## Pick all information needed as a matrix for output
    rn <- c("Alpha", "p.value", "adj.r.squared")
    cn <- c("CAPM_EWLO", "CAPM_VWLO",
            "FF_EWLO", "FF_VWLO",
            "High_EWLO", "Low_EWLO",
            "High_VWLO", "Low_VWLO",

            "CAPM_EWLS", "CAPM_VWLS",
            "FF_EWLS", "FF_VWLS",
            "High_EWLS", "Low_EWLS",
            "High_VWLS", "Low_VWLS",

            "CAPM_EWLO_with_rf", "CAPM_VWLO_with_rf",
            "FF_EWLO_with_rf", "FF_VWLO_with_rf",
            "High_EWLO_with_rf", "Low_EWLO_with_rf",
            "High_VWLO_with_rf", "Low_VWLO_with_rf",

            "CAPM_EWLS_with_rf", "CAPM_VWLS_with_rf",
            "FF_EWLS_with_rf", "FF_VWLS_with_rf",
            "High_EWLS_with_rf", "Low_EWLS_with_rf",
            "High_VWLS_with_rf", "Low_VWLS_with_rf" )
    output <- matrix(nrow=length(rn), ncol=length(cn), dimnames=list(rn, cn))
    for(c in cn) {
        reg <- get(c)
        if (inherits(reg, "try-error")) next;   # Leave NA if lm() failed.

        s <- summary(reg)
        output["Alpha", c] <- coef(s)[1,1]
        output["p.value", c] <- coef(s)[1,4]
        output["adj.r.squared", c] <- s$adj.r.squared
    }

    return(output)
}


#########################################################################################
## The following are cost functions to be minimized during the determination
## of optimal threshold of daily excess log return.

Cost.EWLO <- function(threshold,   # numeric: The optimal threshold to be determined by optimize().
                      ff.mapped) { # data.frame generated by map.TAret.factiva.FF()
    ## ########################################################
    ## Total cost of piecewise CAPM regressions using EWLO.
    ## ########################################################

    ## Piecewise CAPM-type using EWLO.
    High_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log,
                         data = ff.mapped,
                         subset = which(ff.mapped$mktrf_log > threshold)), silent=TRUE)
    Low_EWLO <- try( lm((EWLO-rf_log) ~ mktrf_log,
                        data = ff.mapped,
                        subset = which(ff.mapped$mktrf_log <= threshold)), silent=TRUE)

    ## Use Sum of Squared Error (i.e. residual) to be the cost.
    ## Return NA intentionally if degree of freedom is too small.
    sse.High_EWLO <- ifelse(inherits(High_EWLO, "try-error"),
                            NA,
                            ifelse(High_EWLO$df.residual < 30,
                                   NA,
                                   sum(High_EWLO$residuals ^ 2) ) )
    sse.Low_EWLO <- ifelse(inherits(Low_EWLO, "try-error"),
                           NA,
                           ifelse(Low_EWLO$df.residual < 30,
                                  NA,
                                  sum(Low_EWLO$residuals ^ 2) ) )

    return(sse.High_EWLO + sse.Low_EWLO)
}


Cost.VWLO <- function(threshold,   # numeric: The optimal threshold to be determined by optimize().
                      ff.mapped) { # data.frame generated by map.TAret.factiva.FF()
    ## ########################################################
    ## Total cost of piecewise CAPM regressions using VWLO.
    ## ########################################################

    ## Piecewise CAPM-type using VWLO.
    High_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log,
                         data = ff.mapped,
                         subset = which(ff.mapped$mktrf_log > threshold)), silent=TRUE)
    Low_VWLO <- try( lm((VWLO-rf_log) ~ mktrf_log,
                        data = ff.mapped,
                        subset = which(ff.mapped$mktrf_log <= threshold)), silent=TRUE)

    ## Use Sum of Squared Error (i.e. residual) to be the cost.
    ## Return NA intentionally if degree of freedom is too small.
    sse.High_VWLO <- ifelse(inherits(High_VWLO, "try-error"),
                            NA,
                            ifelse(High_VWLO$df.residual < 30,
                                   NA,
                                   sum(High_VWLO$residuals ^ 2) ) )
    sse.Low_VWLO <- ifelse(inherits(Low_VWLO, "try-error"),
                           NA,
                           ifelse(Low_VWLO$df.residual < 30,
                                  NA,
                                  sum(Low_VWLO$residuals ^ 2) ) )

    return(sse.High_VWLO + sse.Low_VWLO)
}


Cost.EWLS <- function(threshold,   # numeric: The optimal threshold to be determined by optimize().
                      ff.mapped) { # data.frame generated by map.TAret.factiva.FF()
    ## ########################################################
    ## Total cost of piecewise CAPM regressions using EWLS.
    ## ########################################################

    ## Piecewise CAPM-type using EWLS.
    High_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log,
                         data = ff.mapped,
                         subset = which(ff.mapped$mktrf_log > threshold)), silent=TRUE)
    Low_EWLS <- try( lm((EWLS-rf_log) ~ mktrf_log,
                        data = ff.mapped,
                        subset = which(ff.mapped$mktrf_log <= threshold)), silent=TRUE)

    ## Use Sum of Squared Error (i.e. residual) to be the cost.
    ## Return NA intentionally if degree of freedom is too small.
    sse.High_EWLS <- ifelse(inherits(High_EWLS, "try-error"),
                            NA,
                            ifelse(High_EWLS$df.residual < 30,
                                   NA,
                                   sum(High_EWLS$residuals ^ 2) ) )
    sse.Low_EWLS <- ifelse(inherits(Low_EWLS, "try-error"),
                           NA,
                           ifelse(Low_EWLS$df.residual < 30,
                                  NA,
                                  sum(Low_EWLS$residuals ^ 2) ) )

    return(sse.High_EWLS + sse.Low_EWLS)
}


Cost.VWLS <- function(threshold,   # numeric: The optimal threshold to be determined by optimize().
                      ff.mapped) { # data.frame generated by map.TAret.factiva.FF()
    ## ########################################################
    ## Total cost of piecewise CAPM regressions using VWLS.
    ## ########################################################

    ## Piecewise CAPM-type using VWLS.
    High_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log,
                         data = ff.mapped,
                         subset = which(ff.mapped$mktrf_log > threshold)), silent=TRUE)
    Low_VWLS <- try( lm((VWLS-rf_log) ~ mktrf_log,
                        data = ff.mapped,
                        subset = which(ff.mapped$mktrf_log <= threshold)), silent=TRUE)

    ## Use Sum of Squared Error (i.e. residual) to be the cost.
    ## Return NA intentionally if degree of freedom is too small.
    sse.High_VWLS <- ifelse(inherits(High_VWLS, "try-error"),
                            NA,
                            ifelse(High_VWLS$df.residual < 30,
                                   NA,
                                   sum(High_VWLS$residuals ^ 2) ) )
    sse.Low_VWLS <- ifelse(inherits(Low_VWLS, "try-error"),
                           NA,
                           ifelse(Low_VWLS$df.residual < 30,
                                  NA,
                                  sum(Low_VWLS$residuals ^ 2) ) )

    return(sse.High_VWLS + sse.Low_VWLS)
}

#########################################################################################

#########################################################################
## This script amends SDC data based on CRSP data.
## *Note* This whole process should actually be done in 090 (where
## 'file.SDC.complete' was generated). But somehow these problems are
## discovered only by running 180 (which depends on output of 107/script_master_01.R),
## AH HOC amendments are made here.
##
## The amendments involves 3 parts (see the comments below for details).
## All amendments would be recorded in fields 'STAT_amended', 'DW_amended', 'DE_amended'
## and 'DRES_amended'. The original fields are NOT affected.
#########################################################################

rm(list=ls());
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "107_extracting_data_from_CRSP_and_Compustat")
## dir.base <- "~/RA"
## dir.base <- "C:/R/RA"
## dir.code <- file.path(dir.base, "working", "107_extracting_data_from_CRSP_and_Compustat")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))

## Load data.frame containing deals.
load(file.SDC.complete)
dfS <- takeoverData # Use easy-to-type name, where 'S' stands for SDC.
rm(takeoverData) # Avoid confusion and/or naming conflicts.

## Connect to SQLite.
x <- db.connect(file.dbName)
drv <- x[[1]]; con.db <- x[[2]]; rm(x)

#############################################
## Part 1: Correct DE and DRES for completed deals if the last trading day,
##         which is detected by 'PRC', 'RET' or 'DLRET', is later than stated

dfS$DRES_amended <- as.Date(NA)
dfS$DE_amended <- as.Date(NA)

for(i in which(dfS$STAT == "Completed"  & !is.na(dfS$TPERMNO) ) ) {
    cat(i, '\n')
    DRES <- dfS[i, "DRES"]
    statement <- paste("SELECT * FROM ",
                       table.name.CRSP,
                       " WHERE PERMNO='", dfS[i, "TPERMNO"],
                       "' AND DATE>='", as.numeric(DRES) - 14,
                       "' AND DATE<='", as.numeric(DRES) + 14,
                       "' ORDER BY DATE ASC;", sep = "")
    dfCR <- dbGetQuery(con.db, statement)
    if(nrow(dfCR) == 0) next;

    dfCR$DATE <- as.Date(dfCR$DATE)

    j <- max(which(is.finite(dfCR$PRC)),
             which(is.finite(dfCR$RET)),
             which(is.finite(dfCR$DLRET)) )
    if (is.finite(j) && dfCR$DATE[j] > DRES) {
        cat(i, dfS$TPERMNO[i],
            " found last trading day=", format(dfCR$DATE[j]),
            " later than DRES=", format(DRES), '\n')
        print(dfCR[,c('DATE','PRC','RET','DLRET')])

        dfS$DE_amended[i] <- dfCR$DATE[j]
        dfS$DRES_amended[i] <- dfCR$DATE[j]
    }
}


#############################################
## Part 2: From the results of Part 1, I found by MANUAL INSPECTION that there're 11 cases
##         which are actually 'Withdrawn' deals rather than 'Completed' deals. So, I reveal
##         the amendments made in Part 1, then make NEW amendment (for corrected 'STAT').

## These 11 cases are actually 'Withdrawn' deals:
i<-match(c('85046', '79428', '84174', '87388',  '21186', '77103', '83506', '86930',
           '87386', '89838', '41371'),
         dfS$TPERMNO)

dfS$STAT_amended<-dfS$STAT; dfS$STAT_amended[] <- NA
dfS$DW_amended <- as.Date(NA)

## Status before corrections.
dfS[i, c('STAT', 'DW','DE','DRES',
         'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')]

dfS$STAT_amended[i] <- "Withdrawn"
dfS$DW_amended[i] <- dfS$DE[i]
dfS$DE_amended[i] <- NA
dfS$DRES_amended[i] <- NA

## Status after corrections.
dfS[i, c('STAT', 'DW','DE','DRES',
         'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')]

## Some cross validations.
any(dfS$STAT=="Withdrawn" & !is.finite(dfS$DW), na.rm=TRUE) # FALSE
any(dfS$STAT_amended=="Withdrawn" & !is.finite(dfS$DW_amended), na.rm=TRUE) # FALSE
identical(dfS$DE_amended, dfS$DRES_amended) # TRUE
sum(is.finite(dfS$DRES_amended))    # 31
summary(as.numeric(dfS$DRES_amended) - as.numeric(dfS$DRES))
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
##  1.000   1.000   2.000   2.645   4.000   8.000 968.000
sort(dfS$TPERMNO[which(is.finite(dfS$DRES_amended))])
##  [1] "11138" "11214" "22430" "76371" "77291" "80252" "82551" "82804" "83433" "84111"
## [11] "84130" "84133" "84134" "85012" "85026" "85969" "86028" "87051" "87559" "87598"
## [21] "87639" "88180" "88440" "88478" "88948" "89033" "89622" "90546" "90860" "92459"
## [31] "92774"


#############################################
## Part 3. Checks if there's any 'DLRET' for 'Withdrawn' deals (until 'DRES+14' days). If so,
##         it should be a 'Completed' deal.

## Adopt to amended 'STAT' if any.
STAT <- dfS$STAT
STAT[which(!is.na(dfS$STAT_amended))] <- dfS$STAT_amended[which(!is.na(dfS$STAT_amended))]
sum(STAT != dfS$STAT)   # 11

for(i in which(STAT == "Withdrawn"  & !is.na(dfS$TPERMNO) ) ) {
    cat(i, '\n')
    DRES <- ifelse(is.na(dfS$DRES_amended[i]), dfS$DRES[i], dfS$DRES_amended[i])

    statement <- paste("SELECT * FROM ",
                       table.name.CRSP,
                       " WHERE PERMNO='", dfS[i, "TPERMNO"],
                       "' AND DATE>='", as.numeric(DRES) - 14,
                       "' AND DATE<='", as.numeric(DRES) + 14,
                       "' ORDER BY DATE ASC;", sep = "")
    dfCR <- dbGetQuery(con.db, statement)
    if(nrow(dfCR) == 0) next;

    dfCR$DATE <- as.Date(dfCR$DATE)

    j <- max(which(is.finite(dfCR$DLRET)) )
    if (is.finite(j) ) {
        cat(i, dfS$TPERMNO[i],
            " found finite DLRET on", format(dfCR$DATE[j]),
            " with STAT=='Withdrawn'!!! \nAmend STAT, DW, DE, DRES if needed.\n")
        print(dfCR[,c('DATE','PRC','RET','DLRET')])
        cat('from:  \n')
        print(dfS[i, c('TPERMNO',
                       'STAT', 'DW', 'DE', 'DRES',
                       'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')])

        ## Mark as 'Completed' deal that should not have 'DW'.
        dfS$STAT_amended[i] <- "Completed"
        dfS$DW_amended[i] <- NA

        ## Also amend 'DRES' if last trading day doesn't match.
        if (dfCR$DATE[j] != DRES) {
            dfS$DRES_amended[i] <- dfCR$DATE[j]
            dfS$DE_amended[i] <- dfS$DRES_amended[i]
        } else {
            dfS$DE_amended[i] <- dfS$DRES[i]
        }

        cat('to:  \n')
        print(dfS[i, c('TPERMNO',
                       'STAT', 'DW', 'DE', 'DRES',
                       'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')])
    }
}


##########################################################################
## All amendments are made. Now inspect these total 45 amended records.

i<-which(apply(dfS[, c('STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')],
      1, FUN=function(x) {!all(is.na(x))} ) )
dfS[i, c('TPERMNO', 'STAT', 'DW', 'DE', 'DRES', 'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')]
##     TPERMNO      STAT         DW         DE       DRES STAT_amended DW_amended DE_amended DRES_amended
## 26    85289 Withdrawn 2000-06-20       <NA> 2000-06-20    Completed       <NA> 2000-06-14   2000-06-14
## 79    85969 Completed       <NA> 2000-06-14 2000-06-14         <NA>       <NA> 2000-06-16   2000-06-16
## 128   82804 Completed       <NA> 2000-10-16 2000-10-16         <NA>       <NA> 2000-10-17   2000-10-17
## 131   57146 Withdrawn 2000-11-09       <NA> 2000-11-09    Completed       <NA> 2000-11-09         <NA>
## 147   84130 Completed       <NA> 2000-10-16 2000-10-16         <NA>       <NA> 2000-10-23   2000-10-23
## 170   22430 Completed       <NA> 2000-11-28 2000-11-28         <NA>       <NA> 2000-11-30   2000-11-30
## 200   85046 Completed       <NA> 2001-04-19 2001-04-19    Withdrawn 2001-04-19       <NA>         <NA>
## 215   85026 Completed       <NA> 2001-06-18 2001-06-18         <NA>       <NA> 2001-06-19   2001-06-19
## 220   79428 Completed       <NA> 2002-01-30 2002-01-30    Withdrawn 2002-01-30       <NA>         <NA>
## 244   84174 Completed       <NA> 2001-04-19 2001-04-19    Withdrawn 2001-04-19       <NA>         <NA>
## 245   86841 Withdrawn 2001-06-15       <NA> 2001-06-15    Completed       <NA> 2001-06-19   2001-06-19
## 281   87388 Completed       <NA> 2002-02-05 2002-02-05    Withdrawn 2002-02-05       <NA>         <NA>
## 288   11214 Completed       <NA> 2001-11-01 2001-11-01         <NA>       <NA> 2001-11-02   2001-11-02
## 297   21186 Completed       <NA> 2002-01-30 2002-01-30    Withdrawn 2002-01-30       <NA>         <NA>
## 300   77103 Completed       <NA> 2001-10-29 2001-10-29    Withdrawn 2001-10-29       <NA>         <NA>
## 331   77291 Completed       <NA> 2002-03-20 2002-03-20         <NA>       <NA> 2002-03-28   2002-03-28
## 353   89033 Completed       <NA> 2003-02-27 2003-02-27         <NA>       <NA> 2003-02-28   2003-02-28
## 384   83506 Completed       <NA> 2002-06-17 2002-06-17    Withdrawn 2002-06-17       <NA>         <NA>
## 418   87559 Completed       <NA> 2003-04-08 2003-04-08         <NA>       <NA> 2003-04-09   2003-04-09
## 422   85012 Completed       <NA> 2003-03-17 2003-03-17         <NA>       <NA> 2003-03-18   2003-03-18
## 470   86930 Completed       <NA> 2003-09-15 2003-09-15    Withdrawn 2003-09-15       <NA>         <NA>
## 474   86028 Completed       <NA> 2003-11-18 2003-11-18         <NA>       <NA> 2003-11-20   2003-11-20
## 494   11138 Completed       <NA> 2004-01-23 2004-01-23         <NA>       <NA> 2004-01-27   2004-01-27
## 523   87386 Completed       <NA> 2004-11-15 2004-11-15    Withdrawn 2004-11-15       <NA>         <NA>
## 585   84133 Completed       <NA> 2005-01-25 2005-01-25         <NA>       <NA> 2005-01-27   2005-01-27
## 621   88180 Completed       <NA> 2005-09-15 2005-09-15         <NA>       <NA> 2005-09-16   2005-09-16
## 688   88440 Completed       <NA> 2006-03-31 2006-03-31         <NA>       <NA> 2006-04-05   2006-04-05
## 711   83433 Completed       <NA> 2006-08-07 2006-08-07         <NA>       <NA> 2006-08-09   2006-08-09
## 773   87639 Completed       <NA> 2007-05-18 2007-05-18         <NA>       <NA> 2007-05-25   2007-05-25
## 782   84134 Completed       <NA> 2007-05-15 2007-05-15         <NA>       <NA> 2007-05-16   2007-05-16
## 787   84111 Completed       <NA> 2007-08-28 2007-08-28         <NA>       <NA> 2007-08-31   2007-08-31
## 789   82551 Completed       <NA> 2007-04-18 2007-04-18         <NA>       <NA> 2007-04-19   2007-04-19
## 792   80252 Completed       <NA> 2007-04-18 2007-04-18         <NA>       <NA> 2007-04-19   2007-04-19
## 793   88478 Completed       <NA> 2007-05-21 2007-05-21         <NA>       <NA> 2007-05-25   2007-05-25
## 815   89838 Completed       <NA> 2007-07-10 2007-07-10    Withdrawn 2007-07-10       <NA>         <NA>
## 826   87598 Completed       <NA> 2007-11-15 2007-11-15         <NA>       <NA> 2007-11-16   2007-11-16
## 835   88948 Completed       <NA> 2007-09-17 2007-09-17         <NA>       <NA> 2007-09-20   2007-09-20
## 853   90546 Completed       <NA> 2007-11-09 2007-11-09         <NA>       <NA> 2007-11-14   2007-11-14
## 855   41371 Completed       <NA> 2007-11-14 2007-11-14    Withdrawn 2007-11-14       <NA>         <NA>
## 864   90860 Completed       <NA> 2007-12-31 2007-12-31         <NA>       <NA> 2008-01-04   2008-01-04
## 874   89622 Completed       <NA> 2008-03-05 2008-03-05         <NA>       <NA> 2008-03-06   2008-03-06
## 877   87051 Completed       <NA> 2008-03-17 2008-03-17         <NA>       <NA> 2008-03-18   2008-03-18
## 951   92774 Completed       <NA> 2010-01-25 2010-01-25         <NA>       <NA> 2010-01-26   2010-01-26
## 966   92459 Completed       <NA> 2009-07-09 2009-07-09         <NA>       <NA> 2009-07-10   2009-07-10
## 987   76371 Completed       <NA> 2009-11-17 2009-11-17         <NA>       <NA> 2009-11-24   2009-11-24


## Save returns to file and write-protect it.
takeoverData <- dfS
save(takeoverData, file = file.SDC.amended)
Sys.chmod(file.SDC.amended, mode = "0400")


## Disconnect from SQLite.
db.disconnect(drv, con.db); rm(drv, con.db)

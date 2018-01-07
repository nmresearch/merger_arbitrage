#######################################################################
## 
## This script uses data from CCM and CRSP to add tickers and company
## names to the takeover data.
## 
#######################################################################


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "095_add_ticker_and_company_names_from_CCM_and_CRSP")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))

load(file.SDC.complete)
sdc <- takeoverData; rm(takeoverData)

## Connect to SQLite.
x <- db.connect(file.dbName)
drv <- x[[1]]; con.db <- x[[2]]; rm(x)


## Query CRSP.
cat("\nGetting target info from CRSP.\n")
sdc$TTickerCRSP <- rep(NA_character_, nrow(sdc))
sdc$TNameCRSP <- rep(NA_character_, nrow(sdc))
for (i in which(!is.na(sdc$TPERMNO))) {
  cat(i, "\n")
  x <- getTickNamCRSP(sdc[i, "TPERMNO"],
                      sdc[i, "DA"], sdc[i, "DRES"],
                      con.db, table.name.CRSP)
  sdc[i, "TTickerCRSP"] <- x[[1]]; sdc[i, "TNameCRSP"] <- x[[2]]
}
cat("\nGetting acquirer info from CRSP.\n")
sdc$ATickerCRSP <- rep(NA_character_, nrow(sdc))
sdc$ANameCRSP <- rep(NA_character_, nrow(sdc))
for (i in which(!is.na(sdc$APERMNO))) {
  cat(i, "\n")
  x <- getTickNamCRSP(sdc[i, "APERMNO"],
                      sdc[i, "DA"], sdc[i, "DRES"],
                      con.db, table.name.CRSP)
  sdc[i, "ATickerCRSP"] <- x[[1]]; sdc[i, "ANameCRSP"] <- x[[2]]
}

## Query Compustat.
cat("\nGetting target info from Compustat.\n")
sdc$TTickerCCM <- rep(NA_character_, nrow(sdc))
sdc$TNameCCM <- rep(NA_character_, nrow(sdc))
for (i in which(!is.na(sdc$TPERMNO))) {
  cat(i, "\n")
  x <- getTickNamCCM(sdc[i, "TPERMNO"],
                     sdc[i, "DA"], sdc[i, "DRES"],
                     con.db, table.name.CCM.PERMNO)
  sdc[i, "TTickerCCM"] <- x[[1]]; sdc[i, "TNameCCM"] <- x[[2]]
}
cat("\nGetting acquirer info from Compustat.\n")
sdc$ATickerCCM <- rep(NA_character_, nrow(sdc))
sdc$ANameCCM <- rep(NA_character_, nrow(sdc))
for (i in which(!is.na(sdc$APERMNO))) {
  cat(i, "\n")
  x <- getTickNamCCM(sdc[i, "APERMNO"],
                     sdc[i, "DA"], sdc[i, "DRES"],
                     con.db, table.name.CCM.PERMNO)
  sdc[i, "ATickerCCM"] <- x[[1]]; sdc[i, "ANameCCM"] <- x[[2]]
}



## Disconnect from SQLite.
db.disconnect(drv, con.db); rm(drv, con.db)

takeoverData <- sdc # Use more familiar name.
save(takeoverData, file = file.SDC.tickers.et.al)
Sys.chmod(file.SDC.tickers.et.al, mode = "0400")

#########################################################################
##
## For each takeover deal, this script extracts time series of
## log-returns. These log-returns include delisting returns.
##
## Important: Note that because of weekends or because trading was
## halted or because of data availability in CRSP, it is *not* always
## the case that for example Tret.01.00 is the same as Tret.00.00 with
## one day omitted at the beginning even if 'Special.1st.ret' is FALSE.
##
#########################################################################

rm(list=ls());
#dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
#dir.code <- file.path(dir.base, "merger-arbitrage", "107_extracting_data_from_CRSP_and_Compustat")
## dir.base <- "~/RA"
dir.base <- "C:/R/RA"
dir.code <- file.path(dir.base, "working", "107_extracting_data_from_CRSP_and_Compustat")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))

## Connect to SQLite.
x <- db.connect(file.dbName)
drv <- x[[1]]; con.db <- x[[2]]; rm(x)

## Load data.frame containing deals.
## load(file.SDC.complete)
load(file.SDC.amended)
dfS <- takeoverData # Use easy-to-type name, where 'S' stands for SDC.
rm(takeoverData) # Avoid confusion and/or naming conflicts.

## Adopt amended SDC data. See the 45 amended records pasted near the bottom of
## 'script_master_00.R' to understand how the following codes work.
dfS$STAT[which(!is.na(dfS$STAT_amended))] <- dfS$STAT_amended[which(!is.na(dfS$STAT_amended))]
dfS$DRES[which(!is.na(dfS$DRES_amended))] <- dfS$DRES_amended[which(!is.na(dfS$DRES_amended))]
dfS$DE[which(!is.na(dfS$DW_amended))] <- dfS$DE_amended[which(!is.na(dfS$DW_amended))]
dfS$DW[which(!is.na(dfS$DW_amended))] <- dfS$DW_amended[which(!is.na(dfS$DW_amended))]
dfS$DW[which(!is.na(dfS$DE_amended))] <- dfS$DW_amended[which(!is.na(dfS$DE_amended))]
dfS$DE[which(!is.na(dfS$DE_amended))] <- dfS$DE_amended[which(!is.na(dfS$DE_amended))]

i<-which(apply(dfS[, c('STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')],
      1, FUN=function(x) {!all(is.na(x))} ) )
dfS[i, c('TPERMNO', 'STAT', 'DW', 'DE', 'DRES', 'STAT_amended', 'DW_amended', 'DE_amended', 'DRES_amended')]

## -----------------

## Days to omit when extracting the returns, after the announcement
## date and before the resolution date.
ds.om.beg <- 0
ds.om.end <- 0

## Initialize data.frame that will contain log-returns.
Tret.00.00 <- NULL
Aret.00.00 <- NULL

## Extract log-returns.
for (i in seq_along(dfS[, 1])) {
     cat(i, '   Tret00   ', dim(Tret.00.00), '\n');
    Tret.00.00 <- rbind(Tret.00.00,
                        returns(PERMNO = dfS[i, "TPERMNO"],
                                DA = dfS[i, "DA"], DRES = dfS[i, "DRES"],
                                ds.om.beg = ds.om.beg, ds.om.end = ds.om.end,
                                Special.1st.ret = TRUE,
                                df.include = dfS[i, c("dealID",
                                                      "TCU", "TPERMNO", "TN",
                                                      "DA", "DRES",
                                                      "SWAP",
                                                      "EXRATIO",
                                                      "COLLAR",
                                                      "COLLAR_EXCHANGE_RATIO_FIXED",
                                                      "COLLAR_EXCHANGE_RATIO_FLOATING",
                                                      "COLRATIOL", "COLRATIOH")],
                                df.row = dfS[i, c("TCU", "TPERMNO", "TN", "DA", "DRES")],
                                drv = drv, con.db = con.db) )
    gc()                                # force gc() after rbind() to save memory
}
row.names(Tret.00.00) <- NULL

for (i in seq_along(dfS[, 1])) {
     cat(i, '   Aret00  ', dim(Aret.00.00), '\n');
    Aret.00.00 <- rbind(Aret.00.00,
                        returns(PERMNO = dfS[i, "APERMNO"],
                                DA = dfS[i, "DA"], DRES = dfS[i, "DRES"],
                                ds.om.beg = ds.om.beg, ds.om.end = ds.om.end,
                                Special.1st.ret = TRUE,
                                df.include = dfS[i, c("dealID",
                                                      "ACU", "APERMNO", "AN",
                                                      "DA", "DRES",
                                                      "SWAP",
                                                      "EXRATIO",
                                                      "COLLAR",
                                                      "COLLAR_EXCHANGE_RATIO_FIXED",
                                                      "COLLAR_EXCHANGE_RATIO_FLOATING",
                                                      "COLRATIOL", "COLRATIOH")],
                                df.row = dfS[i, c("ACU", "APERMNO", "AN", "DA", "DRES")],
                                drv = drv, con.db = con.db) )
    gc()                                # force gc() after rbind() to save memory
}
row.names(Aret.00.00) <- NULL

## Save returns to file and write-protect it.
save(Tret.00.00, Aret.00.00, file = file.returns.00.00)
Sys.chmod(file.returns.00.00, mode = "0400")

## -----------------



## -----------------
## Days to omit when extracting the returns, after the announcement
## date and before the resolution date.
ds.om.beg <- 1
ds.om.end <- 0

## Initialize data.frame that will contain log-returns.
Tret.01.00 <- NULL
Aret.01.00 <- NULL

## Extract log-returns.
for (i in seq_along(dfS[, 1])) {
     cat(i, '   Tret01  ', dim(Tret.01.00), '\n');
    Tret.01.00 <- rbind(Tret.01.00,
                        returns(PERMNO = dfS[i, "TPERMNO"],
                                DA = dfS[i, "DA"], DRES = dfS[i, "DRES"],
                                ds.om.beg = ds.om.beg, ds.om.end = ds.om.end,
                                Special.1st.ret = TRUE,
                                df.include = dfS[i, c("dealID",
                                                      "TCU", "TPERMNO", "TN",
                                                      "DA", "DRES",
                                                      "SWAP",
                                                      "EXRATIO",
                                                      "COLLAR",
                                                      "COLLAR_EXCHANGE_RATIO_FIXED",
                                                      "COLLAR_EXCHANGE_RATIO_FLOATING",
                                                      "COLRATIOL", "COLRATIOH")],
                                df.row = dfS[i, c("TCU", "TPERMNO", "TN", "DA", "DRES")],
                                drv = drv, con.db = con.db) )
    gc()                                # force gc() after rbind() to save memory
}
row.names(Tret.01.00) <- NULL

for (i in seq_along(dfS[, 1])) {
cat(i, '   Aret01  ', dim(Aret.01.00), '\n');
    Aret.01.00 <- rbind(Aret.01.00,
                        returns(PERMNO = dfS[i, "APERMNO"],
                                DA = dfS[i, "DA"], DRES = dfS[i, "DRES"],
                                ds.om.beg = ds.om.beg, ds.om.end = ds.om.end,
                                Special.1st.ret = TRUE,
                                df.include = dfS[i, c("dealID",
                                                      "ACU", "APERMNO", "AN",
                                                      "DA", "DRES",
                                                      "SWAP",
                                                      "EXRATIO",
                                                      "COLLAR",
                                                      "COLLAR_EXCHANGE_RATIO_FIXED",
                                                      "COLLAR_EXCHANGE_RATIO_FLOATING",
                                                      "COLRATIOL", "COLRATIOH")],
                                df.row = dfS[i, c("ACU", "APERMNO", "AN", "DA", "DRES")],
                                drv = drv, con.db = con.db) )
    gc()                            # force gc() after rbind() to save memory
}
row.names(Aret.01.00) <- NULL

## Save returns to file and write-protect it.
save(Tret.01.00, Aret.01.00, file = file.returns.01.00)
Sys.chmod(file.returns.01.00, mode = "0400")

## -----------------












## Disconnect from SQLite.
db.disconnect(drv, con.db); rm(drv, con.db)

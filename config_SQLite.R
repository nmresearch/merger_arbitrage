#######################################################################
##
## This file contains settings that are used throughout this research
## project for storing CRSP and CCM/Compustat data in SQLite. You
## should source this file somewhere at the beginning of your script.
##
#######################################################################

## Load some packages that are necessary.
library("RSQLite")
library("zoo")     # Need this package for dealing with 'yearqtr'.

## Database containing the imported data from WRDS (i.e. CRSP and
## CCM/Compustat).
## file.dbName <- file.path(dir.data, "020_030_CSRP_CCM.sqlite")
file.dbName <- file.path(dir.data, "020_030_CRSP_CCM.sqlite")
                                        # correct typo

## Names of SQLite tables.
table.name.CRSP <- "SQLTableCRSP"
table.name.CCM.PERMNO <- "SQLTableCCMPERMNO"


## CRSP columns that are converted to 'numeric' when reading from CSV
## file. After reading in the data from CSV, you won't have to worry
## about these columns again because SQLite can deal with numeric data
## types.
col.numeric.CRSP <- c("DIVAMT",
                      "FACPR",
                      "FACSHR",
                      "DLRETX",
                      "DLPRC",
                      "DLRET",
                      "SHROUT",
                      "MMCNT",
                      "BIDLO",
                      "ASKHI",
                      "PRC",
                      "VOL",
                      "RET",
                      "BID",
                      "ASK",
                      "CFACPR",
                      "CFACSHR",
                      "OPENPRC",
                      "NUMTRD",
                      "RETX",
                      "vwretd",
                      "vwretx",
                      "ewretd",
                      "ewretx",
                      "sprtrn")


## CRSP Columns that are of class 'Date' but are stored internatlly in
## SQLite as 'numeric'. Have to convert them back to Date after
## reading from SQLite, e.g. with 'for (i in col.Date.CRSP)
## {class(CRSP.df[, i]) <- "Date"}'. Maybe there is also a smarter way
## to do this with the SQLite date and time functions, but so far I
## have not yet looked into this.
col.Date.CRSP <- c("DATE",
                   "DCLRDT",
                   "DLPDT",
                   "NEXTDT",
                   "PAYDT",
                   "RCRDDT",
                   "NAMEENDT")



## Like with CRSP, this is only for reading in the CSV file. Don't
## have to worry about this later because SQLite can deal with numeric
## data types. The list below is from the printout of 'script_help.R'
## that has to be run separately.
col.numeric.CCM <- c("ADRRQ",
                     "CURRTRQ",
                     "CURUSCNQ",
                     "ACCHGQ",
                     "ACOMINCQ",
                     "ACOQ",
                     "ACTQ",
                     "ALTOQ",
                     "ANCQ",
                     "AOCIDERGLQ",
                     "AOCIOTHERQ",
                     "AOCIPENQ",
                     "AOCISECGLQ",
                     "AOQ",
                     "APQ",
                     "AQAQ",
                     "AQDQ",
                     "AQEPSQ",
                     "AQPQ",
                     "ARCEDQ",
                     "ARCEEPSQ",
                     "ARCEQ",
                     "ATQ",
                     "CAPSQ",
                     "CEQQ",
                     "CHEQ",
                     "CHQ",
                     "CIBEGNIQ",
                     "CICURRQ",
                     "CIDERGLQ",
                     "CIOTHERQ",
                     "CIPENQ",
                     "CISECGLQ",
                     "CITOTALQ",
                     "COGSQ",
                     "CSH12Q",
                     "CSHFDQ",
                     "CSHIQ",
                     "CSHOPQ",
                     "CSHOQ",
                     "CSHPRQ",
                     "CSTKEQ",
                     "CSTKQ",
                     "DCOMQ",
                     "DILADQ",
                     "DILAVQ",
                     "DLCQ",
                     "DLTTQ",
                     "DOQ",
                     "DPACREQ",
                     "DPACTQ",
                     "DPQ",
                     "DPRETQ",
                     "DRCQ",
                     "DRLTQ",
                     "DTEAQ",
                     "DTEDQ",
                     "DTEEPSQ",
                     "DTEPQ",
                     "DVPQ",
                     "EPSF12",
                     "EPSFIQ",
                     "EPSFXQ",
                     "EPSPIQ",
                     "EPSPXQ",
                     "EPSX12",
                     "FCAQ",
                     "FFOQ",
                     "GDWLAMQ",
                     "GDWLIA12",
                     "GDWLIAQ",
                     "GDWLID12",
                     "GDWLIDQ",
                     "GDWLIEPS12",
                     "GDWLIEPSQ",
                     "GDWLIPQ",
                     "GDWLQ",
                     "GLAQ",
                     "GLCEA12",
                     "GLCEAQ",
                     "GLCED12",
                     "GLCEDQ",
                     "GLCEEPS12",
                     "GLCEEPSQ",
                     "GLCEPQ",
                     "GLDQ",
                     "GLEPSQ",
                     "GLPQ",
                     "HEDGEGLQ",
                     "IBADJ12",
                     "IBADJQ",
                     "IBCOMQ",
                     "IBQ",
                     "ICAPTQ",
                     "INTACCQ",
                     "INTANOQ",
                     "INTANQ",
                     "INVFGQ",
                     "INVOQ",
                     "INVRMQ",
                     "INVTQ",
                     "INVWIPQ",
                     "IVAOQ",
                     "IVLTQ",
                     "IVSTQ",
                     "LCOQ",
                     "LCTQ",
                     "LLTQ",
                     "LOQ",
                     "LSEQ",
                     "LTMIBQ",
                     "LTQ",
                     "MIBQ",
                     "MIIQ",
                     "MSAQ",
                     "NIQ",
                     "NOPIQ",
                     "NRTXTDQ",
                     "NRTXTEPSQ",
                     "NRTXTQ",
                     "OEPF12",
                     "OEPS12",
                     "OEPSXQ",
                     "OIADPQ",
                     "OIBDPQ",
                     "OPEPSQ",
                     "OPTDRQ",
                     "OPTFVGRQ",
                     "OPTLIFEQ",
                     "OPTRFRQ",
                     "OPTVOLQ",
                     "PIQ",
                     "PNC12",
                     "PNCD12",
                     "PNCDQ",
                     "PNCEPS12",
                     "PNCEPSQ",
                     "PNCIAPQ",
                     "PNCIAQ",
                     "PNCIDPQ",
                     "PNCIDQ",
                     "PNCIEPSPQ",
                     "PNCIEPSQ",
                     "PNCIPPQ",
                     "PNCIPQ",
                     "PNCPD12",
                     "PNCPDQ",
                     "PNCPEPS12",
                     "PNCPEPSQ",
                     "PNCPQ",
                     "PNCQ",
                     "PNCWIAPQ",
                     "PNCWIAQ",
                     "PNCWIDPQ",
                     "PNCWIDQ",
                     "PNCWIEPQ",
                     "PNCWIEPSQ",
                     "PNCWIPPQ",
                     "PNCWIPQ",
                     "PNRSHOQ",
                     "PPEGTQ",
                     "PPENTQ",
                     "PRCAQ",
                     "PRCD12",
                     "PRCDQ",
                     "PRCE12",
                     "PRCEPS12",
                     "PRCEPSQ",
                     "PRCPD12",
                     "PRCPDQ",
                     "PRCPEPS12",
                     "PRCPEPSQ",
                     "PRCPQ",
                     "PRCRAQ",
                     "PRSHOQ",
                     "PSTKNQ",
                     "PSTKQ",
                     "PSTKRQ",
                     "RCAQ",
                     "RCDQ",
                     "RCEPSQ",
                     "RCPQ",
                     "RDIPAQ",
                     "RDIPDQ",
                     "RDIPEPSQ",
                     "RDIPQ",
                     "RECDQ",
                     "RECTAQ",
                     "RECTOQ",
                     "RECTQ",
                     "RECTRQ",
                     "RECUBQ",
                     "REQ",
                     "RETQ",
                     "REUNAQ",
                     "REVTQ",
                     "RRA12",
                     "RRAQ",
                     "RRD12",
                     "RRDQ",
                     "RREPS12",
                     "RREPSQ",
                     "RRPQ",
                     "SALEQ",
                     "SEQOQ",
                     "SEQQ",
                     "SETA12",
                     "SETAQ",
                     "SETD12",
                     "SETDQ",
                     "SETEPS12",
                     "SETEPSQ",
                     "SETPQ",
                     "SPCE12",
                     "SPCED12",
                     "SPCEDPQ",
                     "SPCEDQ",
                     "SPCEEPS12",
                     "SPCEEPSP12",
                     "SPCEEPSPQ",
                     "SPCEEPSQ",
                     "SPCEP12",
                     "SPCEPD12",
                     "SPCEPQ",
                     "SPCEQ",
                     "SPIDQ",
                     "SPIEPSQ",
                     "SPIOAQ",
                     "SPIOPQ",
                     "SPIQ",
                     "SRETQ",
                     "STKCOQ",
                     "STKCPAQ",
                     "TSTKNQ",
                     "TSTKQ",
                     "TXDBQ",
                     "TXDIQ",
                     "TXDITCQ",
                     "TXPQ",
                     "TXTQ",
                     "TXWQ",
                     "UACOQ",
                     "UAOQ",
                     "UAPTQ",
                     "UCAPSQ",
                     "UCCONSQ",
                     "UCEQQ",
                     "UDDQ",
                     "UDMBQ",
                     "UDOLTQ",
                     "UDPCOQ",
                     "UDVPQ",
                     "UGIQ",
                     "UINVQ",
                     "ULCOQ",
                     "UNIAMIQ",
                     "UNOPINCQ",
                     "UOPIQ",
                     "UPDVPQ",
                     "UPMPFQ",
                     "UPMPFSQ",
                     "UPMSUBPQ",
                     "UPSTKCQ",
                     "UPSTKQ",
                     "URECTQ",
                     "USUBDVPQ",
                     "USUBPCVQ",
                     "WCAPQ",
                     "WDAQ",
                     "WDDQ",
                     "WDEPSQ",
                     "WDPQ",
                     "XIDOQ",
                     "XINTQ",
                     "XIQ",
                     "XOPRQ",
                     "XOPT12",
                     "XOPTD12",
                     "XOPTD12P",
                     "XOPTDQ",
                     "XOPTDQP",
                     "XOPTEPS12",
                     "XOPTEPSP12",
                     "XOPTEPSQ",
                     "XOPTEPSQP",
                     "XOPTQ",
                     "XOPTQP",
                     "XRDQ",
                     "XSGAQ",
                     "ACCHGY",
                     "AOLOCHY",
                     "APALCHY",
                     "AQAY",
                     "AQCY",
                     "AQDY",
                     "AQEPSY",
                     "AQPY",
                     "ARCEDY",
                     "ARCEEPSY",
                     "ARCEY",
                     "CAPXY",
                     "CHECHY",
                     "CIBEGNIY",
                     "CICURRY",
                     "CIDERGLY",
                     "CIOTHERY",
                     "CIPENY",
                     "CISECGLY",
                     "CITOTALY",
                     "COGSY",
                     "CSHFDY",
                     "CSHPRY",
                     "CSTKEY",
                     "DILADY",
                     "DILAVY",
                     "DLCCHY",
                     "DLTISY",
                     "DLTRY",
                     "DOY",
                     "DPCY",
                     "DPRETY",
                     "DPY",
                     "DTEAY",
                     "DTEDY",
                     "DTEEPSY",
                     "DTEPY",
                     "DVPY",
                     "DVY",
                     "EPSFIY",
                     "EPSFXY",
                     "EPSPIY",
                     "EPSPXY",
                     "ESUBCY",
                     "EXREY",
                     "FCAY",
                     "FFOY",
                     "FIAOY",
                     "FINCFY",
                     "FOPOXY",
                     "FOPOY",
                     "FOPTY",
                     "FSRCOY",
                     "FUSEOY",
                     "GDWLAMY",
                     "GDWLIAY",
                     "GDWLIDY",
                     "GDWLIEPSY",
                     "GDWLIPY",
                     "GLAY",
                     "GLCEAY",
                     "GLCEDY",
                     "GLCEEPSY",
                     "GLCEPY",
                     "GLDY",
                     "GLEPSY",
                     "GLPY",
                     "HEDGEGLY",
                     "IBADJY",
                     "IBCOMY",
                     "IBCY",
                     "IBY",
                     "INTPNY",
                     "INVCHY",
                     "IVACOY",
                     "IVCHY",
                     "IVNCFY",
                     "IVSTCHY",
                     "MIIY",
                     "NIY",
                     "NOPIY",
                     "NRTXTDY",
                     "NRTXTEPSY",
                     "NRTXTY",
                     "OANCFY",
                     "OEPSXY",
                     "OIADPY",
                     "OIBDPY",
                     "OPEPSY",
                     "OPTDRY",
                     "OPTFVGRY",
                     "OPTLIFEY",
                     "OPTRFRY",
                     "OPTVOLY",
                     "PIY",
                     "PNCDY",
                     "PNCEPSY",
                     "PNCIAPY",
                     "PNCIAY",
                     "PNCIDPY",
                     "PNCIDY",
                     "PNCIEPSPY",
                     "PNCIEPSY",
                     "PNCIPPY",
                     "PNCIPY",
                     "PNCPDY",
                     "PNCPEPSY",
                     "PNCPY",
                     "PNCWIAPY",
                     "PNCWIAY",
                     "PNCWIDPY",
                     "PNCWIDY",
                     "PNCWIEPSY",
                     "PNCWIEPY",
                     "PNCWIPPY",
                     "PNCWIPY",
                     "PNCY",
                     "PRCAY",
                     "PRCDY",
                     "PRCEPSY",
                     "PRCPDY",
                     "PRCPEPSY",
                     "PRCPY",
                     "PRSTKCY",
                     "RCAY",
                     "RCDY",
                     "RCEPSY",
                     "RCPY",
                     "RDIPAY",
                     "RDIPDY",
                     "RDIPEPSY",
                     "RDIPY",
                     "RECCHY",
                     "REVTY",
                     "RRAY",
                     "RRDY",
                     "RREPSY",
                     "RRPY",
                     "SALEY",
                     "SETAY",
                     "SETDY",
                     "SETEPSY",
                     "SETPY",
                     "SIVY",
                     "SPCEDPY",
                     "SPCEDY",
                     "SPCEEPSPY",
                     "SPCEEPSY",
                     "SPCEPY",
                     "SPCEY",
                     "SPIDY",
                     "SPIEPSY",
                     "SPIOAY",
                     "SPIOPY",
                     "SPIY",
                     "SPPEY",
                     "SPPIVY",
                     "SRETY",
                     "SSTKY",
                     "STKCOY",
                     "STKCPAY",
                     "TXACHY",
                     "TXBCOFY",
                     "TXBCOY",
                     "TXDCY",
                     "TXDIY",
                     "TXPDY",
                     "TXTY",
                     "TXWY",
                     "UDVPY",
                     "UGIY",
                     "UNIAMIY",
                     "UNOPINCY",
                     "UPDVPY",
                     "USUBDVPY",
                     "WCAPCY",
                     "WDAY",
                     "WDDY",
                     "WDEPSY",
                     "WDPY",
                     "XIDOCY",
                     "XIDOY",
                     "XINTY",
                     "XIY",
                     "XOPRY",
                     "XOPTDQPY",
                     "XOPTDY",
                     "XOPTEPSQPY",
                     "XOPTEPSY",
                     "XOPTQPY",
                     "XOPTY",
                     "XRDY",
                     "XSGAY",
                     "CSHTRQ",
                     "DVPSPQ",
                     "DVPSXQ",
                     "MKVALTQ",
                     "PRCCQ",
                     "PRCHQ",
                     "PRCLQ",
                     "ADJEX")

## These CCM columns contain dates in the form
## '1997-03-31'. Internally they are stored in SQLite as 'numeric',
## just like with CRSP. After querying SQLite, you should convert them
## back to class 'Date', e.g. with 'for (i in col.Date.CCM)
## {class(CCM.df[, i]) <- "Date"}'
col.Date.CCM <-
  c("datadate", "rdq", "linkdt", "linkenddt", "dldte", "ipodate")

## These CCM columns contain dates in the form '1997 Q1'. (Actually,
## when downloading from WRDS, they are at the beginning in the form
## '1997Q2' in the CSV file, which should be converted using
## 'as.yearqtr("1997Q2", format = "%YQ%q")'.) Internally in SQLite
## they are stored as class 'character'. I also tried to store it
## internally in SQLite as 'numeric', but after querying SQLite later
## on, it (wrongly) returns first quarters only (thus effectively
## dropping the information about quarters). After querying SQLite,
## you should convert them back to class 'yearqtr' using e.g. 'for (i
## in col.DateQtr.CCM) {CCM.df[, i] <- as.yearqtr(CCM.df[, i])}'. You
## have to load the 'zoo' package for dealing with 'yearqtr'.
col.DateQtr.CCM <- c("DATACQTR", "DATAFQTR")

## These CCM columns contain dates in the form '1997'. They are stored
## as 'numeric'.
col.DateYr.CCM <- "fyearq"








#######################################################################
##
## This function connects to the SQLite backend.
##
#######################################################################
db.connect <- function(file.dbName) {
  drv <- dbDriver("SQLite") # Load SQLite driver.
  con.db <- dbConnect(drv, file.dbName) # Connect to the SQLite database.
  return(list(drv, con.db))
}

#######################################################################
##
## This function disconnects from the database backend.
##
#######################################################################
db.disconnect <- function(drv, con.db) {
  if (dbDisconnect(con.db)) { # Disconnect from database.
    rm(con.db)
  } else {
    stop("Failed to disconnect from database connection 'con'.")
  }
  if (dbUnloadDriver(drv)) { # Unload database driver.
    rm(drv)
  } else {
    stop("Failed to unload database driver 'drv'.")
  }
  return(TRUE)
}

#######################################################################
##
## This function converts the classes resulting from a SQLite query
## about some CCM/Compustat data.
##
#######################################################################
convert.CCM.df <- function(CCM.df) {

  for (k in intersect(col.Date.CCM, colnames(CCM.df)))
    class(CCM.df[, k]) <- "Date"
  for (k in intersect(col.DateQtr.CCM, colnames(CCM.df)))
    CCM.df[, k] <- as.yearqtr(CCM.df[, k])

  ## ## This should not be necessary, but since I'm updating
  ## ## 'col.numeric.CCM' step by step and maybe have not re-imported
  ## ## (and thus converted) the WRDS CSV file, it also won't hurt.
  ## for (k in col.numeric.CCM)
  ##   CCM.df[, k] <- as.numeric(CCM.df[, k])

  return(CCM.df)
}

#######################################################################
##
## This function converts the classes resulting from a SQLite query
## about some CRSP data.
##
#######################################################################
convert.CRSP.df <- function(CRSP.df) {

  for (k in intersect(col.Date.CRSP, colnames(CRSP.df)))
    class(CRSP.df[, k]) <- "Date"

  return(CRSP.df)
}

######################################################################
## 
## For each company name in SDC, this script tries to find a matching
## company name in CRSP. This is purely a sanity check to make sure
## that the matching between SDC and CRSP was successful. This script
## will not be useful for the econometric analysis itself.
## 
######################################################################


dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage",
                      "Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "060_consistency_check_for_company_names")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))


## Set up SQLite.
library("RSQLite")
drv <- dbDriver("SQLite") # Load SQLite driver.
con.db <- dbConnect(drv, file.dbName) # Connect to the SQLite database.

## Load data.frame 'SDC.data.PERMCO' and relabel it to the easier
## 'dfSDC'.
load(file.SDC.with.CUSIP9); rm(file.SDC.with.CUSIP9)
dfSDC <- SDC.data.PERMCO; rm(SDC.data.PERMCO)

## Remove duplicate periods in column names. Fix column names and set
## them to 'automatic'.
names(dfSDC) <-
  gsub("[.]+", ".", names(dfSDC), perl = TRUE)
row.names(dfSDC) <- NULL

cat("Dealing with the acquirer.\n")
dfSDC$AcNamCRSP <- getName(dfSDC, "Acquiror_PERMNO", "Date_Announced", con.db)
cat("Dealing with the target.\n")
dfSDC$TaNamCRSP <- getName(dfSDC, "Target_PERMNO", "Date_Announced", con.db)



## Save result to file.
save(dfSDC, file = file.SDC.with.CRSPNames)
Sys.chmod(file.SDC.with.CRSPNames, mode = "0400") # Write-protect file.



if (dbDisconnect(con.db)) { # Disconnect from database. 
  rm(con.db)
} else {
  stop("Failed to disconnect from database connection 'con.db'.")
}
if (dbUnloadDriver(drv)) { # Unload database driver.
  rm(drv)
} else {
  stop("Failed to unload database driver 'drv'.")
}

#######################################################################
##
## This script adds PERMNO to the job market paper SDC
## dataframe. If in doubt, it adds the PERMNO of the security that has
## the highest trading volume among candidate securities.
##
#######################################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "030_adding_PERMNO_to_SDC_data")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Set up SQLite.
library("RSQLite")
drv <- dbDriver("SQLite") # Load SQLite driver.
con.db <- dbConnect(drv, file.dbName) # Connect to the SQLite database.

## Load dataframe 'takeoverData'.
load(file.SDC.df.cleaned)
rm(file.SDC.df.cleaned)

## Load dataframe 'WRDS.CRSP.Identifiers'.
load(file.WRDS.CRSP.Identifiers.Converted)
rm(file.WRDS.CRSP.Identifiers.Converted)

## Remove duplicate periods in column names. Fix column names and set
## them to 'automatic'.
names(takeoverData) <-
  gsub("[.]+", ".", names(takeoverData), perl = TRUE)
row.names(takeoverData) <- NULL

## Add new columns containing 'NA's (will be later filled with
## content).
NAs <- character(nrow(takeoverData))
is.na(NAs) <- TRUE
SDC.data <- cbind(takeoverData,
                  "Acquiror_PERMNO" = NAs,
                  "Target_PERMNO" = NAs,
                  stringsAsFactors = FALSE)
rm(NAs, takeoverData) # Clean up.

SDC.data <- add.PERMNO(SDC.data,
                       "Acquiror_CUSIP",
                       "Acquiror_Primary_Ticker_Symbol",
                       "Acquiror_PERMNO", 
                       WRDS.CRSP.Identifiers)
SDC.data <- add.PERMNO(SDC.data,
                       "Target_CUSIP",
                       "Target_Primary_Ticker_Symbol",
                       "Target_PERMNO", 
                       WRDS.CRSP.Identifiers)


## Save result to file.
save(SDC.data, file = file.SDC.with.PERMNOs)
Sys.chmod(file.SDC.with.PERMNOs, mode = "0400") # Write-protect file.


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




#######################################################################
##
## This script adds PERMCO to the job market paper SDC
## dataframe with PERMNOs. 
##
#######################################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work/merger-arbitrage")
dir.code <- file.path(dir.base, "Code", "033_adding_PERMCO_to_SDC_data")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.base, "Code", "config_SQLite.R"))
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Set up SQLite.
library("RSQLite")
drv <- dbDriver("SQLite") # Load SQLite driver.
con.db <- dbConnect(drv, file.dbName) # Connect to the SQLite database.

## Load dataframe 'SDC.data'.
load(file.SDC.with.PERMNOs)
rm(file.SDC.with.PERMNOs)

## Load dataframe 'WRDS.CRSP.Identifiers.PERMCO'.
load(file.WRDS.CRSP.PERMCO.Converted)
rm(file.WRDS.CRSP.PERMCO.Converted)

## Remove duplicate periods in column names. Fix column names and set
## them to 'automatic'.
names(SDC.data) <-
  gsub("[.]+", ".", names(SDC.data), perl = TRUE)
row.names(SDC.data) <- NULL

## Add new columns containing 'NA's (will be later filled with
## content).
NAs <- character(nrow(SDC.data))
is.na(NAs) <- TRUE
SDC.data.PERMCO <- cbind(SDC.data,
                  "Acquiror_PERMCO" = NAs,
                  "Target_PERMCO" = NAs,
                  stringsAsFactors = FALSE)
rm(NAs, SDC.data) # Clean up.

SDC.data.PERMCO <- add.PERMCO(SDC.data.PERMCO,
                       "Acquiror_CUSIP",
                       "Acquiror_Primary_Ticker_Symbol",
                       "Acquiror_PERMCO", 
                       WRDS.CRSP.Identifiers.PERMCO)
SDC.data.PERMCO <- add.PERMCO(SDC.data.PERMCO,
                       "Target_CUSIP",
                       "Target_Primary_Ticker_Symbol",
                       "Target_PERMCO", 
                       WRDS.CRSP.Identifiers.PERMCO)


## Save result to file.
save(SDC.data.PERMCO, file = file.SDC.with.PERMCOs)
Sys.chmod(file.SDC.with.PERMCOs, mode = "0400") # Write-protect file.


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




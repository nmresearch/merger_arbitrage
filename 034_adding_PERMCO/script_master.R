#######################################################################
##
## This script adds PERMCO to the job market paper SDC
## dataframe with PERMNOs. 
##
#######################################################################

dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
dir.code <- file.path(dir.base, "merger-arbitrage", "034_adding_PERMCO")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

## Set up SQLite.
library("RSQLite")
drv <- dbDriver("SQLite") # Load SQLite driver.
con.db <- dbConnect(drv, file.dbName) # Connect to the SQLite database.

## Load dataframe 'SDC.data'.
load(file.SDC.with.PERMNOs)
rm(file.SDC.with.PERMNOs)

## Remove duplicate periods in column names. Fix column names and set
## them to 'automatic'.
names(SDC.data) <-
  gsub("[.]+", ".", names(SDC.data), perl = TRUE)
row.names(SDC.data) <- NULL

## Calculate the PERMCOs.
SDC.data.PERMCO <- SDC.data
cat("Dealing with acquirer.\n")
SDC.data.PERMCO$Acquiror_PERMCO <- getPERMCO(SDC.data, "Acquiror_PERMNO",
                                             table.name.CRSP, con.db)
cat("Dealing with target.\n")
SDC.data.PERMCO$Target_PERMCO <- getPERMCO(SDC.data, "Target_PERMNO",
                                           table.name.CRSP, con.db)

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

####################################################################
##
## This script adds accounting imformation from CCM to sdc dataframe
## and save as 'file.SDC.CCM'. It takes a while to run, and only needs
## to be done once, afterwards just load 'file.SDC.CCM' for further
## use.
##
####################################################################

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "107_extracting_data_from_CRSP_and_Compustat")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))

x <- db.connect(file.dbName)
drv <- x[[1]]; con.db <- x[[2]]; rm(x)

## Load data.frame containing deals.
load(file.SDC.complete)
sdc <- takeoverData # Use easy-to-type name.
rm(takeoverData) # Avoid confusion and/or naming conflicts.

## Cycle through all the variables that should be added, and then add
## them for both the target and the acquirer.
for (name.CCM.variable in var.to.add) {
  cat("Adding", name.CCM.variable, "to takeover data.frame.\n")
  for (target in c(TRUE, FALSE)){ # Get data for both target and acquirer.
    sdc <- extract.CCM(name.CCM.variable, target, sdc)
  }
}

## Save new data.frame to file.
takeoverData <- sdc # Use familiar name.
save(takeoverData, file = file.SDC.CCM)
Sys.chmod(file.SDC.CCM, mode = "0400") # Write-protect.

## Disconnect from SQLite.
db.disconnect(drv, con.db); rm(drv, con.db)

#####################################################################
##
## This is the master file for getting the data downloaded from WRDS
## into SQLite. Note that for CCM, you should use the quarterly (not
## the yearly) web screen to download data from WRDS.
##
#####################################################################


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "020_importing_WRDS_data")
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.base, "merger-arbitrage", "config_SQLite.R"))
source(file.path(dir.code, "config.R")) # Load configuration information.
source(file.path(dir.code, "functions.R")) # Load functions for later use.

## Connect to SQLite.
x <- db.connect(file.dbName)
drv <- x[[1]]; con.db <- x[[2]]; rm(x)


# Read WRDS data from CSV files into SQLite; you just have to do this
# once.
if (TRUE) {
  ## Read in CRSP data.
  create.db(file.csv = file.CRSP.csv,
            batch.size = batch.size,
            col.Date = col.Date.CRSP,
            col.numeric = col.numeric.CRSP,
            table.name = table.name.CRSP)
  ## Read in CCM data.
 create.db(file.csv = file.CCM.by.PERMNO.csv,
            batch.size = batch.size,
            col.Date = col.Date.CCM,
            col.DateQtr = col.DateQtr.CCM,
            col.numeric = union(col.numeric.CCM, col.DateYr.CCM),
            table.name = table.name.CCM.PERMNO)
}
rm(file.CRSP.csv, file.CCM.by.PERMNO.csv, batch.size)


## Consistency check.
if (FALSE) {
  ## Get first few CRSP entries to test whether the import worked.
  statement <- paste("SELECT * FROM ", table.name.CRSP, " LIMIT 8;", sep="")
  CRSP.part <- dbGetQuery(con.db, statement)
  for (i in col.Date.CRSP) # Fix dates.
    class(CRSP.part[, i]) <- "Date"

  cat("Printing non-character classes.\n")
  for (i in seq_along(CRSP.part[1, ])) {
    if (class(CRSP.part[, i]) != "character")
      cat(names(CRSP.part)[i], CRSP.part[1, i], class(CRSP.part[, i]), "\n")
  }

  ## Get first few CCM entries to test whether the import worked.
  statement <- paste("SELECT * FROM ", table.name.CCM.PERMNO, " LIMIT 8;", sep="")
  CCM.part <- dbGetQuery(con.db, statement)
  for (i in col.Date.CCM) # Fix dates.
    class(CCM.part[, i]) <- "Date"
  for (i in col.DateQtr.CCM) # Fix dates.
    CCM.part[, i] <- as.yearqtr(CCM.part[, i])
  
  cat("Printing non-character classes.\n")
  for (i in seq_along(CCM.part[1, ])) {
    if (class(CCM.part[, i]) != "character")
      cat(names(CCM.part)[i], CCM.part[1, i], class(CCM.part[, i]), "\n")
  }
  rm(i, statement, CRSP.part, CCM.part) # Clean up.
}

## Consistency check.
if (FALSE) {
  ## How many unique PERMNOs are in the database? 
  statement <- paste("SELECT PERMNO FROM ", table.name.CRSP, ";", sep="")
  PERMNO.CRSP <- dbGetQuery(con.db, statement)[,1]
  length(unique(PERMNO.CRSP))

  ## Warning Message:Reached total allocation of 1535Mb  
}


## Disconnect from SQLite.
db.disconnect(drv, con.db)
rm(drv, con.db)

## Write-protect SQLite database.
Sys.chmod(file.dbName, mode = "0400") 

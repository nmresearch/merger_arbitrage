#######################################################################
## 
## This script prints out those takeover deals that need to be
## downloaded from Factiva to a tab-separated text file (which can
## easily be imported into spreadsheet software). It prints out the
## start date and end date for scraping. The start date can be
## adjusted (see config.R) to be a given number of days prior to the
## announcement date, and the end date is the resolution date.
## 
#######################################################################



## Settings.
dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "007_identify_deals_for_additional_Factiva_download")
source(file.path(dir.code, "config.R"))


## Load 'takeoverData' from SDC 2012-03-07.
load(file.SDC.reloaded)
sdc <- takeoverData; rm(takeoverData)

## Format dates in the way Factiva wants them to be inputted.
sdc$Start_Date <- format(sdc$DA - days.prior.annc, "%d/%m/%Y")
sdc$End_Date <- format(sdc$DRES, "%d/%m/%Y")
## Add alphabetic character at beginning in order for Excel not to
## import as number.
sdc$DEALNO <- paste("D", sdc$DEALNO, sep = "")
## Find the permutation of rows such that the largest deals come
## first.
perm <- order(sdc$VAL, decreasing = TRUE)

## Preview what will be printed.
cols <- c("Start_Date", "End_Date",
          "AN", "ATIC", "TN", "TTIC",
          "DEALNO")
head(sdc[perm, cols])

## Write result to tab-separated text file. 
write.table(sdc[perm, cols],
            file = file.scrape.Factiva.reloaded,
            quote = FALSE, sep = "\t",
            row.names = FALSE, col.names = TRUE)
Sys.chmod(file.scrape.Factiva.reloaded, mode = "0400") # Write-protect file.

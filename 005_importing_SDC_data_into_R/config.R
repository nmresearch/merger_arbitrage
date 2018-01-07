## ======================================================================
##
## This file contains configuration information for 'script_master.R'.
##
## ======================================================================


## Directory containing the data.
dir.data <- file.path(dir.base, "Data")

## Filename of file that contains fixed-width data downloaded from and
## produced by SDC (below referred to as the 'SDC file').
file.SDC.fwf <- file.path(dir.data, "000_020_SDC_scrubbed_data.txt")

## Filename of file to which this program writes tab-separated output.
file.SDC.cleaned <- file.path(dir.data, "005_010_SDC_cleaned.txt")
## Filename of file to which this program saves the cleaned
## data.frame.
file.SDC.df.cleaned <- file.path(dir.data, "005_010_SDC_cleaned.RData")

## Column positions in the SDC file that contain the target and
## acquirer name. The reason why I hard-code those columns is that
## sometimes these column mess up the automatic detection of column
## boundaries in the fixed-width SDC file. For example, sometimes
## these columns contain small typos, e.g. 'MetaSolv~~Inc' instead of
## 'MetaSolv~Inc' (here the '~' symbol is a placeholder for the space
## symbol).
col.ta <- 86:122
col.ac <- 123:159

## Only deals with announcement dates between the following two dates
## will be included in the output (i.e. produced data.frame) of this
## program.
dt.bg <- as.Date("2000-01-01")
dt.ed <- as.Date("2009-12-31")

## Number of largest deals to extract, e.g. extract the 1000 largest
## deals, as measured by deal value.
no.lgst.dls <- 1000 

cols.date <- 1:6 # Columns in SDC file that contain dates.
date.format <- "%m/%d/%y" # Date format in SDC output.

cols.factor <- 16:25 # Columns in SDC file that contain factors.

cols.id <- 9:14 # Columns in SDC file that contain firm identifiers
                # (e.g. CUSIP, ticker, CIDGEN).

## Column names as copied from the original SDC file.
col.names <- c(
               "Rank Date",
               "Date Announced",
               "Date Effective",
               "Date Effective/Unconditional",
               "Date Withdrawn",
               "Target Company Date of Fin.",
               "Target Name", 
               "Acquiror Name", 
               "Acquiror CUSIP", 
               "Target CUSIP", 
               "Acquiror Primary Ticker Symbol", 
               "Target Primary Ticker Symbol", 
               "Acquiror CIDGEN", 
               "Target CIDGEN", 
               "Value of Transaction ($mil)", 
               "Status", 
               "Master Deal Type", 
               "Merger of Equals", 
               "Tender/Merger", 
               "Self-Tender Defense", 
               "Dutch Auction Tender Offer", 
               "Self-Tender", 
               "Tender Offer", 
               "Collar Exchange Ratio Fixed", 
               "Collar Exchange Ratio Floating", 
               "% Owned After Transaction", 
               "Pct Owned by Acquiror Post Merger", 
               "Pct Owned by Target Post Merger", 
               "% of Shares Acq.", 
               "% seeking to own after transaction", 
               "% sought", 
               "Percent of Shares Held at Announcement", 
               "% held prior to transaction", 
               "Percent of Shares Sought in Tender Offer", 
               "Percent of Shares Tendered in Tender Offer", 
               ## "% of Controlling Vote", # This column has no entries
               ##                          # and is thus removed when
               ##                          # reading in the data.
               "Pct of Acq Shs Issued" 
               )




dir.data <- file.path(dir.base, "Data")

## This file is the output of dir 090_. In particular, it contains the
## SDC data downloaded on 2012-03-07 and in addition has various firm
## identifiers (e.g. PERMNO).
file.SDC.complete <- file.path(dir.data, "090_010_SDC_complete.RData")

## This file is the same as above, but it has additional information
## like ticker and company names added from CCM and CRSP.
file.SDC.tickers.et.al <- file.path(dir.data, "095_010_SDC_complete.RData")

## This file contains a list with deal information to determine the
## correct search query in Factiva.
file.scrape.Factiva <- file.path(dir.data, "095_Deals_Download_Factiva.txt")

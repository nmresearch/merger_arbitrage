##################################################################
##
## This file stores basic configuration information.
##
##################################################################


## File to read CRSP data that matches SDC PERMNOs into R.
## http://wrds-web.wharton.upenn.edu/wrds/ds/crsp/dstk/dsf/index.cfm?navGroupHeader=Annual%20Update&navGroup=Daily%20Stocks
file.CRSP.csv <- file.path(dir.data, "020_010_WRDS_CRSP_Search_by_PERMNO.txt")


## Files to read CCM data that matches SDC PERMNOs into R. Note that
## when running the corresponding WRDS web queries, it is important to
## select the *quarterly* screen:
## http://wrds-web.wharton.upenn.edu/wrds/ds/crsp/ccm/fundq/index.cfm?navGroupHeader=Annual%20Update&navGroup=CRSP/COMPUSTAT%20Merged
## The two files differ in that the search variable used was CUSIP9 or
## PERMNO. There is a great deal of overlap, but neither file is a
## subset of the other.


file.CCM.by.PERMNO.csv <- file.path(dir.data, "020_020_WRDS_CCM_Search_by_PERMNO.txt")



# Number of rows to read from CSV file connection into SQLite.
batch.size <- 10000 

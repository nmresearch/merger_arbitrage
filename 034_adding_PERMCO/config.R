## This is the file containing the SDC takeover data with PERMNOs
## 'SDC.data'.
file.SDC.with.PERMNOs <- file.path(dir.data, "030_010_SDC_data_with_PERMNOs.RData")


## ## This file contains the CRSP firm identifiers (in particular PERMCOs
## ## obtained using the 6-digit CUSIPs.
## file.WRDS.CRSP.PERMCO.Converted <-
##   file.path(dir.data, "010_070_CUSIP_WRDS_CRSP_PERMCO.Rdata")


## This is the file in which I store the SDC data.frame 'SDC.data.PERMCO'
## after adding PERMCOs to SDC data with PERMNOs from CRSP.
file.SDC.with.PERMCOs <- file.path(dir.data, "034_010_SDC_data_with_PERMCOs.RData")

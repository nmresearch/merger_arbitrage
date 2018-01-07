
## Directory containing the data.
dir.data <- file.path(dir.base, "Data")

## This file contains the SDC data with added CUSIP9s.
file.SDC.with.CUSIP9 <-
  file.path(dir.data,
            "035_030_SDC_data_with_CUSIP9.RData")

## This file contains the SDC data with added company names from
## CRSP. This is purely a sanity check and not useful for the
## econometric analysis.
file.SDC.with.CRSPNames <-
  file.path(dir.data, "060_010_SDC_data_with_CRSP_Names.RData")

## This is the file containing the SDC takeover dataframe
## 'takeoverData'.
file.SDC.df.cleaned <- file.path(dir.data, "005_010_SDC_cleaned.RData")


## This file contains the CRSP firm identifiers (in particular 8-digit
## CUSIPs) obtained using the 6-digit CUSIPs and various WRDS web
## queries.
file.WRDS.CRSP.Identifiers.Converted <-
  file.path(dir.data, "010_040_CUSIP_WRDS_CRSP_PERMNO.Rdata")


## This is the file in which I store the SDC data.frame 'SDC.data'
## after adding PERMNOs to each takeover case from CRSP. If there are
## multiple PERMNOs, I choose the one whose security has the highest
## trading volume in the year prior to the takeover resolution date.
file.SDC.with.PERMNOs <- file.path(dir.data, "030_010_SDC_data_with_PERMNOs.RData")

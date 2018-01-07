##
## This is the basic configuration file for R programs. 
##
##

## Filename of file to which this program saves the cleaned
## data.frame.
file.SDC.df.cleaned <- file.path(dir.data, "005_010_SDC_cleaned.RData")


## File name to write list of unique acquirer and target CUSIPs from
## 'file.SDC'. This data is needed to later feed it to WRDS, which
## converts the 6-digit CUSIPs to PERMNO.
file.CUSIPs.SDC <- file.path(dir.data, "010_020_CUSIPs_SDC.txt")



## File names that contain the downloaded CRSP firm identifiers from
## WRDS, in particular the PERMNOs.
file.WRDS.CRSP.Identifiers <-
  file.path(dir.data, "010_030_CUSIP_WRDS_CRSP_PERMNO.txt")
file.WRDS.CRSP.Identifiers.Converted <-
  file.path(dir.data, "010_040_CUSIP_WRDS_CRSP_PERMNO.RData")


  
## This file contains a list with unique PERMNO, CRSP's identifier.
file.PERMNO.SDC <- file.path(dir.data, "010_050_PERMNO_SDC.txt")



## File names that contain the downloaded CRSP firm identifiers from
## WRDS, in particular the PERMCOs.
file.WRDS.CRSP.Identifiers.PERMCO <-
  file.path(dir.data, "010_060_CUSIP_WRDS_CRSP_PERMCO.txt")
file.WRDS.CRSP.PERMCO.Converted <-
  file.path(dir.data, "010_070_CUSIP_WRDS_CRSP_PERMCO.RData")
  

  


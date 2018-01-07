

## Directory containing the data.
dir.data <- file.path(dir.base, "Data")


## Result using the OLD algorithm.
## 
## This is the file in which I store the SDC data.frame 'SDC.data.PERMCO'
## after adding PERMCOs to SDC data with PERMNOs from CRSP.
file.SDC.with.PERMCOs.old <- file.path(dir.data, "033_010_SDC_data_with_PERMCOs.RData")


## Result using the NEW algorithm.
## 
## This is the file in which I store the SDC data.frame 'SDC.data.PERMCO'
## after adding PERMCOs to SDC data with PERMNOs from CRSP.
file.SDC.with.PERMCOs.new <- file.path(dir.data, "034_010_SDC_data_with_PERMCOs.RData")

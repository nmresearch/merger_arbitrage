## Directory containing the data.
dir.data <- file.path(dir.base, "Data")


## This file contains the data.frame 'WRDS.Fama.French.factors'.
file.WRDS.Fama.French.Factors <-
  file.path(dir.data, "025_020_WRDS_Fama_French_Factors.RData")

## This file contains the classification results from Factiva.
fname.factiva <- file.path(dir.data, "100_030_factiva.RData")

## File containing log-returns for each deal. The numbers at the end
## stand for the number of days omitted after the announcement date
## and before the resolution date.
file.returns.00.00 <- file.path(dir.data, "107_030_-_020_107_log-returns_00_00.RData")
file.returns.01.00 <- file.path(dir.data, "107_030_-_020_107_log-returns_01_00.RData")

## This file contains accounting information extracted from CCM
file.SDC.CCM <- file.path(dir.data, "107_030_SDC_CCM.RData")

## This file shows which RA downloaded press articles for what deals.
file.division.work <- file.path(dir.data, "110_020_Factiva_Division_of_Work.txt")


dir.data <- file.path(dir.base, "Data")

## Variables to add from Compustat for every deal.
var.to.add <- c(
                "ATQ", # Assets - Total
                "LTQ", # Liabilities - Total
                "CEQQ", # Common/Ordinary Equity - Total
                "PRCCQ", # Price Close - Quarter
                "CSHOQ", # Common Shares Outstanding
                "CHEQ", # Cash and Short-Term Investments
                "CHQ" # Cash
                )


################################################################################
### Input files.

## This file is the output of dir 090_. In particular, it contains the
## SDC data downloaded on 2012-03-07 and in addition has various firm
## identifiers (e.g. PERMNO).
file.SDC.complete <- file.path(dir.data, "090_010_SDC_complete.RData")

## This file contains the classification results from Factiva.
fname.factiva <- file.path(dir.data, "100_030_factiva.RData")

################################################################################

################################################################################
### Output files.

## This file is the output of 'script_master_00.R' that cross-validates the SDC
## information (from 'file.SDC.complete' above) with CRSP data in SQLite
## (i.e. '020_030_CRSP_CCM.sqlite'). Amendments are made if needed.
## *** It's also the input file for 'script_master_01.R '***
file.SDC.amended <- file.path(dir.data, "107_030_-_020_090_SDC_amended_by_CRSP.RData")

## File containing log-returns for each deal. The numbers at the end
## stand for the number of days omitted after the announcement date
## and before the resolution date.
file.returns.00.00 <- file.path(dir.data, "107_030_-_020_107_log-returns_00_00.RData")
file.returns.01.00 <- file.path(dir.data, "107_030_-_020_107_log-returns_01_00.RData")

## This file contains accounting information extracted from CCM
file.SDC.CCM <- file.path(dir.data, "107_030_SDC_CCM.RData")

################################################################################

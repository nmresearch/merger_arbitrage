dir.data <- file.path(dir.base, "Data")



## Only deals with announcement dates between the following two dates
## will be included in the output (i.e. produced data.frame) of this
## program.
dt.bg <- as.Date("1999-01-01")
dt.ed <- as.Date("2009-12-31")

## Number of largest deals to extract, as measured by deal value.
no.lgst.dls <- 1200



## Date format used by SDC.
date.format <- "%m/%d/%y"

## This the file from the previous SDC download from 2012-02-11. At
## that point in time, SDC was broken, so we could only download a few
## variable. However, what's important is that this file contains
## unique deal IDs (created by us, not by Factiva).
## file.SDC.df.cleaned <- file.path(dir.data, "005_010_SDC_cleaned.RData")



## This is a tab-separated text file containing the SDC data
## downloaded 2012-03-07.
file.SDC.txt <- file.path(dir.data, "006_010_SDC_report_output.txt")

## This file is a tab-separated text file that contains the mapping
## between the column names in the Excel file saved from SDC and the
## variable names used internally by SDC (and also by us).
file.SDC.colnames <- file.path(dir.data, "006_020_SDC_column_names.txt")



## 'script_master.R' saves the reloaded SDC data.frame to this
## file, after performing basic sample selection.
file.SDC.reloaded <- file.path(dir.data, "006_060_SDC_reloaded.RData")
## file.SDC.reloaded <- file.path(dir.data, "006_030_SDC_reloaded.RData")

## 'script_master_deprecated.R' saves the merged SDC data.frame to this file.
## file.SDC.merged <- file.path(dir.data, "006_040_SDC_merged.RData")

## =================================================================
## 
## This file contains basic configuration information.
## 
## =================================================================


## The number of days prior to the announcement date that should be
## included when scraping press articles from Factiva.
days.prior.annc <- 0



## This directory contains the data files.
dir.data <- file.path(dir.base, "Data")

## This file contains the data from Buehlmaier's original job market
## paper. It's a direct copy of the file
## 'SDC_and_Factiva_combined_V04.RData'.
## file.JMP <- file.path(dir.data, "000_030_original_JMP_Data.RData")

## This file contains the data that Buehlmaier downloaded from SDC on
## 2012-02-11. It includes all relevant deals, but for every deal it
## does not include all variables, e.g. some deal characteristics such
## as stock swap are missing. These variables will have to be
## downloaded later once SDC works again.
## file.SDC.new <- file.path(dir.data, "005_010_SDC_cleaned.RData")


## The code in this folder outputs a tab-separated text file to the
## following file name.
## file.scrape.Factiva <- file.path(dir.data, "007_010_deals_Factiva_webscraping.txt")




## 'script_master_01.R' in dir 006_ saves the reloaded SDC data.frame
## (from 2012-03-07) to this file, after performing basic sample
## selection.
file.SDC.reloaded <- file.path(dir.data, "006_060_SDC_reloaded.RData")

## This is part of the file 'file.SDC.reloaded' exported to
## tab-separated text file.
file.scrape.Factiva.reloaded <-
  file.path(dir.data, "007_060_deals_Factiva_webscraping_reloaded.txt")

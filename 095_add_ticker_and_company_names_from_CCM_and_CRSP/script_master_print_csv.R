#######################################################################
## 
## This script prints the takeover information to a tab-separated text
## file, including tickers and company names from both Compustat and
## CRSP. This file can later be used for web-scraping Factiva.
## 
#######################################################################



dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage",
                      "095_add_ticker_and_company_names_from_CCM_and_CRSP")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

load(file.SDC.tickers.et.al)
sdc <- takeoverData; rm(takeoverData) # Easy to type name.


cols.print <- c("DA", "DRES",
                "AN", "ANameCRSP", "ANameCCM", "ATIC", "ATickerCRSP", "ATickerCCM",
                "TN", "TNameCRSP", "TNameCCM", "TTIC", "TTickerCRSP", "TTickerCCM")

write.table(sdc[, cols.print],
            file = file.scrape.Factiva,
            quote = FALSE, sep = "\t",
            row.names = FALSE, col.names = TRUE)
Sys.chmod(file.scrape.Factiva, mode = "0400") # Write-protect file.


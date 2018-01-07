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
file.returns.00.00 <- file.path(dir.data, "107_010_log-returns_00_00.RData")
file.returns.01.00 <- file.path(dir.data, "107_010_log-returns_01_00.RData")

## This file contains accounting information extracted from CCM
file.SDC.CCM <- file.path(dir.data, "107_030_SDC_CCM.RData")


## LaTeX files to print to:
dir.LaTeX <- "LaTeX_Output"
sum_stat_numeric <- 
  file.path(dir.code, dir.LaTeX,
            "summary_stats_numeric.tex") 
sum_stat_nominal <- 
  file.path(dir.code, dir.LaTeX,
            "summary_stats_nominal.tex") 
regr.cross.sec.return <-
  file.path(dir.code, dir.LaTeX,
            "regression_cross-sec_return.tex") 
regr.cross.sec.return.sources <-
  file.path(dir.code, dir.LaTeX,
            "regression_cross-sec_return_media_sources.tex") 
regr.cross.sec.return.interactions <-
  file.path(dir.code, dir.LaTeX,
            "regression_cross-sec_return_interaction_terms.tex") 
regr.status.determinants <- 
  file.path(dir.code, dir.LaTeX,
            "regression_GLM_status.tex") 
regr.media.determinants <- 
  file.path(dir.code, dir.LaTeX,
            "regression_media.tex") 

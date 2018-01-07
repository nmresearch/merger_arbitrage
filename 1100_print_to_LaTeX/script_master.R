###################################################################
## 
## This R script pretty prints summary statistics 
## and regression results into LaTeX files. 
## 
## R -q --no-save --no-restore --slave <script_master.R
## 
###################################################################
library("lmtest")
library("sandwich")

dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"
dir.code <- file.path(dir.base, "merger-arbitrage", "1100_print_to_LaTeX")
source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))
source(file.path(dir.base, "merger-arbitrage", "110_event_time_regression", "functions.R"))

## Load classified Factiva data.
load(fname.factiva)
f <- factiva; rm(factiva) # Use easy-to-type name.
## For backwards compatibility when we tried to separately predict the
## 'positive' and 'negative' media probabilities.
if ("classificationC" %in% colnames(f) && !"classification" %in% colnames(f))
  f$classification <- f$classificationC

## Load the log-return time series.
load(file.returns.00.00)
load(file.returns.01.00)

## Load data.frame containing deals and calculate additional variables
## (based on existing variables).
load(file.SDC.CCM)
sdc <- takeoverData; rm(takeoverData) # Use easy-to-type name.
sdc <- calc.variables(sdc) # Add additional variables.
sdc$TDARet_real <- annc.d.ret(Tret.00.00)
sdc$ADARet_real <- annc.d.ret(Aret.00.00)
stopifnot(length(Tret.00.00) == nrow(sdc), length(Tret.01.00) == nrow(sdc))

## Load market returns and Fama-French portfolio returns.
load(file.WRDS.Fama.French.Factors)
FF <- WRDS.Fama.French.factors; rm(WRDS.Fama.French.factors)
## Convert all (effective) returns to logreturns.
FF$mktrf_log <- log(1 + FF$mktrf)
FF$smb_log <- log(1 + FF$smb)
FF$hml_log <- log(1 + FF$hml)
FF$rf_log <- log(1 + FF$rf)
FF$umd_log <- log(1 + FF$umd) 




###################################################################
## Matrix 'nmap' shows how names are mapped to new names. Column 1
## contains the old names, column 2 the new names. The sequence of
## rows in 'nmap' do *not* have to correspond to the sequence of
## column names in 'x' (this is nice, isn't it?). Furthermore, if you
## want to keep some name unchanged, you simply do not create an entry
## for it in 'nmap' below.
###################################################################
nmap <- NULL

## nmap <- rbind(nmap, c("", ""))

nmap <- rbind(nmap, c("TBM_log", "log(Tar.\\ B/M)"))
nmap <- rbind(nmap, c("ABM_log", "log(Acq.\\ B/M)"))
nmap <- rbind(nmap, c("TBM.fixed", "Tar.\\ B/M"))
nmap <- rbind(nmap, c("ABM.fixed", "Acq.\\ B/M"))
nmap <- rbind(nmap, c("log(Target.size.CCM)", "log(Tar.\\ Size)"))
nmap <- rbind(nmap, c("log(Acquiror.size.CCM)", "log(Acq.\\ Size)"))
nmap <- rbind(nmap, c("Target.size.CCM", "Tar.\\ Size"))
nmap <- rbind(nmap, c("Acquiror.size.CCM", "Acq.\\ Size"))
nmap <- rbind(nmap, c("UNSOLICITEDYes", "Unsolicited=Yes"))
nmap <- rbind(nmap, c("SWAPYes", "Stock Deal=Yes"))
nmap <- rbind(nmap, c("TDARet_real", "Tar.\\ Annc.\\ Day Return"))
nmap <- rbind(nmap, c("ADARet_real", "Acq.\\ Annc.\\ Day Return"))
nmap <- rbind(nmap, c("TCashE_log", "log(Tar.\\ Cash/Total Assets)"))
nmap <- rbind(nmap, c("ACashE_log", "log(Acq.\\ Cash/Total Assets)"))
nmap <- rbind(nmap, c("Target.CHEQ.CCM", "Tar.\\ Cash"))
nmap <- rbind(nmap, c("Acquiror.CHEQ.CCM", "Acq.\\ Cash"))
nmap <- rbind(nmap, c("Target.ATQ.CCM", "Tar.\\ Total Assets"))
nmap <- rbind(nmap, c("Acquiror.ATQ.CCM", "Acq.\\ Total Assets"))
nmap <- rbind(nmap, c("media.md", "median(media)"))
nmap <- rbind(nmap, c("media.mn", "mean(media)"))
nmap <- rbind(nmap, c("media.mn.14", "mean(media)$+$14d"))
nmap <- rbind(nmap, c("Tret.raw", "Raw Return"))
nmap <- rbind(nmap, c("Tret.xs.rf", "Excess Return rf"))
nmap <- rbind(nmap, c("Tret.xs.mkt", "Excess Return Mkt"))
nmap <- rbind(nmap, c("media.mn.topNP", "Top Newspapers"))
nmap <- rbind(nmap, c("media.mn.topNW", "Top Newswires"))
nmap <- rbind(nmap, c("STAT", "Deal Status"))
nmap <- rbind(nmap, c("SWAP", "Stock Deal"))
nmap <- rbind(nmap, c("UNSOLICITED", "Unsolicited"))
nmap <- rbind(nmap, c("DAYS", "Deal Duration"))
nmap <- rbind(nmap, c("logDAYS", "log(Deal Duration)"))
nmap <- rbind(nmap, c("premium", "Premium"))

## Miscellaneous.
nmap <- rbind(nmap, c("(Intercept)", "Intercept"))

## Interaction terms.
nmap <- rbind(nmap, c("SWAPNo:media.mn", "media $\\times$ Stock Deal=No"))
nmap <- rbind(nmap, c("SWAPYes:media.mn", "media $\\times$ Stock Deal=Yes"))
nmap <- rbind(nmap, c("UNSOLICITEDNo:media.mn", "media $\\times$ Unsolicited=No"))
nmap <- rbind(nmap, c("UNSOLICITEDYes:media.mn", "media $\\times$ Unsolicited=Yes"))






###################################################################
## 
## Print summary statistics of numeric variables to LaTeX. 
## 
###################################################################

cat_file <- sum_stat_numeric
dig <- 2 # How many digits to print.

days <- 0 # Cutoff date for media and returns.
sdc$Tret.raw <- cumret(Tret.00.00, # Raw cumulative returns.
                       days.omit.beg = days + 1,
                       DAs = sdc$DA)
sdc$Tret.xs.rf <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                         FF,
                         xs = "rf",
                         days.omit.beg = days + 1,
                         days.include.after = NULL,
                         DAs = sdc$DA)
sdc$Tret.xs.mkt <- cumret(Tret.00.00, # Excess returns over market.
                          FF,
                          xs = "mkt",
                          days.omit.beg = days + 1,
                          days.include.after = NULL,
                          DAs = sdc$DA)
sdc$media.mn <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
sdc$media.md <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days, mn = FALSE)

## Get and index 'row' of those rows that do not contain any NA when
## fitting the model using 'fmla.mn'. This makes sure the number of
## observations in the summary statistics correspond to the number of
## observations in the regressions.
if (FALSE) {
  fmla <- as.formula(Tret.raw ~ media.mn
                     + TCashE_log+ ACashE_log
                     + TBM_log+ ABM_log
                     + log(Target.size.CCM) + log(Acquiror.size.CCM)
                     + UNSOLICITED
                     + SWAP
                     + TDARet_real+ ADARet_real)
  dfTO <- na.omit(df.from.fml(fmla, sdc, TRUE))
  rows <- as.numeric(rownames(dfTO))
} else {
  rows <- seq_along(sdc[, 1]) # Take all observations.
}

## Column names from 'sdc' that are of interest. The sequence
## here is the same as it will appear in the LaTeX printout.
cnam <- c("media.mn",
          "media.md",
          "Tret.raw",
          "Tret.xs.rf",
          "Tret.xs.mkt",
          "TDARet_real",
          "ADARet_real",
          "TBM.fixed",
          "ABM.fixed",
          "Target.size.CCM",
          "Acquiror.size.CCM",
          "Target.CHEQ.CCM",
          "Acquiror.CHEQ.CCM",
          "Target.ATQ.CCM",
          "Acquiror.ATQ.CCM",
          "DAYS",
          "premium")
## Extract columns of interest.
x <- sdc[rows, cnam]
## Apply mapping of column names.
x <- col.nam.chg(x, cnam, nmap)

## Print 'x' to LaTeX.
cat("", file = cat_file) # start with empty file 
mycat("Variable & Min & $P_{25}$ & Mean & Median & $P_{75}$ & Max & Std.\\ Dev. \\\\ \n")
mycat("\\hline\n") 
for (i in seq_along(x)) { # Cycle through columns of 'x'.
  mycat(	
        names(x)[i], " \\qquad & ",
        pp(min(x[[i]], na.rm = TRUE), dig), " & ",
        pp(quantile(x[[i]], .25, na.rm = TRUE), dig), " & ",
        pp(mean(x[[i]], na.rm = TRUE), dig), " & ",
        pp(median(x[[i]], na.rm = TRUE), dig), " & ",
        pp(quantile(x[[i]], .75, na.rm = TRUE), dig), " & ",
        pp(max(x[[i]], na.rm = TRUE), dig), " & ",
        pp(sd(x[[i]], na.rm = TRUE), dig), " \\\\ \n" 
	) 
}; rm(i)
mycat("\\hline\n") 

rm(x) 	# Clean up. 
Sys.chmod(cat_file, mode = "0400") # Write-protect the file.





###################################################################
## 
## Print summary statistics of nominal variables to LaTeX. 
## 
###################################################################

cat_file <- sum_stat_nominal
dig <- 1 # How many digits to print.

## Column names from 'sdc' that are of interest. The sequence
## here is the same as it will appear in the LaTeX printout.
cnam <- c("STAT",
          "SWAP",
          "UNSOLICITED")
## Extract columns of interest.
x <- sdc[rows, cnam]
## Apply mapping of column names.
x <- col.nam.chg(x, cnam, nmap)

## Convert entries to lowercase.
for (i in seq_along(x)) { 
  levels(x[[i]]) <- tolower(levels(x[[i]]))
} 

cat("", file = cat_file) 	# start with empty file 
mycat("Variable & \\qquad Levels & \\qquad Observations & \\qquad \\% \\\\ \n")
mycat("\\hline\n") 
for (i in seq_along(x)) { 
  if (length(levels(x[[i]])) != 2) 
    stop("Levels don't match.") 
  mycat(
        names(x)[i], " & \\qquad ", 
        levels(x[[i]])[1], " & ", 
        no_obs <- length(which(x[[i]] == levels(x[[i]])[1])), " & ",
        pp(100*no_obs/length(which(!is.na(x[[i]]))), dig), " \\\\ \n" 
	) 
  mycat( 
        " & ", levels(x[[i]])[2], " & ", 
        no_obs <- length(which(x[[i]] == levels(x[[i]])[2])), " & ",
        pp(100*no_obs/length(which(!is.na(x[[i]]))), dig), " \\\\ \n" 
	) 
} 
mycat("\\hline\n")

rm(x) # Clean up.
Sys.chmod(cat_file, mode = "0400") # Write-protect the file.










###################################################################
## 
## Cumulative event-time return as a function of the media.
## 
###################################################################




###################################################################
## Set formulae.
###################################################################

fmla <- as.formula(Tret ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + premium
                   + TDARet_real+ ADARet_real)



###################################################################
## Set up printing options.
###################################################################

dig <- 3 # How many decimal digits to print. 
cat_file <- regr.cross.sec.return
days <- 0 # Cutoff date for media and returns.
reord <- c(1, 13,14, 10, 11,12, 4,5, 6,7, 2,3, 8,9)
## When creating 'reord', type
## cbind(seq_along(rownames(my.coeff)), rownames(my.coeff))



###################################################################
## Using various link functions, put the regression results into
## matrices and vectors that are later used for printing to LaTeX.
###################################################################

dep.var <- NULL    # Names of dependent variable.
my.R.sq <- NULL    # R-squareds.
my.obs <- NULL     # Number of observations.

my.coeff <- NULL   # Coefficients. 
my.sig <- NULL     # Significance stars.
my.t.stat <- NULL  # p-values.

sdc$Tret.xs.rf <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                         FF,
                         xs = "rf",
                         days.omit.beg = days + 1,
                         days.include.after = NULL,
                         DAs = sdc$DA)
sdc$media.md <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days, mn = FALSE)
fm <- update(fmla, Tret.xs.rf ~ . -media + media.md)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 


sdc$Tret.raw <- cumret(Tret.00.00,
                       days.omit.beg = days + 1,
                       DAs = sdc$DA)
fm <- update(fmla, Tret.raw ~ . - media + media.md)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 


sdc$media.mn <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
fm <- update(fmla, Tret.raw ~ . - media + media.mn)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



###################################################################
## Change the names by using the mapping in 'nmap'.
###################################################################

cnam <- rownames(my.coeff) # Consider all rownames for a potential change.
my.coeff <- row.nam.chg(my.coeff, cnam, nmap)
coeff.nam <- rownames(my.coeff) # Useful shortcut for later.
dep.var <- char.nam.chg(dep.var, nmap)


###################################################################
## Change ordering of coefficients. Media should come first after
## intercept.
###################################################################

if (!all(seq_along(coeff.nam) %in% reord) || length(reord)!=length(coeff.nam)) 
  stop("Something went wrong with the reordering of coefficients.\n")
if (length(unique(reord)) != length(reord))
  stop("There should not be any duplicate entries in 'reord'.")
coeff.nam <- coeff.nam[reord] 
my.coeff <- my.coeff[reord, ] 
my.sig <- my.sig[reord, ] 
my.t.stat <- my.t.stat[reord, ] 



###################################################################
## Print OLS regression results to LaTeX (automatically write-protects
## file).
###################################################################
print.lm.TeX(cat_file, dig, 
             dep.var, my.R.sq, my.obs, 
             coeff.nam, my.coeff, my.sig, my.t.stat)

















###################################################################
## 
## Cumulative event-time return as a function of the media;
## considering top newspapers vs. top neswires.
## 
###################################################################



###################################################################
## Set formulae.
###################################################################

fmla <- as.formula(Tret ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + premium
                   + TDARet_real+ ADARet_real)



###################################################################
## Set up printing options.
###################################################################

dig <- 3 # How many decimal digits to print. 
cat_file <- regr.cross.sec.return.sources
days <- 1 # Cutoff date for media and returns.
reord <- c(1, 13,14, 10, 11,12, 4,5, 6,7, 2,3, 8,9)
## When creating 'reord', type
## cbind(seq_along(rownames(my.coeff)), rownames(my.coeff))




## Consider top newspapers and top newswires.
f$articleSource <- as.factor(f$articleSource)
summary(f$articleSource)[1:40]
## Top newswire.
f.top.newsw <- c("Dow Jones News Service")
f.pos.top.newsw <- which(f$articleSource %in% f.top.newsw)
length(f.pos.top.newsw)
## Top newswires.
f.top.newsws <- c("Dow Jones News Service",
                  "Reuters News",
                  "Associated Press Newswires",
                  "Reuters Significant Developments",
                  "Business Wire")
f.pos.top.newsws <- which(f$articleSource %in% f.top.newsws)
length(f.pos.top.newsws)
## Top newspapers.
f.top.newsp <- c("The Wall Street Journal",
                 "Financial Times", "Financial Times (FT.Com)",
                 "The New York Times", "The New York Times Abstracts",  "New York Times Abstracts",
                 "The Globe and Mail",
                 "eWEEK")
f.pos.top.newsp <- which(f$articleSource %in% f.top.newsp)
length(f.pos.top.newsp)
## Top domestic newspapers.
f.top.newsp.d <- c("The Wall Street Journal",
                   "The New York Times", "The New York Times Abstracts", "New York Times Abstracts",
                   "The Washington Post",
                   "The San Francisco Chronicle",
                   "Investor's Business Daily")
f.pos.top.newsp.d <- which(f$articleSource %in% f.top.newsp.d)
length(f.pos.top.newsp.d)


###################################################################
## Using various link functions, put the regression results into
## matrices and vectors that are later used for printing to LaTeX.
###################################################################

dep.var <- NULL    # Names of dependent variable.
my.R.sq <- NULL    # R-squareds.
my.obs <- NULL     # Number of observations.

my.coeff <- NULL   # Coefficients. 
my.sig <- NULL     # Significance stars.
my.t.stat <- NULL  # p-values.

sdc$Tret.xs.rf <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                         FF,
                         xs = "rf",
                         days.omit.beg = days + 1,
                         days.include.after = NULL,
                         DAs = sdc$DA)
sdc$media.mn.topNP <- media(f[unique(union(f.pos.top.newsp, f.pos.top.newsp.d)), ],
                            sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
fm <- update(fmla, Tret.xs.rf ~ . -media + media.mn.topNP)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



sdc$media.mn.topNW <- media(f[f.pos.top.newsws, ],
                            sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
fm <- update(fmla, Tret.xs.rf ~ . - media + media.mn.topNW)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



###################################################################
## Change the names by using the mapping in 'nmap'.
###################################################################

cnam <- rownames(my.coeff) # Consider all rownames for a potential change.
my.coeff <- row.nam.chg(my.coeff, cnam, nmap)
coeff.nam <- rownames(my.coeff) # Useful shortcut for later.
dep.var <- char.nam.chg(dep.var, nmap)


###################################################################
## Change ordering of coefficients. Media should come first after
## intercept.
###################################################################

if (!all(seq_along(coeff.nam) %in% reord) || length(reord)!=length(coeff.nam)) 
  stop("Something went wrong with the reordering of coefficients.\n")
if (length(unique(reord)) != length(reord))
  stop("There should not be any duplicate entries in 'reord'.")
coeff.nam <- coeff.nam[reord] 
my.coeff <- my.coeff[reord, ] 
my.sig <- my.sig[reord, ] 
my.t.stat <- my.t.stat[reord, ] 



###################################################################
## Print OLS regression results to LaTeX (automatically write-protects
## file).
###################################################################
print.lm.TeX(cat_file, dig, 
             dep.var, my.R.sq, my.obs, 
             coeff.nam, my.coeff, my.sig, my.t.stat)




















###################################################################
## 
## Return as function of media; considering interaction tersm.
## 
###################################################################



###################################################################
## Set formulae.
###################################################################

fmla <- as.formula(Tret ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + premium
                   + TDARet_real+ ADARet_real)



###################################################################
## Set up printing options.
###################################################################

dig <- 3 # How many decimal digits to print. 
cat_file <- regr.cross.sec.return.interactions
days <- 0 # Cutoff date for media and returns.
reord <- c(1, 13,14, 15,16, 10, 11,12, 4,5, 6,7, 2,3, 8,9)
## When creating 'reord', type
## cbind(seq_along(rownames(my.coeff)), rownames(my.coeff))



###################################################################
## Using various link functions, put the regression results into
## matrices and vectors that are later used for printing to LaTeX.
###################################################################

dep.var <- NULL    # Names of dependent variable.
my.R.sq <- NULL    # R-squareds.
my.obs <- NULL     # Number of observations.

my.coeff <- NULL   # Coefficients. 
my.sig <- NULL     # Significance stars.
my.t.stat <- NULL  # p-values.

sdc$Tret.xs.rf <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                         FF,
                         xs = "rf",
                         days.omit.beg = days + 1,
                         days.include.after = NULL,
                         DAs = sdc$DA)
sdc$media.mn <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
fm <- update(fmla, Tret.xs.rf ~ . - media + media.mn : SWAP)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



fm <- update(fmla, Tret.xs.rf ~ . - media + media.mn : UNSOLICITED)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



###################################################################
## Change the names by using the mapping in 'nmap'.
###################################################################

cnam <- rownames(my.coeff) # Consider all rownames for a potential change.
my.coeff <- row.nam.chg(my.coeff, cnam, nmap)
coeff.nam <- rownames(my.coeff) # Useful shortcut for later.
dep.var <- char.nam.chg(dep.var, nmap)


###################################################################
## Change ordering of coefficients. Media should come first after
## intercept.
###################################################################

if (!all(seq_along(coeff.nam) %in% reord) || length(reord)!=length(coeff.nam)) 
  stop("Something went wrong with the reordering of coefficients.\n")
if (length(unique(reord)) != length(reord))
  stop("There should not be any duplicate entries in 'reord'.")
coeff.nam <- coeff.nam[reord] 
my.coeff <- my.coeff[reord, ] 
my.sig <- my.sig[reord, ] 
my.t.stat <- my.t.stat[reord, ] 



###################################################################
## Print OLS regression results to LaTeX (automatically write-protects
## file).
###################################################################
print.lm.TeX(cat_file, dig, 
             dep.var, my.R.sq, my.obs, 
             coeff.nam, my.coeff, my.sig, my.t.stat)














###################################################################
## 
## STAT ~ media; GLM models including interactions.
## 
###################################################################


###################################################################
## Set formulae.
###################################################################

fmla <- as.formula(STAT ~ media
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + SWAP + UNSOLICITED + logDAYS
                   + premium
                   + TDARet_real+ ADARet_real)



###################################################################
## Set up printing options.
###################################################################

dig <- 2 # How many decimal digits to print. 
cat_file <- regr.status.determinants
days <- 0 # Cutoff date for media and returns.
reord <- c(1, 14, 15,16, 17,18, 11, 12,13, 4,5, 6,7, 2,3, 9,8,10)
## When creating 'reord', type
## cbind(seq_along(rownames(my.coeff)), rownames(my.coeff))

###################################################################
## Using various link functions, put the regression results into
## matrices and vectors that are later used for printing to LaTeX.
###################################################################

dep.var <- NULL    # Name of dependent variable.
link.fcn <- NULL   # Name of link function.
my.R.sq <- NULL    # Pseudo R-squareds.
my.obs <- NULL     # Number of observations.

my.coeff <- NULL   # Coefficients. 
my.effects <- NULL # Marginal effects. 
my.sig <- NULL     # Significance stars.
my.z.value <- NULL # z-values.


mylink <- "probit" 
link.fcn <- c(link.fcn, "Probit")
fm <- fmla

sdc$Tret.xs.rf <- cumret(Tret.00.00, # Excess returns over risk-free rate.
                         FF,
                         xs = "rf",
                         days.omit.beg = days + 1,
                         days.include.after = NULL,
                         DAs = sdc$DA)
sdc$media.mn <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
my_super_model <- glm(update(fm, . ~ . - media + media.mn), data = sdc,
                      family = binomial(link = mylink))

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, McFadden_ps_R_sq(my_super_model)) 
my.obs <- c(my.obs, nrow(sdc)-length(summary(my_super_model)$na.action))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my_super_model))) 
my.effects <- col.add.TeX(my.effects, as.matrix(av_sample_marg_eff(my_super_model))) 
my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my_super_model)$coefficients[, "Pr(>|z|)"]))) 
my.z.value <- col.add.TeX(my.z.value, as.matrix(summary(my_super_model)$coefficients[, "z value"])) 


mylink <- "probit" 
link.fcn <- c(link.fcn, "Probit")
fm <- fmla

my_super_model <- glm(update(fm, . ~ . - media + media.mn : SWAP), data = sdc,
                      family = binomial(link = mylink))

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, McFadden_ps_R_sq(my_super_model)) 
my.obs <- c(my.obs, nrow(sdc)-length(summary(my_super_model)$na.action)) 

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my_super_model))) 
my.effects <- col.add.TeX(my.effects, as.matrix(av_sample_marg_eff(my_super_model))) 
my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my_super_model)$coefficients[, "Pr(>|z|)"]))) 
my.z.value <- col.add.TeX(my.z.value, as.matrix(summary(my_super_model)$coefficients[, "z value"])) 


mylink <- "probit" 
link.fcn <- c(link.fcn, "Probit")
fm <- fmla

my_super_model <- glm(update(fm, . ~ . - media + media.mn : UNSOLICITED), data = sdc,
                      family = binomial(link = mylink))

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, McFadden_ps_R_sq(my_super_model)) 
my.obs <- c(my.obs, nrow(sdc)-length(summary(my_super_model)$na.action)) 

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my_super_model))) 
my.effects <- col.add.TeX(my.effects, as.matrix(av_sample_marg_eff(my_super_model))) 
my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my_super_model)$coefficients[, "Pr(>|z|)"]))) 
my.z.value <- col.add.TeX(my.z.value, as.matrix(summary(my_super_model)$coefficients[, "z value"])) 



###################################################################
## Change the coefficient names by using the mapping in 'nmap'.
###################################################################

cnam <- rownames(my.coeff) # Consider all rownames for a potential change.
my.coeff <- row.nam.chg(my.coeff, cnam, nmap)
coeff.nam <- rownames(my.coeff) # Useful shortcut for later.
dep.var <- char.nam.chg(dep.var, nmap)



###################################################################
## Change ordering of coefficients. Media should come first after
## intercept.
###################################################################

if (!all(seq_along(coeff.nam) %in% reord) || length(reord)!=length(coeff.nam)) 
  stop("Something went wrong with the reordering of coefficients.\n")
if (length(unique(reord)) != length(reord))
  stop("There should not be any duplicate entries in 'reord'.")
coeff.nam <- coeff.nam[reord] 
my.coeff <- my.coeff[reord, ] 
my.effects <- my.effects[reord, ] 
my.sig <- my.sig[reord, ] 
my.z.value <- my.z.value[reord, ] 
rm(reord) # Clean up. 


###################################################################
## Print GLM regression results to LaTeX.
###################################################################
print.glm.TeX(cat_file, dig, 
              dep.var, link.fcn, my.R.sq, my.obs, 
              coeff.nam, my.coeff, my.effects, my.sig, my.z.value)














###################################################################
## 
## Media determinants.
## 
###################################################################




###################################################################
## Set formulae.
###################################################################

fmla <- as.formula(media ~ 
                   + TCashE_log+ ACashE_log
                   + TBM_log+ ABM_log
                   + log(Target.size.CCM) + log(Acquiror.size.CCM)
                   + UNSOLICITED
                   + SWAP
                   + logDAYS
                   + premium
                   + TDARet_real+ ADARet_real)



###################################################################
## Set up printing options.
###################################################################

dig <- 3 # How many decimal digits to print. 
cat_file <- regr.media.determinants
days <- 0 # Cutoff date for media and returns.
reord <- c(1, 11, 12,13, 4,5, 6,7, 2,3, 8,9,10)
## When creating 'reord', type
## cbind(seq_along(rownames(my.coeff)), rownames(my.coeff))



###################################################################
## Using various link functions, put the regression results into
## matrices and vectors that are later used for printing to LaTeX.
###################################################################

dep.var <- NULL    # Names of dependent variable.
my.R.sq <- NULL    # R-squareds.
my.obs <- NULL     # Number of observations.

my.coeff <- NULL   # Coefficients. 
my.sig <- NULL     # Significance stars.
my.t.stat <- NULL  # p-values.

sdc$media.mn <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days)
fm <- update(fmla, media.mn ~ .)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 


sdc$media.mn.14 <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days+14)
fm <- update(fmla, media.mn.14 ~ .)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 


sdc$media.md <- media(f, sdc[, c("dealID", "DA", "DRES")], days.inc.DA = days, mn = FALSE)
fm <- update(fmla, media.md ~ .)
my.lm <- lm(fm, data = sdc)

dep.var <- c(dep.var, as.character(fm)[2])
my.R.sq <- c(my.R.sq, summary(my.lm)$r.squared) 
my.obs <- c(my.obs, nrow(my.lm$model))

my.coeff <- col.add.TeX(my.coeff, as.matrix(coef(my.lm))) 
## my.sig <- col.add.TeX(my.sig, as.matrix(sig(summary(my.lm)$coefficients[, "Pr(>|t|)"]))) 
my.sig <-
  col.add.TeX(my.sig,
              as.matrix(sig(coeftest(my.lm,
                                     vcov = function(...) vcovHC(..., type = "HC0"))[, "Pr(>|t|)"]))) 
## my.t.stat <- col.add.TeX(my.t.stat, as.matrix(summary(my.lm)$coefficients[, "t value"])) 
my.t.stat <-
  col.add.TeX(my.t.stat,
              as.matrix(coeftest(my.lm,
                                 vcov = function(...) vcovHC(..., type = "HC0"))[, "t value"])) 



###################################################################
## Change the names by using the mapping in 'nmap'.
###################################################################

cnam <- rownames(my.coeff) # Consider all rownames for a potential change.
my.coeff <- row.nam.chg(my.coeff, cnam, nmap)
coeff.nam <- rownames(my.coeff) # Useful shortcut for later.
dep.var <- char.nam.chg(dep.var, nmap)


###################################################################
## Change ordering of coefficients. Media should come first after
## intercept.
###################################################################

if (!all(seq_along(coeff.nam) %in% reord) || length(reord)!=length(coeff.nam)) 
  stop("Something went wrong with the reordering of coefficients.\n")
if (length(unique(reord)) != length(reord))
  stop("There should not be any duplicate entries in 'reord'.")
coeff.nam <- coeff.nam[reord] 
my.coeff <- my.coeff[reord, ] 
my.sig <- my.sig[reord, ] 
my.t.stat <- my.t.stat[reord, ] 



###################################################################
## Print OLS regression results to LaTeX (automatically write-protects
## file).
###################################################################
print.lm.TeX(cat_file, dig, 
             dep.var, my.R.sq, my.obs, 
             coeff.nam, my.coeff, my.sig, my.t.stat)



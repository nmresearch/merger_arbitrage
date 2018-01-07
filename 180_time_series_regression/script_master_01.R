#######################################################################
##
## This script locates the optimal threshold of daily excess log return
## , i.e. (daily log) market return minus risk free rate, for various
## piecewise CAPM time-series regressions.
##
## The optimization is done in two ways: calling 'optimize()' and
## direct gridding. The results pasted within this script were checked
## manually by inspecting the cost curve.
##
#######################################################################

rm(list = ls())

## dir.base <- file.path("~/RA")
#dir.base <- file.path("~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work")
#dir.code <- file.path(dir.base, "merger-arbitrage", "180_time_series_regression")
dir.base <- file.path('C:/R/RA')
dir.code <- file.path(dir.base, 'working', '180_time_series_regression')
dir.data <- file.path(dir.base, "Data")

source(file.path(dir.code, "config.R"))
source(file.path(dir.code, "functions.R"))

##########################################################################
### Prepare the data, in particular 'FF.mapped', for various cost functions.

## Load data.frame 'Tret.01.00' and 'Aret.01.00'.
load(file.returns.01.00); rm(file.returns.01.00)

## Load Factiva classification results (data.frame 'factiva').
load(file.factiva); rm(file.factiva)

## Combine Tret.01.00m Aret.01.00 and Factiva information.
## Remains dealID 'gxwwvuqc', 'hqinsasa', 'roonefiv' with NA Aret when needed.
TAret.factiva <- combine.TAret.factiva(Tret.01.00, Aret.01.00, factiva)

## Load Fama-French factors and use a simple name for the data.frame.
load(file.WRDS.Fama.French.Factors); rm(file.WRDS.Fama.French.Factors)
FF <- WRDS.Fama.French.factors; rm(WRDS.Fama.French.factors)

## Convert all (effective) returns to log-returns.
FF$mktrf_log <- log(1+FF$mktrf)
FF$smb_log <- log(1+FF$smb)
FF$hml_log <- log(1+FF$hml)
FF$rf_log <- log(1+FF$rf)
FF$umd_log <- log(1+FF$umd)

## Map Long-target-Only or Long-target-Short-acquirer daily log returns onto
## the dates of 'FF'.
FF.mapped <- map.TAret.factiva.FF(TAret.factiva, FF)
###
##########################################################################


##########################################################################
### Various optimal threshold of daily excess log return
### for each piecewise CAPM regression.

### 1) Piecewise CAPM regressions using EWLO.
## First MANUALLY inspecting the cost landscape.
x <- seq(min(FF.mapped$mktrf_log), max(FF.mapped$mktrf_log), length.out=800)
y <- rep(as.numeric(NA), 800)
for(i in seq_along(x)) y[i] <- Cost.EWLO(x[i], FF.mapped)
plot(x, y, type='b', xlab='mktrf_log threshold', ylab='EWLO Cost')
## x[which.min(y)]
## [1] 0.0245402

## Then call optimize() to locate the optimal threshold.
optimize(f=Cost.EWLO, interval=range(FF.mapped$mktrf_log), FF.mapped )
## $minimum
## [1] 0.02435029

## Finally, visually check if it's really minimal cost.
abline(v=0.02435029, col='blue')

## Answer: optimal threshold = 0.02435029 .
## 0.024235029 * 22
## [1] 0.5331706


### 2) Piecewise CAPM regressions using VWLO.
## First MANUALLY inspecting the cost landscape.
x <- seq(min(FF.mapped$mktrf_log), max(FF.mapped$mktrf_log), length.out=800)
y <- rep(as.numeric(NA), 800)
for(i in seq_along(x)) y[i] <- Cost.VWLO(x[i], FF.mapped)
plot(x, y, type='b', xlab='mktrf_log threshold', ylab='VWLO Cost')
## x[which.min(y)]
## [1] -0.02788653

## *Note* Cost function NOT well behaved: minimal cost may NOT be well defined.

## Then call optimize() to locate the optimal threshold.
optimize(f=Cost.VWLO, interval=range(FF.mapped$mktrf_log), FF.mapped )
## $minimum
## [1] -0.02805339

## Finally, visually check if it's really minimal cost.
abline(v=-0.02805339, col='blue')

## Answer: 'optimal' threshold = -0.02805339 .
## -0.02805339 * 22
## [1] -0.6171746


### 3) Piecewise CAPM regressions using EWLS.
## First MANUALLY inspecting the cost landscape.
x <- seq(min(FF.mapped$mktrf_log), max(FF.mapped$mktrf_log), length.out=800)
y <- rep(as.numeric(NA), 800)
for(i in seq_along(x)) y[i] <- Cost.EWLS(x[i], FF.mapped)
plot(x, y, type='b', xlab='mktrf_log threshold', ylab='EWLS Cost')
## x[which.min(y)]
## [1] -0.02126956

## *Note* Cost function NOT well behaved: minimal cost may NOT be well defined.

## Then call optimize() to locate the optimal threshold.
optimize(f=Cost.EWLS, interval=range(FF.mapped$mktrf_log), FF.mapped )
## $minimum
## [1] -0.03646221

## Finally, visually check if it's really minimal cost.
abline(v=-0.03646221, col='red')   # optimize() failed in this particular case.
abline(v=-0.02126956, col='blue')

## Answer: 'optimal' threshold = -0.02126956 .
## -0.02126956*22
## [1] -0.4679303

### 4) Piecewise CAPM regressions using VWLS.
## First MANUALLY inspecting the cost landscape.
x <- seq(min(FF.mapped$mktrf_log), max(FF.mapped$mktrf_log), length.out=800)
y <- rep(as.numeric(NA), 800)
for(i in seq_along(x)) y[i] <- Cost.VWLS(x[i], FF.mapped)
plot(x, y, type='b', xlab='mktrf_log threshold', ylab='VWLS Cost')
## x[which.min(y)]
## [1] -0.02050606

## Then call optimize() to locate the optimal threshold.
optimize(f=Cost.VWLS, interval=range(FF.mapped$mktrf_log), FF.mapped )
## $minimum
## [1] 0.03132241

## Finally, visually check if it's really minimal cost.
abline(v=0.03132241, col='red')   # optimize() failed in this particular case.
abline(v=-0.02050606, col='blue')

## Answer: 'optimal' threshold = -0.02050606 .
## -0.02050606*22
## [1] -0.4511333

###
##########################################################################

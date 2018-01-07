#######################################################################
##
## This script calculates risk arbitrage return series treating all
## deals as cash deals, i.e. we only long the target shares.
## But Long-target and Short-acquirer returns are also prepared for
## further studies.
##
## Upon finished producing the output file (see 'config.R'), all kinds
## of time series regressions on all combinations of pruning by prior
## media content or prior media coverage are generated and stored in
## arrays, i.e. things like 'FilterMode' in previous version is no longer
## needed.
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

### Convert all (effective) returns to log-returns.
FF$mktrf_log <- log(1+FF$mktrf)
FF$smb_log <- log(1+FF$smb)
FF$hml_log <- log(1+FF$hml)
FF$rf_log <- log(1+FF$rf)
FF$umd_log <- log(1+FF$umd)

##########################################################################
## Unconditional regression (no pruning). NA media cases INCLUDED.
FF.mapped <- map.TAret.factiva.FF(TAret.factiva, FF)
All.cases.including.NA.media <- TSRegression(FF.mapped)
## > All.cases.including.NA.media
##                   CAPM_EWLO    CAPM_VWLO      FF_EWLO      FF_VWLO  High_EWLO      Low_EWLO    High_VWLO    Low_VWLO
## Alpha         -1.011611e-05 0.0004617819 1.291092e-05 0.0006278261 0.01527677 -9.844536e-05 0.0004537906 -0.01613095
## p.value        9.549093e-01 0.0676375851 9.391881e-01 0.0089872151 0.09128552  5.243267e-01 0.0664802609  0.07024260
## adj.r.squared  5.594419e-01 0.4400778140 6.071139e-01 0.4956292480 0.02561009  5.667885e-01 0.3971547710  0.05824509
##                  CAPM_EWLS    CAPM_VWLS      FF_EWLS      FF_VWLS    High_EWLS     Low_EWLS    High_VWLS     Low_VWLS
## Alpha         0.0002137898 0.0005281175 0.0001721686 0.0005168281 0.0002053762 1.212453e-02 0.0004349064 0.0152149816
## p.value       0.2225732283 0.0500698660 0.3159487317 0.0549309426 0.2419597471 1.695956e-05 0.1100630259 0.0001042097
## adj.r.squared 0.3854708695 0.1795754593 0.4123097056 0.1839200505 0.2964358407 4.728267e-01 0.1293449698 0.2890533310
##               CAPM_EWLO_with_rf CAPM_VWLO_with_rf FF_EWLO_with_rf FF_VWLO_with_rf High_EWLO_with_rf Low_EWLO_with_rf
## Alpha              9.319173e-05      0.0005650897    0.0001161775    0.0007310927        0.01539588     5.745678e-06
## p.value            6.023484e-01      0.0253456642    0.4923380439    0.0023529475        0.08893862     9.703391e-01
## adj.r.squared      5.595225e-01      0.4400657729    0.6071066416    0.4955507811        0.02540135     5.671439e-01
##               High_VWLO_with_rf Low_VWLO_with_rf CAPM_EWLS_with_rf CAPM_VWLS_with_rf FF_EWLS_with_rf FF_VWLS_with_rf
## Alpha              0.0005584209      -0.01601762      0.0003170976      0.0006314253    0.0002754352    0.0006200947
## p.value            0.0239666058       0.07163431      0.0704312141      0.0191644807    0.1086562251    0.0213026372
## adj.r.squared      0.3968481849       0.05884450      0.3854654870      0.1795171507    0.4122337464    0.1838331750
##               High_EWLS_with_rf Low_EWLS_with_rf High_VWLS_with_rf Low_VWLS_with_rf
## Alpha              0.0003105652      0.012252114      0.0005400723     1.534681e-02
## p.value            0.0768434706      0.000013662      0.0472697381     8.990977e-05
## adj.r.squared      0.2960446995      0.474237140      0.1290709475     2.901266e-01


##########################################################################
## Generate various time series regression based on different combinations
## of pruning conditions.
## Each of the following runs involves 1) subsetting of 'Tret.factiva',
## 2) mapping the subset onto the dates of 'FF' by calculating 'EWLO' and 'VWLO'
## using 'map.TAret.factiva.FF()' and 3) generating various regressions on these
## returns by 'TSRegression()'.

###
## Consider ONLY NA media cases.
TAret.factiva.subset <- TAret.factiva[which(!is.finite(TAret.factiva$Content.prior)) , ]
FF.mapped <- map.TAret.factiva.FF(TAret.factiva.subset, FF)
Only.cases.with.NA.media <- TSRegression(FF.mapped)
## > Only.cases.with.NA.media
##                  CAPM_EWLO   CAPM_VWLO      FF_EWLO     FF_VWLO   High_EWLO     Low_EWLO    High_VWLO    Low_VWLO
## Alpha         0.0002664731 0.001011656 0.0002966886 0.001139258  0.02600432 0.0002510759 0.0008791289 -0.01787577
## p.value       0.5393555886 0.014338317 0.4905884236 0.004242158  0.08677049 0.5468061915 0.0319672196  0.14008845
## adj.r.squared 0.1460315385 0.188595043 0.1625264674 0.247368682 -0.01444713 0.1321111222 0.1715257948 -0.01030972
##                  CAPM_EWLS   CAPM_VWLS      FF_EWLS     FF_VWLS    High_EWLS    Low_EWLS   High_VWLS     Low_VWLS
## Alpha         0.0005933176 0.001180489 0.0005830711 0.001270721 0.0005583338 0.005267852 0.001039119 0.0008923087
## p.value       0.1893470235 0.009077094 0.1957243593 0.004031094 0.2260058851 0.396086784 0.022262752 0.8951478429
## adj.r.squared 0.0784821698 0.104915382 0.0859996428 0.148242390 0.0548898556 0.083382537 0.084216968 0.0398430403
##               CAPM_EWLO_with_rf CAPM_VWLO_with_rf FF_EWLO_with_rf FF_VWLO_with_rf High_EWLO_with_rf Low_EWLO_with_rf
## Alpha              0.0003813808       0.001126564    0.0004114762     0.001254046        0.02614734     0.0003671073
## p.value            0.3797185427       0.006404079    0.3390692561     0.001650358        0.08512285     0.3783224537
## adj.r.squared      0.1460202612       0.188582487    0.1624705170     0.247262217       -0.01444089     0.1322065068
##               High_VWLO_with_rf Low_VWLO_with_rf CAPM_EWLS_with_rf CAPM_VWLS_with_rf FF_EWLS_with_rf FF_VWLS_with_rf
## Alpha              0.0009953794      -0.01774058      0.0007082254       0.001295397    0.0006978587     0.001385508
## p.value            0.0151922986       0.14253485      0.1172309371       0.004200128    0.1215452628     0.001719261
## adj.r.squared      0.1713353349      -0.01016513      0.0784683008       0.104911891    0.0859610500     0.148165463
##               High_EWLS_with_rf Low_EWLS_with_rf High_VWLS_with_rf Low_VWLS_with_rf
## Alpha                0.00067499      0.005413783       0.001155772      0.001040769
## p.value              0.14334955      0.382980394       0.011028956      0.877708885
## adj.r.squared        0.05477647      0.083888068       0.084073199      0.040231492




###
## Defines dimension names of output arrays also the loop of pruning thresholds.
info.set <- rownames(All.cases.including.NA.media)
regression.set <- colnames(All.cases.including.NA.media)
content.set <- seq(from=0, to=1, by=0.05)
coverage.set <- seq(from=1, to=180, by=1)

## It's hard to loop on 'Coverage.prior' with step=1 as max is too large.
## > summary(TAret.factiva$Coverage.prior, na.rm=TRUE)
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##     0.0     2.0     7.0    18.2    16.0   688.0

###
## Pruned by 'Content.prior' AND 'Coverage.prior', as well as
## 'Content.prior' OR 'Coverage.prior'.
By.Content.prior.And.Coverage.prior <- array(dim = c(length(info.set),
                                                     length(regression.set),
                                                     length(content.set),
                                                     length(coverage.set) ),
                                             dimnames = list(info.set,
                                                             regression.set,
                                                             content.set,
                                                             coverage.set) )

By.Content.prior.Or.Coverage.prior <- By.Content.prior.And.Coverage.prior

for (i.content in 1:length(content.set) ) {
    for (i.coverage in 1:length(coverage.set) ) {
         cat('Content=', content.set[i.content],
             '  Coverage=', coverage.set[i.coverage], '\n')

        ## Pruned by 'Content.prior' AND 'Coverage.prior'.
        TAret.factiva.subset <- TAret.factiva[which(TAret.factiva$Content.prior >= content.set[i.content] &
                                                    TAret.factiva$Coverage.prior >= coverage.set[i.coverage]), ]

        FF.mapped <- map.TAret.factiva.FF(TAret.factiva.subset, FF)
        if(nrow(FF.mapped) == 0) next;

        reg <- TSRegression(FF.mapped)

        for (rn in rownames(reg) ) {
            for (cn in colnames(reg) ) {
                By.Content.prior.And.Coverage.prior[rn, cn, i.content, i.coverage] <- reg[rn, cn]
            }
        }

        ## Pruned by 'Content.prior' OR 'Coverage.prior'.
        TAret.factiva.subset <- TAret.factiva[which(TAret.factiva$Content.prior >= content.set[i.content] |
                                                    TAret.factiva$Coverage.prior >= coverage.set[i.coverage]), ]

        FF.mapped <- map.TAret.factiva.FF(TAret.factiva.subset, FF)
        if(nrow(FF.mapped) == 0) next;

        reg <- TSRegression(FF.mapped)

        for (rn in rownames(reg) ) {
            for (cn in colnames(reg) ) {
                By.Content.prior.Or.Coverage.prior[rn, cn, i.content, i.coverage] <- reg[rn, cn]
            }
        }

    }
}

## Visualize information like:
filled.contour(x=content.set,
               y=coverage.set,
               z=By.Content.prior.And.Coverage.prior["Alpha", "CAPM_VWLS", ,])
filled.contour(x=content.set,
               y=coverage.set,
               z=By.Content.prior.Or.Coverage.prior["Alpha", "CAPM_VWLS", ,])

###
## Pruned by 'Content.prior' only.
By.Content.prior <- array(dim = c(length(info.set),
                                  length(regression.set),
                                  length(content.set) ),
                          dimnames = list(info.set,
                                          regression.set,
                                          content.set) )

for (i.content in 1:length(content.set) ) {
    cat('Content=', content.set[i.content], '\n')

    TAret.factiva.subset <- TAret.factiva[which(TAret.factiva$Content.prior >= content.set[i.content] ), ]

    FF.mapped <- map.TAret.factiva.FF(TAret.factiva.subset, FF)
    if(nrow(FF.mapped) == 0) next;

    reg <- TSRegression(FF.mapped)

    for (rn in rownames(reg) ) {
        for (cn in colnames(reg) ) {
            By.Content.prior[rn, cn, i.content] <- reg[rn, cn]
        }
    }
}

## Actually it's same as the particular case 'By.Content.prior.And.Coverage.prior[, , , 1]'.
## > identical(By.Content.prior, By.Content.prior.And.Coverage.prior[, , , 1] )
## [1] TRUE
## >
## > identical(By.Content.prior, By.Content.prior.Or.Coverage.prior[, , , 1] )
## [1] FALSE

## Visualize results like:
plot(x=content.set,
     y=By.Content.prior["Alpha", "CAPM_VWLS", ],
     type="b", xlab="Content.prior threshold", ylab="Alpha")
abline(h=0, col='grey68')


###
## Pruned by 'Coverage.prior' only.
By.Coverage.prior <- array(dim = c(length(info.set),
                                   length(regression.set),
                                   length(coverage.set) ),
                           dimnames = list(info.set, regression.set, coverage.set) )

for (i.coverage in 1:length(coverage.set) ) {
    cat('Coverage=', coverage.set[i.coverage], '\n')

    TAret.factiva.subset <- TAret.factiva[which(TAret.factiva$Coverage.prior >= coverage.set[i.coverage] ), ]

    FF.mapped <- map.TAret.factiva.FF(TAret.factiva.subset, FF)
    if(nrow(FF.mapped) == 0) next;

    reg <- TSRegression(FF.mapped)

    for (rn in rownames(reg) ) {
        for (cn in colnames(reg) ) {
            By.Coverage.prior[rn, cn, i.coverage] <- reg[rn, cn]
        }
    }
}

## Actually it's same as the particular case 'By.Content.prior.And.Coverage.prior[, , 1, ]'.
## > identical(By.Coverage.prior, By.Content.prior.And.Coverage.prior[, , 1, ] )
## [1] TRUE
## >
## > identical(By.Coverage.prior, By.Content.prior.Or.Coverage.prior[, , 1, ] )
## [1] FALSE

## Visualize results like:
plot(x=coverage.set,
     y=By.Coverage.prior["Alpha", "CAPM_VWLS", ],
     type="b", xlab="Coverage.prior threshold", ylab="Alpha")
abline(h=0, col='grey68')



##########################################################################

## Save mapped data frames and all results.
save(TAret.factiva, FF,
     info.set, regression.set, content.set, coverage.set,
     By.Content.prior.And.Coverage.prior,
     By.Content.prior.Or.Coverage.prior,
     By.Content.prior,
     By.Coverage.prior,
     All.cases.including.NA.media,
     Only.cases.with.NA.media,
     file = file.time.series.regression.results)
Sys.chmod(file.time.series.regression.results, mode = "0400")


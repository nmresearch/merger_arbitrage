######################################################################
##
## This is a master script to read the SDC data and convert to PERMNO 
## and PERMCO.
##
######################################################################


dir.base <- "~/Documents/Research/Media_and_Risk_Arbitrage/Empirical_030_-_RA_Work"

## Set directory variables.
dir.code <- file.path(dir.base,
                      "merger-arbitrage",
                      "010_convert_to_PERMNO_and_PERMCO")
dir.data <- file.path(dir.base, "Data")

## Load remaining configuration.
source(file.path(dir.code, "config.R"))
load(file.SDC.df.cleaned)

######################################################################
## This part of the program writes list with unique 6-digit CUSIPs from
## SDC in a text file so that I can use this text file to feed it to a 
## WRDS web query.
######################################################################


if (TRUE) {
  pos.print.acq <- which(!is.na(takeoverData[, "Acquiror_CUSIP"]))
  pos.print.tar <- which(!is.na(takeoverData[, "Target_CUSIP"]))
  ## 'union' already discards duplicate values in both arguments.
  CUSIPs <- union(takeoverData[pos.print.acq, "Acquiror_CUSIP"],
                  takeoverData[pos.print.tar, "Target_CUSIP"])
  write.table(CUSIPs,
              file = file.CUSIPs.SDC,
              quote = FALSE, sep = "",
              row.names = FALSE, col.names = FALSE)
  Sys.chmod(file.CUSIPs.SDC, mode = "0400") # Write-protect file.
  ## Test if it worked.
  if (!all(takeoverData[!is.na(takeoverData[, "Acquiror_CUSIP"]),
                           "Acquiror_CUSIP"] %in% CUSIPs))
    stop("Something went wrong.")
  if (!all(takeoverData[!is.na(takeoverData[, "Target_CUSIP"]),
                           "Target_CUSIP"] %in% CUSIPs))
    stop("Something went wrong.")
  rm(pos.print.acq, pos.print.tar, CUSIPs) # Clean up.
}


######################################################################
## This part of the program converts CUSIP to PERMNOs.
######################################################################

## Read firm identifiers that I've downloaded from WRDS: Click 'Tools'
## and then 'CRSP Tools - Translate to PERMCO/PERMNO (Annual
## Updates)' or point your browser to http://wrds-web.wharton.upenn.e
## du/wrds/ds/crsp/tools_a/dse/translate/index.cfm?navGroupHeader=Ann
## ual%20Update&navGroup=Tools
##
##
## The important information here is that from WRDS, for
## every CUSIP I now have the PERMNOs, which are the primary
## identifiers for CRSP.
##
## For reproducibility, I have also saved the WRDS Data Request
## Summary and the WRDS SAS log (http://wrds-sol1.wharton.upenn.
## edu/cgi-bin/getlog.cgi?username=buehl&request_id=)in separate files.


## Read WRDS file for acquirer. The meaning of the column names is as
## follows:
##    DATE:   Event date. A combination of ticker, exchange, and date
##            uniquely identifies a security. I think DATE shows the
##            first day when the TICKER was used (because it does not
##            show the first day when the NCUSIP or PERMNO was used).
##    COMNAM: CRSP company name
##    NCUSIP: CRSP historical CUSIP
##    TICKER: Exchangep-specific ticker 
##    PERMNO: CRSP primary identifier
##    EXCHCD: Code showing on which exchange the security is
##            listed (1=NYSE, 2=AMEX, 3=Nasdaq, ...)
WRDS.CRSP.Identifiers <-
  read.table(file = file.WRDS.CRSP.Identifiers,
             header = TRUE,
             sep = "\t",
             quote="",
             dec = ".",
             colClasses = "character", # Surpress conversion
             fill = TRUE,
             comment.char = "") # Turn off interpretation of comments.
## Convert dates.
WRDS.CRSP.Identifiers[, "DATE"] <-
  as.Date(strptime(WRDS.CRSP.Identifiers[, "DATE"], "%Y%m%d"))


## Create unique vector with 6-digit CUSIPs from the 8-digit CUSIPs
## from WRDS. This vector will be used to check whether I have found a
## WRDS 8-digit CUSIP for every 6-digit CUSIP in SDC.
CUSIP6 <- gsub("^(.{6}).*", "\\1",
                unique(WRDS.CRSP.Identifiers[, "NCUSIP"]),
                perl = TRUE)


## Find out which SDC acquirer CUSIPs are not in WRDS data.  
pos.OK <- which(takeoverData[, "Acquiror_CUSIP"] %in% CUSIP6)
pos.problem <-
  setdiff(intersect(which(!is.na(takeoverData[, "Acquiror_CUSIP"])),
                    1:dim(takeoverData)[1]),
          pos.OK)
rm(pos.OK) # Clean up.
## What are we dealing with here?
cat("Takeover where I can't find the SDC acquirer CUSIP in WRDS:\n")
print(takeoverData[pos.problem,
                      c("Date_Announced",
                        "Acquiror_Name", "Acquiror_CUSIP",
                        "Acquiror_Primary_Ticker_Symbol", 
                        "Target_Name", "Target_CUSIP")])
## Output looks as follows:
##  Date_Announced                Acquiror_Name Acquiror_CUSIP
## 1901     2000-03-06           Genzyme Biosurgery         37240C
## 1904     2000-03-06    Genzyme Surgical Products         37291H
## 2035     2000-09-07                     SATX Inc         78400J
## 2047     2000-09-26                 Rbid.com Inc         749283
## 2110     2000-12-20        Crowley Maritime Corp         228090
## 2202     2001-05-22             Golden Chest Inc         380816
## 2221     2001-06-13         Celera Genomics Corp         03802A
## 2299     2001-11-28    Apple Hospitality Two Inc         037846
## 2357     2002-04-30    Apple Hospitality Two Inc         037846
## 2416     2002-10-24    Apple Hospitality Two Inc         037846
## 2463     2003-03-26        Riverwood Holding Inc         76951B
## 2740     2005-07-25              Jane Butel Corp         470766
## 2932     2007-03-12        Robcor Properties Inc         77039C
## 3061     2008-03-25 Fortress Financial Group Inc         34956U
## 3096     2008-06-23                    Bunge Ltd         12056P
## 3128     2008-10-13 Health Systems Solutions Inc         42223R
## 3130     2008-10-20               Adrenalina Inc         00725L
## 3212     2009-09-23  International Minerals Corp         459875
##      
## Acquiror_Primary_Ticker_Symbol                   Target_Name
## 1901                           GZBX                 Biomatrix Inc
## 1904                           GZSP         Genzyme Tissue Repair
## 2035                          SATXE  Shared Technologies Cellular
## 2047                           RBDC   Herbalife International Inc
## 2110                           CWLM         Marine Transport Corp
## 2202                           GCHF Tri-National Development Corp
## 2221                            CRA      AXYS Pharmaceuticals Inc
## 2299                            N/A     Marriott Residence Inn LP
## 2357                            N/A  Marriott Residence Inn II LP
## 2416                            N/A              Apple Suites Inc
## 2463                            RVW   Graphic Packaging Intl Corp
## 2740                           JNBU              Bootie Beer Corp
## 2932                           RBCR             Redpoint Bio Corp
## 3061                           FFGO                Hunt Gold Corp
## 3096                             BG        Corn Products Intl Inc
## 3128                           HSSO                   Emageon Inc
## 3130                          AENAE     Pacific Sunwear of CA Inc
## 3212                            IMZ             Ventura Gold Corp
##     
##  Target_CUSIP                                    Remark
## 1901       09060P                    SDC has wrong CUSIP! 372917 
## 1904       37251H                    SDC has wrong CUSIP! 372917
## 2035       819487                    Not in CRSP                  
## 2047       426908                    Not in CRSP
## 2110       567912                    Not in CRSP
## 2202       89557H                    Not in CRSP
## 2221       054635                    Not in CRSP
## 2299       571641                    Not in CRSP                  
## 2357       57161X                    Not in CRSP
## 2416       037682                    Not in CRSP
## 2463       388690                    Not in CRSP
## 2740       099468                    Not in CRSP
## 2932       75771X                    Not in CRSP
## 3061       445623                    Not in CRSP
## 3096       219023                    SDC has wrong CUSIP! G169621
## 3128       29076V                    In CCM & SDC has correct CUSIP!       
##                                      But no output searching CUSIP
## 3130       694873                    Not in CRSP
## 3212       923219                    Not in CRSP


pos.OK <- which(takeoverData[, "Target_CUSIP"] %in% CUSIP6)
pos.problem <-
  setdiff(intersect(which(!is.na(takeoverData[, "Target_CUSIP"])),
                    1:dim(takeoverData)[1]),
          pos.OK)
rm(pos.OK, CUSIP6) # Clean up.
## What are we dealing with here?
cat("Takeovers where I can't find the SDC target CUSIP in WRDS:\n")
print(takeoverData[pos.problem,
                      c("Date_Announced",
                        "Acquiror_Name", "Acquiror_CUSIP",
                        "Target_Name", "Target_CUSIP",
                        "Target_Primary_Ticker_Symbol")])
## Output looks as follows.
## Date_Announced                 Acquiror_Name Acquiror_CUSIP
## 1843     2000-01-17          Parker Hannifin Corp         701094
## 1880     2000-02-14                      CMGI Inc         125750
## 1904     2000-03-06     Genzyme Surgical Products         37291H
## 2019     2000-08-22     Nextel Communications Inc         65332V
## 2058     2000-10-04                   Maxtor Corp         577729
## 2202     2001-05-22              Golden Chest Inc         380816
## 2228     2001-06-28                    Terex Corp         880779
## 2299     2001-11-28     Apple Hospitality Two Inc         037846
## 2325     2002-02-07   BioMarin Pharmaceutical Inc         09061G
## 2347     2002-04-02           Gray Television Inc         389190
## 2357     2002-04-30     Apple Hospitality Two Inc         037846
## 2384     2002-06-17        UnitedHealth Group Inc         91324P
## 2416     2002-10-24     Apple Hospitality Two Inc         037846
## 2427     2002-11-19                   Jarden Corp         471109
## 2448     2003-02-04 Perry Ellis International Inc         288853
## 2547     2003-10-30        Boston Scientific Corp         101137
## 2628     2004-08-09  US Restaurant Properties Inc         902971
## 2655     2004-11-11       Jones Apparel Group Inc         480074
## 2677     2005-01-18        Liberty Media Intl Inc         530719
## 2699     2005-03-17                      iPCS Inc         44980Y
## 2706     2005-04-14        Boston Scientific Corp         101137
## 2715     2005-05-12                  Cephalon Inc         156708
## 2740     2005-07-25               Jane Butel Corp         470766
## 2752     2005-08-29            Sprint Nextel Corp         852061
## 2760     2005-09-26                Corgentech Inc         21872P
## 2809     2006-04-03      EPIX Pharmaceuticals Inc         26881Q
## 2838     2006-07-19            Planar Systems Inc         726900
## 2876     2006-10-20 Developers Diversified Realty         251591
## 2932     2007-03-12         Robcor Properties Inc         77039C
## 2951     2007-04-27      DealerTrack Holdings Inc         242309
## 2973     2007-06-28                   infoUSA Inc         456818
## 3011     2007-10-08            ABM Industries Inc         000957
## 3018     2007-11-05                      Dell Inc         24702R
## 3048     2008-02-13               Hecla Mining Co         422704
## 3055     2008-03-04 International Game Technology         459902
## 3061     2008-03-25  Fortress Financial Group Inc         34956U
## 3093     2008-06-19                   Deluxe Corp         248019
## 3104     2008-07-15                ViroPharma Inc         928241
## 3171     2009-05-04             DirecTV Group Inc         25459L
## 3206     2009-09-04               Biogen Idec Inc         09062X
## 3207     2009-09-08               Windstream Corp         97381W
## 3212     2009-09-23   International Minerals Corp         459875
##                        Target_Name Target_CUSIP
## 1843     Commercial Intertech Corp       20170X
## 1880                    Tallan Inc       87441K
## 1904         Genzyme Tissue Repair       37251H
## 2019  Chadmoore Wireless Group Inc       157259
## 2058     Quantum HDD(Quantum Corp)       74795W
## 2202 Tri-National Development Corp       89557H
## 2228                      CMI Corp       12389Y
## 2299     Marriott Residence Inn LP       571641
## 2325          Glyko Biomedical Ltd       379904
## 2347       Stations Holding Co Inc       857694
## 2357  Marriott Residence Inn II LP       57161X
## 2384              AmeriChoice Corp       03062Q
## 2416              Apple Suites Inc       037682
## 2427            Diamond Brands Inc       252564
## 2448                   Salant Corp       794004
## 2547          Rubicon Medical Corp       78112Q
## 2628           CNL Income Fund Ltd       125928
## 2655          Barneys New York Inc       06808T
## 2677              UGC Holdings Inc       913277
## 2699               Horizon PCS Inc       44043U
## 2706          Rubicon Medical Corp       78112Q
## 2715                  Salmedix Inc       795467
## 2740              Bootie Beer Corp       099468
## 2752              IWO Holdings Inc       45071T
## 2760    AlgoRx Pharmaceuticals Inc       01586T
## 2809    Predix Pharmaceuticals Inc       74086H
## 2838    Clarity Visual Systems Inc       18063T
## 2876     Inland Retail Real Estate       45746N
## 2932             Redpoint Bio Corp       75771X
## 2951                    Arkona Inc       041268
## 2973                 Guideline Inc       401716
## 3011        OneSource Services Inc       68305X
## 3018                EqualLogic Inc       29440V
## 3048    Independence Lead Mines Co       453578
## 3055      Cyberview Technology Inc       23724Z
## 3061                Hunt Gold Corp       445623
## 3093              Hostopia.com Inc       44109A
## 3104       Lev Pharmaceuticals Inc       52730C
## 3171     Liberty Entertainment Inc       53044P
## 3206            Facet Biotech Corp       30474M
## 3207                    Lexcom Inc       528864
## 3212             Ventura Gold Corp       923219
##      Target_Primary_Ticker_Symbol        Remark
## 1843                          TEK        Not in CRSP       
## 1880                         TALN        Not in CRSP 
## 1904                         GZTR        Not in CRSP
## 2019                         MOOR        Not in CRSP
## 2058                          HDD        SDC has wrong CUSIP! 747906
## 2202                         TNAV        Not in CRSP
## 2228                          CMI        CMI CORP OKLA has CUSIP 125761
## 2299                         <NA>        Not in CRSP   
## 2325                          GBL        Not in CRSP
## 2347                         <NA>        Not in CRSP
## 2357                         <NA>        Not in CRSP
## 2384                         AMCH        Not in CRSP
## 2416                         <NA>        Not in CRSP
## 2427                         <NA>        In CCM & SDC has correct CUSIP!  
##                                          But no output searching CUSIP
## 2448                         SLNT        Not in CRSP
## 2547                         RMDC
## 2628                        XXKVD
## 2655                         BNNY
## 2677                        UCOMA
## 2699                         HZPS
## 2706                         RMDC
## 2715                         SMDX
## 2740                         BTIB
## 2752                         IWHD
## 2760                         AGRX
## 2809                         PRDX
## 2838                         <NA>
## 2876                        ZZILR
## 2932                         RPBC
## 2951                         ARKN
## 2973                         GDLN
## 3011                          OSS
## 3018                         EQLX
## 3048                         ILDM
## 3055                         CYBV
## 3061                         HGLC
## 3093                            H
## 3104                         LEVP
## 3171                        LMDIA
## 3206                         FACT
## 3207                        LXCMB
## 3212                          VGO


## Save converted WRDS.CRSP.Identifiers
save(WRDS.CRSP.Identifiers, file= file.WRDS.CRSP.Identifiers.Converted)
Sys.chmod(file.WRDS.CRSP.Identifiers.Converted, mode = "0400")

rm(file.CUSIPs.SDC,
   file.WRDS.CRSP.Identifiers,
   file.WRDS.CRSP.Identifiers.Converted,
   file.SDC.df.cleaned) # Clean up.


## printing out PERMNO, CRSP's identifier to CSV file so that I can later 
## use them for a WRDS web query to get data from CRSP.

write.table(unique(c(WRDS.CRSP.Identifiers[, "PERMNO"])),
            file = file.PERMNO.SDC,
            quote = FALSE, sep = "",
            row.names = FALSE, col.names = FALSE)
Sys.chmod(file.PERMNO.SDC, mode = "0400") # Write-protect file.
rm(file.PERMNO.SDC) # Clean up.


######################################################################
## This part of the program converts CUSIP to PERMCOs.
######################################################################

## Read firm identifiers that I've downloaded from WRDS: Click 'Tools'
## and then 'CRSP Tools - Translate to PERMCO/PERMNO (Annual
## Updates)' or point your browser to http://wrds-web.wharton.upenn.e
## du/wrds/ds/crsp/tools_a/dse/translate/index.cfm?navGroupHeader=Ann
## ual%20Update&navGroup=Tools
##
##
##
## For reproducibility, I have also saved the WRDS Data Request
## Summary and the WRDS SAS log (http://wrds-sol1.wharton.upenn.
## edu/cgi-bin/getlog.cgi?username=buehl&request_id=)in separate files.


WRDS.CRSP.Identifiers.PERMCO <-
  read.table(file = file.WRDS.CRSP.Identifiers.PERMCO,
             header = TRUE,
             sep = "\t",
             quote="",
             dec = ".",
             colClasses = "character", # Surpress conversion
             fill = TRUE,
             comment.char = "") # Turn off interpretation of comments.
## Convert dates.
WRDS.CRSP.Identifiers.PERMCO[, "DATE"] <-
  as.Date(strptime(WRDS.CRSP.Identifiers.PERMCO[, "DATE"], "%Y%m%d"))


## Create unique vector with 6-digit CUSIPs from the 8-digit CUSIPs
## from WRDS. This vector will be used to check whether I have found a
## WRDS 8-digit CUSIP for every 6-digit CUSIP in SDC.
CUSIP6 <- gsub("^(.{6}).*", "\\1",
                unique(WRDS.CRSP.Identifiers.PERMCO[, "NCUSIP"]),
                perl = TRUE)


## Find out which SDC acquirer CUSIPs are not in WRDS data.  
pos.OK <- which(takeoverData[, "Acquiror_CUSIP"] %in% CUSIP6)
pos.problem <-
  setdiff(intersect(which(!is.na(takeoverData[, "Acquiror_CUSIP"])),
                    1:dim(takeoverData)[1]),
          pos.OK)
rm(pos.OK) # Clean up.
## What are we dealing with here?
cat("Takeover where I can't find the SDC acquirer CUSIP in WRDS:\n")
print(takeoverData[pos.problem,
                      c("Date_Announced",
                        "Acquiror_Name", "Acquiror_CUSIP",
                        "Acquiror_Primary_Ticker_Symbol", 
                        "Target_Name", "Target_CUSIP")])
## Output looks as follows:
##  Date_Announced                Acquiror_Name Acquiror_CUSIP
## 1901     2000-03-06           Genzyme Biosurgery         37240C
## 1904     2000-03-06    Genzyme Surgical Products         37291H
## 2035     2000-09-07                     SATX Inc         78400J
## 2047     2000-09-26                 Rbid.com Inc         749283
## 2110     2000-12-20        Crowley Maritime Corp         228090
## 2202     2001-05-22             Golden Chest Inc         380816
## 2221     2001-06-13         Celera Genomics Corp         03802A
## 2299     2001-11-28    Apple Hospitality Two Inc         037846
## 2357     2002-04-30    Apple Hospitality Two Inc         037846
## 2416     2002-10-24    Apple Hospitality Two Inc         037846
## 2463     2003-03-26        Riverwood Holding Inc         76951B
## 2740     2005-07-25              Jane Butel Corp         470766
## 2932     2007-03-12        Robcor Properties Inc         77039C
## 3061     2008-03-25 Fortress Financial Group Inc         34956U
## 3096     2008-06-23                    Bunge Ltd         12056P
## 3128     2008-10-13 Health Systems Solutions Inc         42223R
## 3130     2008-10-20               Adrenalina Inc         00725L
## 3212     2009-09-23  International Minerals Corp         459875
##      
## Acquiror_Primary_Ticker_Symbol                   Target_Name
## 1901                           GZBX                 Biomatrix Inc
## 1904                           GZSP         Genzyme Tissue Repair
## 2035                          SATXE  Shared Technologies Cellular
## 2047                           RBDC   Herbalife International Inc
## 2110                           CWLM         Marine Transport Corp
## 2202                           GCHF Tri-National Development Corp
## 2221                            CRA      AXYS Pharmaceuticals Inc
## 2299                            N/A     Marriott Residence Inn LP
## 2357                            N/A  Marriott Residence Inn II LP
## 2416                            N/A              Apple Suites Inc
## 2463                            RVW   Graphic Packaging Intl Corp
## 2740                           JNBU              Bootie Beer Corp
## 2932                           RBCR             Redpoint Bio Corp
## 3061                           FFGO                Hunt Gold Corp
## 3096                             BG        Corn Products Intl Inc
## 3128                           HSSO                   Emageon Inc
## 3130                          AENAE     Pacific Sunwear of CA Inc
## 3212                            IMZ             Ventura Gold Corp
##     
##  Target_CUSIP                                    Remark
## 1901       09060P                    SDC has wrong CUSIP! 372917 
## 1904       37251H                    SDC has wrong CUSIP! 372917
## 2035       819487                    Not in CRSP                  
## 2047       426908                    Not in CRSP
## 2110       567912                    Not in CRSP
## 2202       89557H                    Not in CRSP
## 2221       054635                    Not in CRSP
## 2299       571641                    Not in CRSP                  
## 2357       57161X                    Not in CRSP
## 2416       037682                    Not in CRSP
## 2463       388690                    Not in CRSP
## 2740       099468                    Not in CRSP
## 2932       75771X                    Not in CRSP
## 3061       445623                    Not in CRSP
## 3096       219023                    SDC has wrong CUSIP! G169621
## 3128       29076V                    In CCM & SDC has correct CUSIP!       
##                                      But no output searching CUSIP
## 3130       694873                    Not in CRSP
## 3212       923219                    Not in CRSP


pos.OK <- which(takeoverData[, "Target_CUSIP"] %in% CUSIP6)
pos.problem <-
  setdiff(intersect(which(!is.na(takeoverData[, "Target_CUSIP"])),
                    1:dim(takeoverData)[1]),
          pos.OK)
rm(pos.OK, CUSIP6) # Clean up.
## What are we dealing with here?
cat("Takeovers where I can't find the SDC target CUSIP in WRDS:\n")
print(takeoverData[pos.problem,
                      c("Date_Announced",
                        "Acquiror_Name", "Acquiror_CUSIP",
                        "Target_Name", "Target_CUSIP",
                        "Target_Primary_Ticker_Symbol")])
## Output looks as follows.
## Date_Announced                 Acquiror_Name Acquiror_CUSIP
## 1843     2000-01-17          Parker Hannifin Corp         701094
## 1880     2000-02-14                      CMGI Inc         125750
## 1904     2000-03-06     Genzyme Surgical Products         37291H
## 2019     2000-08-22     Nextel Communications Inc         65332V
## 2058     2000-10-04                   Maxtor Corp         577729
## 2202     2001-05-22              Golden Chest Inc         380816
## 2228     2001-06-28                    Terex Corp         880779
## 2299     2001-11-28     Apple Hospitality Two Inc         037846
## 2325     2002-02-07   BioMarin Pharmaceutical Inc         09061G
## 2347     2002-04-02           Gray Television Inc         389190
## 2357     2002-04-30     Apple Hospitality Two Inc         037846
## 2384     2002-06-17        UnitedHealth Group Inc         91324P
## 2416     2002-10-24     Apple Hospitality Two Inc         037846
## 2427     2002-11-19                   Jarden Corp         471109
## 2448     2003-02-04 Perry Ellis International Inc         288853
## 2547     2003-10-30        Boston Scientific Corp         101137
## 2628     2004-08-09  US Restaurant Properties Inc         902971
## 2655     2004-11-11       Jones Apparel Group Inc         480074
## 2677     2005-01-18        Liberty Media Intl Inc         530719
## 2699     2005-03-17                      iPCS Inc         44980Y
## 2706     2005-04-14        Boston Scientific Corp         101137
## 2715     2005-05-12                  Cephalon Inc         156708
## 2740     2005-07-25               Jane Butel Corp         470766
## 2752     2005-08-29            Sprint Nextel Corp         852061
## 2760     2005-09-26                Corgentech Inc         21872P
## 2809     2006-04-03      EPIX Pharmaceuticals Inc         26881Q
## 2838     2006-07-19            Planar Systems Inc         726900
## 2876     2006-10-20 Developers Diversified Realty         251591
## 2932     2007-03-12         Robcor Properties Inc         77039C
## 2951     2007-04-27      DealerTrack Holdings Inc         242309
## 2973     2007-06-28                   infoUSA Inc         456818
## 3011     2007-10-08            ABM Industries Inc         000957
## 3018     2007-11-05                      Dell Inc         24702R
## 3048     2008-02-13               Hecla Mining Co         422704
## 3055     2008-03-04 International Game Technology         459902
## 3061     2008-03-25  Fortress Financial Group Inc         34956U
## 3093     2008-06-19                   Deluxe Corp         248019
## 3104     2008-07-15                ViroPharma Inc         928241
## 3171     2009-05-04             DirecTV Group Inc         25459L
## 3206     2009-09-04               Biogen Idec Inc         09062X
## 3207     2009-09-08               Windstream Corp         97381W
## 3212     2009-09-23   International Minerals Corp         459875
##                        Target_Name Target_CUSIP
## 1843     Commercial Intertech Corp       20170X
## 1880                    Tallan Inc       87441K
## 1904         Genzyme Tissue Repair       37251H
## 2019  Chadmoore Wireless Group Inc       157259
## 2058     Quantum HDD(Quantum Corp)       74795W
## 2202 Tri-National Development Corp       89557H
## 2228                      CMI Corp       12389Y
## 2299     Marriott Residence Inn LP       571641
## 2325          Glyko Biomedical Ltd       379904
## 2347       Stations Holding Co Inc       857694
## 2357  Marriott Residence Inn II LP       57161X
## 2384              AmeriChoice Corp       03062Q
## 2416              Apple Suites Inc       037682
## 2427            Diamond Brands Inc       252564
## 2448                   Salant Corp       794004
## 2547          Rubicon Medical Corp       78112Q
## 2628           CNL Income Fund Ltd       125928
## 2655          Barneys New York Inc       06808T
## 2677              UGC Holdings Inc       913277
## 2699               Horizon PCS Inc       44043U
## 2706          Rubicon Medical Corp       78112Q
## 2715                  Salmedix Inc       795467
## 2740              Bootie Beer Corp       099468
## 2752              IWO Holdings Inc       45071T
## 2760    AlgoRx Pharmaceuticals Inc       01586T
## 2809    Predix Pharmaceuticals Inc       74086H
## 2838    Clarity Visual Systems Inc       18063T
## 2876     Inland Retail Real Estate       45746N
## 2932             Redpoint Bio Corp       75771X
## 2951                    Arkona Inc       041268
## 2973                 Guideline Inc       401716
## 3011        OneSource Services Inc       68305X
## 3018                EqualLogic Inc       29440V
## 3048    Independence Lead Mines Co       453578
## 3055      Cyberview Technology Inc       23724Z
## 3061                Hunt Gold Corp       445623
## 3093              Hostopia.com Inc       44109A
## 3104       Lev Pharmaceuticals Inc       52730C
## 3171     Liberty Entertainment Inc       53044P
## 3206            Facet Biotech Corp       30474M
## 3207                    Lexcom Inc       528864
## 3212             Ventura Gold Corp       923219
##      Target_Primary_Ticker_Symbol        Remark
## 1843                          TEK        Not in CRSP       
## 1880                         TALN        Not in CRSP 
## 1904                         GZTR        Not in CRSP
## 2019                         MOOR        Not in CRSP
## 2058                          HDD        SDC has wrong CUSIP! 747906
## 2202                         TNAV        Not in CRSP
## 2228                          CMI        CMI CORP OKLA has CUSIP 125761
## 2299                         <NA>        Not in CRSP   
## 2325                          GBL        Not in CRSP
## 2347                         <NA>        Not in CRSP
## 2357                         <NA>        Not in CRSP
## 2384                         AMCH        Not in CRSP
## 2416                         <NA>        Not in CRSP
## 2427                         <NA>        In CCM & SDC has correct CUSIP!  
##                                          But no output searching CUSIP
## 2448                         SLNT        Not in CRSP
## 2547                         RMDC
## 2628                        XXKVD
## 2655                         BNNY
## 2677                        UCOMA
## 2699                         HZPS
## 2706                         RMDC
## 2715                         SMDX
## 2740                         BTIB
## 2752                         IWHD
## 2760                         AGRX
## 2809                         PRDX
## 2838                         <NA>
## 2876                        ZZILR
## 2932                         RPBC
## 2951                         ARKN
## 2973                         GDLN
## 3011                          OSS
## 3018                         EQLX
## 3048                         ILDM
## 3055                         CYBV
## 3061                         HGLC
## 3093                            H
## 3104                         LEVP
## 3171                        LMDIA
## 3206                         FACT
## 3207                        LXCMB
## 3212                          VGO


## Save converted WRDS.CRSP.Identifiers
save(WRDS.CRSP.Identifiers.PERMCO, file= file.WRDS.CRSP.PERMCO.Converted)
Sys.chmod(file.WRDS.CRSP.PERMCO.Converted, mode = "0400")

rm(file.CUSIPs.SDC,
   file.WRDS.CRSP.Identifiers.PERMCO,
   file.WRDS.CRSP.PERMCO.Converted) # Clean up.

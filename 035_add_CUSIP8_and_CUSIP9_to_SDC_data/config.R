## File name to write list of unique acquirer and target CUSIPs from
## 'file.SDC.old'. This data is needed to later feed it to WRDS, which
## converts the 6-digit CUSIPs to 8-digit CUSIPs.
file.CUSIPs.SDC <- file.path(dir.data, "010_020_CUSIPs_SDC.txt")


## This is the file in which I store the SDC data.frame 'SDC.data'
## after adding PERMNOs to each takeover case from CRSP. If there are
## multiple PERMNOs, I choose the one whose security has the highest
## trading volume in the year prior to the takeover resolution date.
file.SDC.with.PERMCOs <- file.path(dir.data, "033_010_SDC_data_with_PERMCOs.RData")


## This file contains the output of the WRDS web query
## http://wrds-web.wharton.upenn.edu/wrds/ds/crsp/tools/dse/translate/index.cfm
## where the input file to this web query is 'file.CUSIP6.all' from
## below.
file.CRSP.Ident <- file.path(dir.data, "010_030_CUSIP_WRDS_CRSP_PERMNO.txt")


#######################################################################
##
## Output files.
##
#######################################################################

## This file contains a list with unique 8-digit CUSIPs that I got
## from combining the 6-digit CUSIPs in SDC with various WRDS web
## queries.
file.CUSIP8.SDC <- file.path(dir.data, "035_010_CUSIP8_SDC.txt")

## This file contains all the 6-digit CUSIPs that the program could
## find. The file contents can then be fed into the WRDS web query
## that returns all matching CRSP identifiers.
file.CUSIP6.all <- file.path(dir.data, "035_010_CUSIP6_list_all.txt")

## This file contains the takeover data.frame where 8-digit CUSIPs and
## 9-digit CUSIPs have been added to it.
file.SDC.with.CUSIP9 <- file.path(dir.data, "035_030_SDC_data_with_CUSIP9.RData")

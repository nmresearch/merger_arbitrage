getTickNamCRSP <- function(PERMNO, DA, DRES, con.db, table.name.CRSP) {
  statement <- paste("SELECT * FROM ",
                     table.name.CRSP,
                     " WHERE PERMNO='", PERMNO,
                     "' AND DATE>='", as.numeric(DA)-1,
                     "' AND DATE<'", as.numeric(DRES),
                     "';", sep = "")
  df <- dbGetQuery(con.db, statement)
  df <- convert.CRSP.df(df)

  ## Use information that is most close to announcement date (here we
  ## are not worried about look-ahead bias).
  df <- df[which.min(abs(as.numeric(df$DATE - DA))), ]
  
  symb <- unique(df$TICKER) # Ticker symbol. See also 'TSYMBOL'.
  nam <- unique(df$COMNAM) # Company name.

  if (length(symb) == 0)
    symb <- NA_character_
  if (length(nam) == 0)
    nam <- NA_character_

  stopifnot(length(symb) == 1, length(nam) == 1)
  return(list(symb, nam))
}

getTickNamCCM <- function(PERMNO, DA, DRES, con.db, table.name.CCM.PERMNO) {
  statement <- paste("SELECT * FROM ",
                     table.name.CCM.PERMNO,
                     " WHERE lpermno='", PERMNO,
                     "';", sep = "")
  df <- dbGetQuery(con.db, statement)
  df <- convert.CCM.df(df)

  ## Don't use any information on or after announcement date. Also use
  ## the information that is most close to the announcement date (here
  ## we are not worried about look-ahead bias).
  df <- df[which(df$datadate < DRES), ]
  df <- df[which.min(abs(as.numeric(df$datadate - DA))), ]
  
  symb <- unique(df$tic) # Ticker symbol.
  nam <- unique(df$conm) # Company name.

  if (length(symb) == 0)
    symb <- NA_character_
  if (length(nam) == 0)
    nam <- NA_character_

  stopifnot(length(symb) == 1, length(nam) == 1)
  return(list(symb, nam))
}

#####################################################################
##
## This function reads contents (e.g. CRSP or CCM) into SQLite that
## were downloaded from WRDS. It assumes that the CSV file contains a
## header row. It stores dates in special formats because SQLite
## cannot deal with dates directly. When querying SQLite later on, you
## should re-convert some of the dates.
##
#####################################################################

create.db <-
  function (file.csv,    # CSV file to read from.
            batch.size,  # How many lines to read from file connection.
            col.Date,    # Columns that are of class 'Date'.
            col.DateQtr = NULL, # Cols of class 'yearqtr'; only matters for CCM
            col.numeric, # Columns that are of class 'numeric'.
            table.name)  # SQL table name to dump data into.
  { # Open function bracket.
    
    ## Open file connection to read CSV file from.
    con.file <- file(file.csv, "r")
    on.exit(close(con.file))

    header.from.csv <- read.table(file = con.file,
                                  header = FALSE,
                                  sep = "\t",
                                  quote = "\"",
                                  dec = ".",
                                  colClasses = "character", # Surpress conversion
                                  nrows = 1,
                                  fill = TRUE,
                                  comment.char = "")
    if (nrow(header.from.csv) != 1)
      stop("Something is wrong with 'header.from.csv'")
    header.from.csv <- # Fix names and convert to class 'character'.
      gsub("[.]", "_", as.character(header.from.csv), perl = TRUE)

    done <- FALSE
    while (!done) {
      df.from.csv <- read.table(file = con.file,
                                header = FALSE,
                                sep = "\t",
                                quote = "\"",
                                dec = ".",
                                col.names = header.from.csv,
                                colClasses = "character", # Surpress conversion
                                nrows = batch.size,
                                fill = TRUE,
                                comment.char = "")
      for (i in col.Date) { # Store 'Date' in special SQLite format.
        df.from.csv[, i] <- 
          as.numeric(as.Date(df.from.csv[, i], format = "%Y%m%d"))
      }
      for (i in col.DateQtr) { # Deal with quarters, internally save as char
        df.from.csv[, i] <- 
          as.character(as.yearqtr(df.from.csv[, i], format = "%YQ%q"))
      }
      for (i in col.numeric) { # Convert to 'numeric'.
        df.from.csv[, i] <- as.numeric(df.from.csv[, i])
      }
      if (nrow(df.from.csv) < batch.size) {
        done <- TRUE
        if (nrow(df.from.csv) == 0) {
          cat("Data frame has zero rows.\n")
          break
        }
      }
      dbWriteTable(con.db, table.name, df.from.csv,
                   row.names = FALSE, append = TRUE)
    }
    rm(i, header.from.csv, done, df.from.csv)

    ## ## Close file connection to CSV file.
    ## if (isOpen(con.file)) {
    ##   close(con.file)
    ##   rm(con.file)
    ## } else {
    ##   stop("Cannot close file connection.")
    ## }

  } # Close function bracket.

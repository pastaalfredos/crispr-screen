save_to_sql <- function() {

  # Import library
  library(DBI)

  # Load data if missing
  load_if_missing("full_data", "data/full_data.rda")

  # Connect to database
  mydb <- dbConnect(RSQLite::SQLite(), "full_data.sqlite")

  # Create table if it doesn't exist yet
  if (!dbExistsTable(mydb, "full_data")) {
    dbWriteTable(mydb, "full_data", full_data)
    message("Table created.")
  } else {
    message("Table already exists.")
  }

  # List tables to check
  print(dbListTables(mydb))

  # Run a test query
  print(dbGetQuery(mydb, 'SELECT * FROM full_data LIMIT 5'))

  # Disconnect when done
  dbDisconnect(mydb)

}


save_to_sql <- function() {

  # Import library
  library(DBI)
  library(RSQLite)

  # Load data if missing
  load_if_missing("full_data", "data/full_data.rda")

  # Connect to database
  conn <- dbConnect(SQLite(), "data/full_data.db")

  # Create table if it doesn't exist yet
  if (!dbExistsTable(conn, "full_data")) {
    dbWriteTable(conn, "full_data", full_data)
    message("Table created.")
  } else {
    message("Table already exists.")
  }

  # List tables to check
  print(dbListTables(conn))

  # Run a test query
  print(dbGetQuery(conn, 'SELECT * FROM full_data LIMIT 5'))

  # Disconnect when done
  dbDisconnect(conn)

}


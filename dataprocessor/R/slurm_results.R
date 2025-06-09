slurm_results <- function() {

library(DBI)
library(RSQLite)

# Read in results
slurm_res <- readRDS("data/slurm_results/slurm_results.rds")

# Make a database
conn <- dbConnect(SQLite(), "data/results.db")

# Save scalar values as summary table
summary_df <- data.frame(
  metric = c("r2", "mae", "rmse"),
  value = c(slurm_res$r2, slurm_res$mae, slurm_res$rmse)
)

dbWriteTable(conn, "metrics_summary", summary_df, overwrite = TRUE)

# Save predicted and expected as a comparison table
pred_df <- data.frame(expected = slurm_res$expected, predicted = slurm_res$predicted)

dbWriteTable(conn, "predictions", pred_df, overwrite = TRUE)

# Save importance into a dataframe
dbWriteTable(conn, "feature_importance", as.data.frame(slurm_res$importance), overwrite = TRUE)

# Serialize the model
raw_model <- serialize(slurm_res$model, NULL)

# Create the table manually (if it doesn't exist)
dbExecute(conn, "
  CREATE TABLE IF NOT EXISTS model_store (
    id INTEGER PRIMARY KEY,
    model_blob BLOB
  )
")

# Remove previous model (optional)
dbExecute(conn, "DELETE FROM model_store")

# Use parameter binding with dbBind for raw object
stmt <- dbSendStatement(conn, "INSERT INTO model_store (model_blob) VALUES (:blob)")
dbBind(stmt, list(blob = list(raw_model)))
dbClearResult(stmt)

# Retrieve model
raw_model <- dbGetQuery(conn, "SELECT model_blob FROM model_store")$model_blob[[1]]
loaded_model <- unserialize(raw_model)

}

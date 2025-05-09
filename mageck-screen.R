# Load libraries to read and manipulate data
library(readxl)
library(dplyr)

# Set file path to dataset
file_path <- "data/13059_2020_1940_MOESM3_ESM.xlsx"

# Read the dropout and cisplatin sheets
dropout_data <- read_excel(file_path, sheet = "Dropout")
cisplatin_data <- read_excel(file_path, sheet = "Cisplatin")

# Check column names
# colnames(dropout_data)
colnames(cisplatin_data)

# Sort for essential sgRNAs by low LFC and FDR
essential_sgrnas <- dropout_data %>% 
  select(sgrna = 1, gene = 2, lfc = 5, fdr = 12) %>% 
  filter(lfc < -1, fdr < 0.05) %>% arrange(fdr)

# Sort for drug resistant sgRNAs and senzitation sgRNAs
res_sgrnas <- cisplatin_data %>%
  select(sgrna = 1, gene = 2, lfc = 5, fdr = 12, high_in_treatment = 13) %>%
  filter(lfc > 0, fdr < 0.05, high_in_treatment == TRUE) %>%
  arrange(fdr)
sens_sgrnas <- cisplatin_data %>%
  select(sgrna = 1, gene = 2, lfc = 5, fdr = 12) %>%
  filter(lfc < -1, fdr < 0.05) %>%
  arrange(fdr)

# View the output
print(head(essential_sgrnas))
print(head(res_sgrnas))
print(head(sens_sgrnas))
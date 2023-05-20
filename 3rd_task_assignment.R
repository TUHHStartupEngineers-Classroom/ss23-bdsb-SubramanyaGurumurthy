# library(vroom)
# col_types <- list(
#   id = col_character(),
#   type = col_character(),
#   number = col_character(),
#   country = col_character(),
#   date = col_date("%Y-%m-%d"),
#   abstract = col_character(),
#   title = col_character(),
#   kind = col_character(),
#   num_claims = col_double(),
#   filename = col_character(),
#   withdrawn = col_double()
# )
# 
# patent_tbl <- vroom(
#   file       = "Patent_data_reduced/patent.tsv", 
#   delim      = "\t", 
#   col_types  = col_types,
#   na         = c("", "NA", "NULL")
# )

##############################################################################################
#1st task
##############################################################################################

#using data.table
library(data.table)

# Step 1: Read the asignee.tsv file
asignee_dt <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/assignee.tsv")

# Step 2: Read the patent_asignee.tsv file
patent_asignee_dt <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/patent_assignee.tsv")

# Merge the data.tables based on assignee_id
merged_dt <- merge(asignee_dt, patent_asignee_dt, by.x = "id", by.y = "assignee_id")

# Count the number of patents for each organization
patent_counts <- merged_dt[, .N, by = organization]

# Sort the patent counts in descending order
sorted_counts <- patent_counts[order(-N)]

# Select the top 10 companies with the most patents
top_10_companies <- sorted_counts[1:10]

# Print the result
print(top_10_companies)


#using dplyr
library(vroom)
library(dplyr)

asignee_file <- "C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/assignee.tsv"
patent_asignee_file <- "C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/patent_assignee.tsv"

top_10_companies <- vroom(asignee_file, col_select = c("id", "organization")) %>%
  inner_join(vroom(patent_asignee_file, col_select = c("patent_id", "assignee_id")), by = c("id" = "assignee_id")) %>%
  group_by(organization) %>%
  summarise(patent_count = n()) %>%
  arrange(desc(patent_count)) %>%
  head(10)

print(top_10_companies)


##############################################################################################
# 2nd task:
##############################################################################################

library(data.table)

# Read the files into data.tables
asignee_dt <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/assignee.tsv")
patent_asignee_dt <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/patent_assignee.tsv")
patent_dt <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/patent.tsv")

# Merge the data.tables based on assignee_id
merged_dt <- merge(merge(asignee_dt, patent_asignee_dt, by.x = "id", by.y = "assignee_id"), patent_dt, by.x = "patent_id", by.y = "id")

# Filter the merged data.table for patents granted in August 2014
august_2014_patents <- merged_dt[lubridate::year(date) == 2014 & lubridate::month(date) == 8]

# Count the number of patents for each organization in August 2014
patent_counts <- august_2014_patents[, .N, by = organization]

# Order the counts in descending order
ordered_counts <- patent_counts[order(-N)]

# Select the top 10 companies with the most new granted patents in August 2014
top_10_companies_in_august <- ordered_counts[1:10]

# Print the result
print(top_10_companies_in_august)

##############################################################################################
#3rd task
##############################################################################################

library(data.table)

# Read the files into data tables
asignee <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/assignee.tsv")
patent_asignee <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/patent_assignee.tsv")
uspc <- fread("C:/Users/LENOVO/OneDrive/Desktop/business_module/ss23-bdsb-SubramanyaGurumurthy/Patent_data_reduced/uspc.tsv")

# Merge the data tables
merged_dt <- merge(asignee, patent_asignee, by.x = "id", by.y = "assignee_id")
merged_dt <- merge(merged_dt, uspc, by = "patent_id")

# Count the number of patents per company
patents_per_company <- merged_dt[, .(patent_count = .N), by = organization]
top_10_companies <- patents_per_company[order(-patent_count)][1:10]

# Get the top 5 USPTO tech main classes for the top 10 companies
top_10_company_ids <- top_10_companies$organization
top_5_main_classes <- merged_dt[organization %in% top_10_company_ids, .N, by = mainclass_id][order(-N)][1:5]

# Print the results
most_innovative_tech_sector <- top_5_main_classes$mainclass_id
print(most_innovative_tech_sector)


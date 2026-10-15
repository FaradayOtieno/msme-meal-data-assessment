library(readxl)

file_path <- "C:/Users/user/Desktop/Meal_Master/Data-CaseStudySCKenya_Original.xlsx"

# List all worksheets
excel_sheets(file_path)

df <- read_excel(file_path, sheet = 1)

# Dataset dimensions
dim(df)

# Column names
names(df)

# First six rows
head(df)

# Data types and structure
str(df)

### === Step 3 — Perform the first data-quality checks === ###

# Missing values in each column
colSums(is.na(df))

# Exact duplicate rows
sum(duplicated(df))

# Unique values in key categorical variables
unique(df$County)
unique(df$Training)
unique(df$Size)

# Summary of revenue and employment
summary(df$Revenue)
summary(df$Employees)

### === investigate before cleaning === ###

# A. Inspect the duplicate and missing records

# Show the exact duplicate row
df[duplicated(df), ]

# Check whether any IDs are duplicated
sum(duplicated(df$ID))

# Display records missing revenue or employee counts
df[is.na(df$Revenue) | is.na(df$Employees),
   c("ID", "County", "Size", "Training",
     "Revenue", "Employees")]

# Check whether the missing values occur in the same rows
sum(is.na(df$Revenue) & is.na(df$Employees))

# B. Investigate unusually high revenue and employment

# Ten businesses with the highest revenue
head(df[order(df$Revenue, decreasing = TRUE),
        c("ID", "County", "Size",
          "Training", "Revenue", "Employees")], 10)

# Ten businesses with the highest employee counts
head(df[order(df$Employees, decreasing = TRUE),
        c("ID", "County", "Size",
          "Training", "Revenue", "Employees")], 10)

# Revenue distribution at selected percentiles
quantile(df$Revenue,
         probs = c(0, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99, 1),
         na.rm = TRUE)


# Employee-count distribution by business size
aggregate(
  Employees ~ Size,
  data = df,
  FUN = function(x) {
    c(
      n = sum(!is.na(x)),
      median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      maximum = max(x, na.rm = TRUE)
    )
  }
)

# Count records above selected thresholds
sapply(
  c(20, 50, 100, 500, 1000),
  function(cutoff) sum(df$Employees > cutoff, na.rm = TRUE)
)

df[
  df$Employees > 100 | df$Revenue > 1000000,
  c("ID", "County", "Size", "Gender of owner",
    "Training", "Revenue", "Employees")
]

# Confirm the number of actual rows
nrow(df)

# Count completely empty rows
sum(rowSums(!is.na(df)) == 0)

# Count missing IDs
sum(is.na(df$ID))

# Show businesses with very high employee counts
df[!is.na(df$Employees) & df$Employees >= 500,
   c("ID", "County", "Size", "Training",
     "Revenue", "Employees")]

### central question: are trained businesses performing differently 
### from untrained businesses?

# Step 4 — Compare trained and untrained MSME

# Compare business performance by training status
aggregate(
  cbind(Revenue, Employees) ~ Training,
  data = df,
  FUN = function(x) {
    c(
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE)
    )
  }
)

# Revenue by training and business size
aggregate(
  Revenue ~ Training + Size,
  data = df,
  FUN = function(x) {
    c(
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE)
    )
  }
)

# Employment by training and business size
aggregate(
  Employees ~ Training + Size,
  data = df,
  FUN = function(x) {
    c(
      n = sum(!is.na(x)),
      mean = mean(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE)
    )
  }
)

# investigate the data quality before drawing conclusions


library(dplyr)

df %>%
  group_by(Training) %>%
  summarise(
    businesses = n(),
    revenue_over_1m = sum(Revenue > 1000000, na.rm = TRUE),
    revenue_over_10m = sum(Revenue > 10000000, na.rm = TRUE),
    employees_over_20 = sum(Employees > 20, na.rm = TRUE),
    employees_over_100 = sum(Employees > 100, na.rm = TRUE),
    .groups = "drop"
  )


df %>%
  filter(!is.na(Revenue), Revenue > 10000000) %>%
  select(
    ID, County, Size, `Gender of owner`,
    `Type of major production: industrial classification (ISIC)`,
    Training, Revenue, Employees
  ) %>%
  arrange(desc(Revenue))

# Let's quantify how much the extreme values influence the results


df %>%
  group_by(Training) %>%
  summarise(
    businesses = sum(!is.na(Revenue)),
    mean_original = mean(Revenue, na.rm = TRUE),
    median_original = median(Revenue, na.rm = TRUE),
    mean_below_10m = mean(
      Revenue[!is.na(Revenue) & Revenue <= 10000000]
    ),
    median_below_10m = median(
      Revenue[!is.na(Revenue) & Revenue <= 10000000]
    ),
    .groups = "drop"
  )

# examine whether the pattern holds within business sizes


df %>%
  filter(!is.na(Revenue), Revenue <= 10000000) %>%
  group_by(Size, Training) %>%
  summarise(
    businesses = n(),
    median_revenue = median(Revenue),
    mean_revenue = mean(Revenue),
    .groups = "drop"
  )


library(dplyr)

# 1. Overall employment comparison by training status
df %>%
  group_by(Training) %>%
  summarise(
    businesses = sum(!is.na(Employees)),
    mean_employees = mean(Employees, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    minimum = min(Employees, na.rm = TRUE),
    maximum = max(Employees, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Employment comparison by training AND business size
df %>%
  filter(!is.na(Employees)) %>%
  group_by(Size, Training) %>%
  summarise(
    businesses = n(),
    mean_employees = mean(Employees),
    median_employees = median(Employees),
    minimum = min(Employees),
    maximum = max(Employees),
    .groups = "drop"
  ) %>%
  arrange(Size, Training)

# 3. Screen for unusually high employment counts
df %>%
  group_by(Training) %>%
  summarise(
    businesses = n(),
    employees_over_20 = sum(Employees > 20, na.rm = TRUE),
    employees_over_100 = sum(Employees > 100, na.rm = TRUE),
    employees_over_500 = sum(Employees > 500, na.rm = TRUE),
    .groups = "drop"
  )

# 4. Inspect records reporting more than 100 employees
df %>%
  filter(!is.na(Employees), Employees > 100) %>%
  select(
    ID, County, Size, `Gender of owner`,
    Training, Revenue, Employees
  ) %>%
  arrange(desc(Employees))

# 5. Diagnostic comparison excluding counts above 100 employees
# This does NOT permanently remove or modify records
# Compare employment by business size and training status
# Retain all recorded employee counts
df %>%
  filter(!is.na(Employees)) %>%
  group_by(Size, Training) %>%
  summarise(
    businesses = n(),
    mean_employees = mean(Employees),
    median_employees = median(Employees),
    .groups = "drop"
  ) %>%
  arrange(Size, Training)

# 6. Employment distribution by training status
df %>%
  group_by(Training) %>%
  summarise(
    businesses = sum(!is.na(Employees)),
    one_employee = sum(Employees == 1, na.rm = TRUE),
    two_to_five = sum(Employees >= 2 & Employees <= 5, na.rm = TRUE),
    six_to_twenty = sum(Employees >= 6 & Employees <= 20, na.rm = TRUE),
    over_twenty = sum(Employees > 20, na.rm = TRUE),
    .groups = "drop"
  )


library(dplyr)
library(stringr)
library(tibble)

# 1. Create a working copy and flag potential data-quality issues
df_dqa <- df %>%
  mutate(
    row_number_dqa = row_number(),
    exact_duplicate = duplicated(df),
    repeated_ID = duplicated(ID) | duplicated(ID, fromLast = TRUE),
    missing_revenue = is.na(Revenue),
    missing_employees = is.na(Employees),
    extreme_revenue = !is.na(Revenue) &
      Revenue > quantile(Revenue, 0.99, na.rm = TRUE),
    extreme_employees = !is.na(Employees) &
      Employees > quantile(Employees, 0.99, na.rm = TRUE)
  )

# 2. Summarise the overall data quality
repeated_id_groups <- df %>%
  count(ID, name = "records") %>%
  filter(!is.na(ID), records > 1)

dqa_summary <- tibble(
  check = c(
    "Total records",
    "Unique identifiers",
    "Repeated ID groups",
    "Extra exact-duplicate rows",
    "Missing revenue values",
    "Missing employee values",
    "Distinct county labels",
    "Revenue values above 99th percentile",
    "Employee values above 99th percentile"
  ),
  result = c(
    nrow(df),
    n_distinct(df$ID, na.rm = TRUE),
    nrow(repeated_id_groups),
    sum(duplicated(df)),
    sum(is.na(df$Revenue)),
    sum(is.na(df$Employees)),
    n_distinct(df$County, na.rm = TRUE),
    sum(df_dqa$extreme_revenue),
    sum(df_dqa$extreme_employees)
  )
)

# 3. Create the DQA issue log
dqa_log <- tibble(
  issue_id = c("DQ01", "DQ02", "DQ03", "DQ04", "DQ05"),
  issue = c(
    "Repeated identifiers",
    "Exact duplicate record",
    "Inconsistent county labels",
    "Extreme values",
    "Missing data"
  ),
  evidence = c(
    paste(nrow(repeated_id_groups),
          "ID groups have repeated records"),
    paste(sum(duplicated(df)),
          "extra exact-duplicate rows"),
    paste(n_distinct(df$County, na.rm = TRUE),
          "distinct raw county labels"),
    paste(sum(df_dqa$extreme_revenue),
          "extreme revenue flags and",
          sum(df_dqa$extreme_employees),
          "extreme employee flags"),
    paste(sum(is.na(df$Revenue)),
          "missing revenue and",
          sum(is.na(df$Employees)),
          "missing employee values")
  ),
  corrective_action = c(
    "Verify the meaning of ID against the source",
    "Verify the duplicate before excluding a record",
    "Map county labels to the official county list",
    "Check flagged values against source records",
    "Investigate missingness and document treatment"
  ),
  status = "Pending verification"
)

# 4. Display the outputs needed for the MEAL report
cat("\n--- DQA SUMMARY ---\n")
print(dqa_summary, n = Inf)

cat("\n--- DQA ISSUE LOG ---\n")
print(dqa_log, n = Inf)

cat("\n--- FIRST 10 FLAGGED RECORDS ---\n")
df_dqa %>%
  filter(
    exact_duplicate | repeated_ID |
      extreme_revenue | extreme_employees |
      missing_revenue | missing_employees
  ) %>%
  select(
    row_number_dqa, ID, County, Size,
    Revenue, Employees, exact_duplicate,
    repeated_ID, extreme_revenue,
    extreme_employees, missing_revenue,
    missing_employees
  ) %>%
  head(10) %>%
  print(n = 10)


library(dplyr)
library(tidyr)

# Monitoring 1: Overall programme-related data profile
monitoring_overview <- df %>%
  summarise(
    records = n(),
    businesses_with_training_status = sum(!is.na(Training)),
    trained_records = sum(Training == "Received training", na.rm = TRUE),
    untrained_records = sum(Training == "No training", na.rm = TRUE),
    median_revenue = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE)
  )

# Monitoring 2: Results by training status
training_comparison <- df %>%
  group_by(Training) %>%
  summarise(
    records = n(),
    median_revenue = median(Revenue, na.rm = TRUE),
    mean_revenue = mean(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    mean_employees = mean(Employees, na.rm = TRUE),
    .groups = "drop"
  )

# Monitoring 3: Disaggregation by business size
size_comparison <- df %>%
  group_by(Size, Training) %>%
  summarise(
    records = n(),
    median_revenue = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    .groups = "drop"
  )

# Monitoring 4: Disaggregation by owner's gender
gender_comparison <- df %>%
  group_by(`Gender of owner`, Training) %>%
  summarise(
    records = n(),
    median_revenue = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- OVERALL PROFILE ---\n")
print(monitoring_overview)

cat("\n--- TRAINING COMPARISON ---\n")
print(training_comparison)

cat("\n--- RESULTS BY BUSINESS SIZE ---\n")
print(size_comparison, n = Inf)

cat("\n--- RESULTS BY OWNER GENDER ---\n")
print(gender_comparison, n = Inf)


# Evaluation

evaluation_summary <- df %>%
  group_by(Training) %>%
  summarise(
    records = n(),
    median_revenue = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    revenue_difference = median_revenue -
      median_revenue[Training == "No training"],
    employment_difference = median_employees -
      median_employees[Training == "No training"]
  )

print(evaluation_summary, n = Inf)

# Accountability & Learning


meal_actions <- tibble(
  area = c(
    "Data quality",
    "Monitoring",
    "Evaluation",
    "Accountability",
    "Learning"
  ),
  finding = c(
    "Repeated IDs, county-label inconsistencies, extreme values and missing data",
    "Training status and business outcomes are available",
    "Trained businesses have higher median revenue and employment",
    "No feedback or complaints data are available",
    "Differences warrant further investigation"
  ),
  recommended_action = c(
    "Verify source records and document all corrections",
    "Define indicators, denominators, targets and reporting periods",
    "Collect baseline and follow-up data and assess other explanations",
    "Establish accessible feedback channels and a response log",
    "Discuss findings with stakeholders and track agreed actions"
  ),
  evidence_needed = c(
    "Original records and data dictionary",
    "Programme targets and monitoring plan",
    "Baseline, follow-up and evaluation design",
    "Feedback, complaints and resolution records",
    "Meeting notes, action owners and completion dates"
  ),
  status = "Pending"
)

print(meal_actions, n = Inf)

### Cleaned dataset


library(dplyr)
library(stringr)
library(tibble)
library(readr)

# 1. Preserve the original and create a working copy
df_original <- df

df_work <- df_original %>%
  mutate(
    source_row = row_number(),
    County_raw = County,
    County_clean = str_squish(County)
  )

# 2. Standardise only county variants identified in our inspection
df_work <- df_work %>%
  mutate(
    County_clean = case_when(
      str_to_lower(County_clean) %in%
        c("nairobii", "nairobii.", "naiirobi", "narobi",
          "nairo bii", "nairob", "nai robi", "nairob i") ~ "Nairobi",
      
      str_to_lower(County_clean) %in%
        c("kiaambu", "kiam bu") ~ "Kiambu",
      
      str_to_lower(County_clean) == "kisu mu" ~ "Kisumu",
      
      str_to_lower(County_clean) %in%
        c("mombas a", "mombasaa") ~ "Mombasa",
      
      str_to_lower(County_clean) == "machako s" ~ "Machakos",
      
      str_to_lower(County_clean) == "merru" ~ "Meru",
      
      str_to_lower(County_clean) %in%
        c("nakuru.", "nakurru") ~ "Nakuru",
      
      str_to_lower(County_clean) == "uasin  gishu" ~ "Uasin Gishu",
      
      TRUE ~ County_clean
    )
  )

# 3. Flag exact duplicate rows BEFORE removing any
df_work <- df_work %>%
  mutate(
    exact_duplicate = duplicated(
      df_original
    )
  )

exact_duplicate_log <- df_work %>%
  filter(exact_duplicate) %>%
  select(source_row, ID, County_raw, County_clean,
         Training, Revenue, Employees)

# 4. Flag repeated IDs; retain these records pending verification
repeated_ids <- df_work %>%
  count(ID, name = "records_with_id") %>%
  filter(!is.na(ID), records_with_id > 1)

df_work <- df_work %>%
  mutate(
    repeated_ID = ID %in% repeated_ids$ID
  )

# 5. Flag unusual values for investigation, not automatic deletion
revenue_p99 <- quantile(
  df_work$Revenue, 0.99, na.rm = TRUE
)

employees_p99 <- quantile(
  df_work$Employees, 0.99, na.rm = TRUE
)

df_work <- df_work %>%
  mutate(
    extreme_revenue = !is.na(Revenue) &
      Revenue > revenue_p99,
    
    extreme_employees = !is.na(Employees) &
      Employees > employees_p99
  )

# 6. Create the analytical copy
# Exclude only the confirmed exact duplicate row identified
# by exact equality across the original eight columns.
df_clean <- df_work %>%
  filter(!exact_duplicate)

# 7. Produce an audit summary
cleaning_summary <- tibble(
  check = c(
    "Original records",
    "Records in analytical copy",
    "Exact duplicate rows excluded",
    "Repeated-ID groups retained for review",
    "Rows with missing revenue",
    "Rows with missing employees",
    "Distinct county labels before standardisation",
    "Distinct county labels after standardisation",
    "Extreme revenue flags retained",
    "Extreme employee flags retained"
  ),
  result = c(
    nrow(df_original),
    nrow(df_clean),
    sum(df_work$exact_duplicate),
    nrow(repeated_ids),
    sum(is.na(df_clean$Revenue)),
    sum(is.na(df_clean$Employees)),
    n_distinct(df_work$County_raw),
    n_distinct(df_clean$County_clean),
    sum(df_clean$extreme_revenue),
    sum(df_clean$extreme_employees)
  )
)

print(cleaning_summary, n = Inf)

cat("\n--- EXACT DUPLICATE EXCLUDED ---\n")
print(exact_duplicate_log)

cat("\n--- COUNTY MAPPING REVIEW ---\n")
print(
  df_work %>%
    distinct(County_raw, County_clean) %>%
    arrange(County_clean, County_raw),
  n = Inf
)

cat("\n--- REPEATED ID EXAMPLES ---\n")
print(
  df_clean %>%
    filter(repeated_ID) %>%
    select(source_row, ID, County_raw, County_clean,
           Size, Training, Revenue, Employees) %>%
    head(15),
  n = 15
)

### Validate the cleaning


# Check that the original dataset remains unchanged
stopifnot(identical(df, df_original))

# Check the expected record count after excluding one exact duplicate
stopifnot(nrow(df_clean) == nrow(df_original) - 1)

# Check that missing values have not been imputed
stopifnot(sum(is.na(df_clean$Revenue)) == 6)
stopifnot(sum(is.na(df_clean$Employees)) == 6)

# Check that all original records other than the excluded
# exact duplicate remain represented in the analytical copy
cat("Original rows:", nrow(df_original), "\n")
cat("Analytical rows:", nrow(df_clean), "\n")
cat("Repeated-ID rows retained:",
    sum(df_clean$repeated_ID), "\n")
cat("Missing revenue:", sum(is.na(df_clean$Revenue)), "\n")
cat("Missing employees:", sum(is.na(df_clean$Employees)), "\n")

# Save separate files for reproducibility
write_csv(df_clean, "MSME_MEAL_Cleaned_Independent.csv")
write_csv(cleaning_summary, "MSME_MEAL_Cleaning_Audit.csv")
write_csv(exact_duplicate_log, "MSME_MEAL_Excluded_Duplicate_Log.csv")

### Recalculate the key MEAL results


compare_training <- function(data, dataset_name) {
  data %>%
    group_by(Training) %>%
    summarise(
      dataset = dataset_name,
      records = n(),
      revenue_n = sum(!is.na(Revenue)),
      median_revenue = median(Revenue, na.rm = TRUE),
      mean_revenue = mean(Revenue, na.rm = TRUE),
      employees_n = sum(!is.na(Employees)),
      median_employees = median(Employees, na.rm = TRUE),
      mean_employees = mean(Employees, na.rm = TRUE),
      .groups = "drop"
    )
}

training_results <- bind_rows(
  compare_training(df_original, "Original"),
  compare_training(
    df_clean %>%
      mutate(County = County_clean),
    "Independently cleaned"
  )
)

print(training_results, n = Inf)

# Compare county counts before and after standardisation
county_comparison <- bind_rows(
  df_original %>%
    count(County, name = "records") %>%
    mutate(version = "Original"),
  
  df_clean %>%
    count(County_clean, name = "records") %>%
    rename(County = County_clean) %>%
    mutate(version = "Independently cleaned")
) %>%
  arrange(version, County)

print(county_comparison, n = Inf)
write_csv(training_results, "MSME_MEAL_Training_Comparison.csv")


### Cleaned copy analysis


# 1. Overall monitoring indicators
monitoring_final <- df_clean %>%
  summarise(
    records = n(),
    trained = sum(Training == "Received training"),
    untrained = sum(Training == "No training"),
    training_coverage_pct = round(
      100 * mean(Training == "Received training"), 1
    ),
    median_revenue = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE)
  )

print(monitoring_final)

# 2. Final comparison by training status
training_final <- df_clean %>%
  group_by(Training) %>%
  summarise(
    businesses = n(),
    revenue_observations = sum(!is.na(Revenue)),
    median_revenue_KES = median(Revenue, na.rm = TRUE),
    mean_revenue_KES = round(mean(Revenue, na.rm = TRUE)),
    employee_observations = sum(!is.na(Employees)),
    median_employees = median(Employees, na.rm = TRUE),
    mean_employees = round(mean(Employees, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(training_final, n = Inf)

# 3. Compare median outcomes by business size
size_final <- df_clean %>%
  group_by(Size, Training) %>%
  summarise(
    businesses = n(),
    median_revenue_KES = median(Revenue, na.rm = TRUE),
    median_employees = median(Employees, na.rm = TRUE),
    .groups = "drop"
  )

print(size_final, n = Inf)

# 4. Save the final tables for the article
write_csv(monitoring_final, "MSME_MEAL_Final_Monitoring.csv")
write_csv(training_final, "MSME_MEAL_Final_Training_Results.csv")
write_csv(size_final, "MSME_MEAL_Final_Size_Results.csv")

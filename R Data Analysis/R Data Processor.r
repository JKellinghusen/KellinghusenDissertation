print("Processing...")
# Define the file name to read
file_name <- "C:/Users/jtorr/OneDrive/DrPH/Dissertation/Dissertation Raw Materials/Final Questionnaire Test Data.csv"

# Define the new file name to save the processed data
new_file_name <- sub("\\.csv$", " processed.csv", file_name)

# Load the data from the CSV file
data <- read.csv(file_name, stringsAsFactors = FALSE)

# Check if the file was successfully read
if (!is.null(data)) {
  cat("File successfully read\n")
} else {
  cat("Failed to read the file\n")
}

# Define new variables for question text
question_text <- list()
columns_to_use <- c("Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q3.2_1", "Q3.2_2", "Q3.2_3", "Q3.2_4", "Q3.2_5", "Q3.2_6", "Q3.3_1", "Q3.3_2", "Q3.3_3", "Q3.3_4", "Q3.3_5", "Q3.3_6", "Q4.2", "Q4.3", "Q4.4", "Q5.2", "Q5.3", "Q6.2", "Q6.3_1", "Q7.2_1", "Q7.2_2", "Q7.2_3", "Q7.2_4", "Q7.2_5", "Q7.2_6", "Q7.2_7", "Q7.2_8", "Q8.2", "Q8.3", "Q9.2", "Q9.3_1", "Q10.2_1", "Q10.2_2", "Q10.2_3", "Q10.2_4", "Q10.2_5", "Q10.2_6", "Q10.2_7", "Q10.2_8", "Q10.2_9", "Q10.2_10", "Q10.3", "Q11.2_1", "Q11.3_1", "Q12.2_1", "Q12.2_2", "Q12.2_3", "Q13.2_1", "Q13.3_1", "Q14.2_1", "Q14.3", "Q14.4_1", "Q15.2_1", "Q15.3_1", "Q16.2", "Q16.3_1", "Q17.2_1", "Q17.2_2", "Q18.2_1", "Q18.2_2")

for (col in columns_to_use) {
  if (col %in% colnames(data)) {
    question_text[[col]] <- data[1, col]
  } else {
    question_text[[col]] <- NA
  }
}

# Remove rows 2 and 3 from the dataset
if (nrow(data) >= 2) {
  data <- data[-c(1, 2), ]
} else {
  cat("Warning: Not enough rows to remove rows 2 and 3.\n")
}

# Remove rows where column "Q1.2" has a value of 1
if ("Q1.2" %in% colnames(data)) {
  data <- data[data$Q1.2 != 1, ]
} else {
  cat("Column 'Q1.2' does not exist in the data.\n")
}
# Remove specified columns from the dataset
columns_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration..in.seconds.", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage", "Q2.6", "Q3.4", "Q4.5", "Q5.4", "Q6.4", "Q7.3", "Q8.4", "Q9.4", "Q10.4", "Q11.4", "Q12.3", "Q13.4", "Q14.5", "Q15.4", "Q16.4", "Q17.3", "Q18.3", "Q5.3_1_TEXT", "Q10.2_10_TEXT", "Q14.3_1_TEXT", "Q1.2")
data <- data[, !(colnames(data) %in% columns_to_remove)]
columns_to_remove <- c("StartDate", "EndDate", "Status", "IPAddress", "Progress", "Duration..in.seconds.", "Finished", "RecordedDate", "ResponseId", "RecipientLastName", "RecipientFirstName", "RecipientEmail", "ExternalReference", "LocationLatitude", "LocationLongitude", "DistributionChannel", "UserLanguage", "Q2.6", "Q3.4", "Q4.5", "Q5.4", "Q6.4", "Q7.3", "Q8.4", "Q9.4", "Q10.4", "Q11.4", "Q12.3", "Q13.4", "Q14.5", "Q15.4", "Q16.4", "Q17.3", "Q18.3")
data <- data[, !(colnames(data) %in% columns_to_remove)]

# Delete any rows that are completely blank
data <- data[rowSums(is.na(data)) != ncol(data), ]

# Load necessary library
library(openxlsx)

# Create a workbook
wb <- createWorkbook()

# Remove any string that reads "0" from columns Q5.3 and Q14.3
cols_remove_zeros <- c("Q5.3", "Q14.3")
data[cols_remove_zeros] <- lapply(data[cols_remove_zeros], function(x) {
  gsub("\\b0\\b", "", as.character(x))
})

# Replace cell content based on character length for columns Q5.3 and Q14.3
cols_replace_length <- c("Q5.3", "Q14.3")
data[cols_replace_length] <- lapply(data[cols_replace_length], function(x) {
  sapply(nchar(as.character(x)), function(len) {
    if (len == 0) {
      return(0)
    } else if (len == 1) {
      return(1)
    } else if (len == 3) {
      return(2)
    } else if (len == 5) {
      return(3)
    } else if (len == 7) {
      return(4)
    } else if (len == 9) {
      return(5)
    } else {
      return(x)
    }
  })
})

# Change the format of specified columns to "number"
cols_to_number <- c("Q2.2", "Q2.3", "Q2.4", "Q2.5", "Q3.2_1", "Q3.2_2", "Q3.2_3", "Q3.2_4", "Q3.2_5", "Q3.2_6", "Q3.3_1", "Q3.3_2", "Q3.3_3", "Q3.3_4", "Q3.3_5", "Q3.3_6", "Q4.2", "Q4.3", "Q4.4", "Q5.2", "Q5.3", "Q6.2", "Q6.3_1", "Q7.2_1", "Q7.2_2", "Q7.2_3", "Q7.2_4", "Q7.2_5", "Q7.2_6", "Q7.2_7", "Q7.2_8", "Q8.2", "Q8.3", "Q9.2", "Q9.3_1", "Q10.2_1", "Q10.2_2", "Q10.2_3", "Q10.2_4", "Q10.2_5", "Q10.2_6", "Q10.2_7", "Q10.2_8", "Q10.2_9", "Q10.2_10", "Q10.3", "Q11.2_1", "Q11.3_1", "Q12.2_1", "Q12.2_2", "Q12.2_3", "Q13.2_1", "Q13.3_1", "Q14.2_1", "Q14.3", "Q14.4_1", "Q15.2_1", "Q15.3_1", "Q16.2", "Q16.3_1", "Q17.2_1", "Q17.2_2", "Q18.2_1", "Q18.2_2")
data[cols_to_number] <- lapply(data[cols_to_number], function(x) as.numeric(as.character(x)))

# Divide the values in specified columns by 6
cols_div_6 <- c("Q5.3")
data[cols_div_6] <- lapply(data[cols_div_6], function(x) x / 6)

# Divide the values in specified columns by 5
cols_div_5 <- c("Q2.2", "Q3.2_1", "Q3.2_2", "Q3.2_3", "Q3.2_4", "Q3.2_5", "Q3.2_6", "Q3.3_1", "Q3.3_2", "Q3.3_3", "Q3.3_4", "Q3.3_5", "Q3.3_6", "Q5.2", "Q6.3_1", "Q7.2_1", "Q7.2_2", "Q7.2_3", "Q7.2_4", "Q7.2_5", "Q7.2_6", "Q7.2_7", "Q7.2_8", "Q9.3_1", "Q10.2_1", "Q10.2_2", "Q10.2_3", "Q10.2_4", "Q10.2_5", "Q10.2_6", "Q10.2_7", "Q10.2_8", "Q10.2_9", "Q10.2_10", "Q11.2_1", "Q11.3_1", "Q12.2_1", "Q12.2_2", "Q12.2_3", "Q13.2_1", "Q13.3_1", "Q14.2_1", "Q14.3", "Q14.4_1", "Q15.2_1", "Q15.3_1", "Q16.3_1", "Q17.2_1", "Q17.2_2", "Q18.2_1", "Q18.2_2")
data[cols_div_5] <- lapply(data[cols_div_5], function(x) x / 5)

# Scale specified columns by dividing their values by 4
cols_div_4 <- c("Q2.3", "Q2.4", "Q9.2")
data[cols_div_4] <- lapply(data[cols_div_4], function(x) x / 4)

# Divide the values in specified columns by 3
cols_div_3 <- c("Q2.5", "Q4.2", "Q4.3", "Q4.4", "Q6.2", "Q8.2", "Q8.3", "Q10.3", "Q16.2")
data[cols_div_3] <- lapply(data[cols_div_3], function(x) x / 3)

# Create a new variable for each column whose value is the average of the numbers in said column
for (col in cols_to_number) {
  assign(col, mean(data[[col]], na.rm = TRUE))
}

# Define the Components and their corresponding columns
components <- list(
  "Prioritize workplace physical safety" = c("Q2.2", "Q2.3", "Q2.4", "Q2.5"),
  "Prioritize workplace psychological safety" = c("Q3.2_1", "Q3.2_2", "Q3.2_3", "Q3.2_4", "Q3.2_5", "Q3.2_6", "Q3.3_1", "Q3.3_2", "Q3.3_3", "Q3.3_4", "Q3.3_5", "Q3.3_6"),
  "Enable adequate rest" = c("Q4.2", "Q4.3", "Q4.4"),
  "Normalize and support mental health" = c("Q5.2", "Q5.3"),
  "Operationalize DEIA* norms, policies, and programs" = c("Q6.2", "Q6.3_1"),
  "Create cultures of inclusion and belonging" = c("Q7.2_1", "Q7.2_2", "Q7.2_3", "Q7.2_4"),
  "Cultivate trusted relationships" = c("Q7.2_5", "Q7.2_6"),
  "Foster collaboration and teamwork" = c("Q7.2_7", "Q7.2_8"),
  "Provide more autonomy over how work is done" = c("Q8.2", "Q8.3"),
  "Make schedules as flexible and predictable as possible" = c("Q9.2", "Q9.3_1"),
  "Increase access to paid leave" = c("Q10.2_1", "Q10.2_2", "Q10.2_3", "Q10.2_4", "Q10.2_5", "Q10.2_6", "Q10.2_7", "Q10.2_8", "Q10.2_9", "Q10.2_10", "Q10.3"),
  "Respect boundaries between work and non-work time" = c("Q11.2_1", "Q11.3_1"),
  "Provide a living wage" = c("Q12.2_1", "Q12.2_2", "Q12.2_3"),
  "Engage workers in workplace decisions" = c("Q13.2_1", "Q13.3_1"),
  "Build a culture of gratitude and recognition" = c("Q14.2_1", "Q14.3", "Q14.4_1"),
  "Connect individual work with organizational mission" = c("Q15.2_1", "Q15.3_1"),
  "Offer quality training, education, and mentoring" = c("Q16.2", "Q16.3_1"),
  "Foster clear, equitable pathways for career advancement" = c("Q17.2_1", "Q17.2_2"),
  "Ensure relevant, reciprocal feedback" = c("Q18.2_1", "Q18.2_2")
)

# Create and print the Components variables
for (component in names(components)) {
  cols <- components[[component]]
  component_value <- mean(sapply(cols, function(col) get(col)), na.rm = TRUE)
  assign(component, component_value)
}

# Convert all columns to character to avoid list type issues
data[] <- lapply(data, as.character)

# Save a new CSV file with "processed" at the end of the name in the same folder as the original
write.csv(data, new_file_name, row.names = FALSE)

# Define the plot chart data for each category
protection_from_harm <- c(
  "Prioritize workplace physical safety" = `Prioritize workplace physical safety`,
  "Prioritize workplace psychological safety" = `Prioritize workplace psychological safety`,
  "Enable adequate rest" = `Enable adequate rest`,
  "Normalize and support mental health" = `Normalize and support mental health`,
  "Operationalize DEIA* norms, policies, and programs" = `Operationalize DEIA* norms, policies, and programs`
)

connection_and_community <- c(
  "Create cultures of inclusion and belonging" = `Create cultures of inclusion and belonging`,
  "Cultivate trusted relationships" = `Cultivate trusted relationships`,
  "Foster collaboration and teamwork" = `Foster collaboration and teamwork`
)

work_life_harmony <- c(
  "Provide more autonomy over how work is done" = `Provide more autonomy over how work is done`,
  "Make schedules as flexible and predictable as possible" = `Make schedules as flexible and predictable as possible`,
  "Increase access to paid leave" = `Increase access to paid leave`,
  "Respect boundaries between work and non-work time" = `Respect boundaries between work and non-work time`
)

mattering_at_work <- c(
  "Provide a living wage" = `Provide a living wage`,
  "Engage workers in workplace decisions" = `Engage workers in workplace decisions`,
  "Build a culture of gratitude and recognition" = `Build a culture of gratitude and recognition`,
  "Connect individual work with organizational mission" = `Connect individual work with organizational mission`
)

opportunity_for_growth <- c(
  "Offer quality training, education, and mentoring" = `Offer quality training, education, and mentoring`,
  "Foster clear, equitable pathways for career advancement" = `Foster clear, equitable pathways for career advancement`,
  "Ensure relevant, reciprocal feedback" = `Ensure relevant, reciprocal feedback`
)

# Define Essentials averages
essentials_averages <- list()

plot_data <- list(
  "Protection from Harm" = protection_from_harm,
  "Connection & Community" = connection_and_community,
  "Work-Life Harmony" = work_life_harmony,
  "Mattering at Work" = mattering_at_work,
  "Opportunity for Growth" = opportunity_for_growth
)

for (essential in names(plot_data)) {
  essential_components <- plot_data[[essential]]
  essential_value <- mean(sapply(essential_components, function(x) x), na.rm = TRUE)
  essentials_averages[[essential]] <- essential_value
}

# Create a table that displays all the question_text values in the first column and column names in the second column
question_text_df <- data.frame(
  Question = names(question_text),
  Text = unlist(question_text),
  Column_Name = names(question_text),
  stringsAsFactors = FALSE
)

# Create a summary table with count, min, max, average, and median for each column
summary_stats <- data.frame(
  Column = colnames(data),
  Count = sapply(data, function(x) sum(!is.na(x))),
  Min = sapply(data, function(x) round(min(as.numeric(x), na.rm = TRUE), 2)),
  Max = sapply(data, function(x) round(max(as.numeric(x), na.rm = TRUE), 2)),
  Average = sapply(data, function(x) round(mean(as.numeric(x), na.rm = TRUE), 2)),
  Median = sapply(data, function(x) round(median(as.numeric(x), na.rm = TRUE), 2)),
  stringsAsFactors = FALSE
)

# Combine the two tables into one
combined_df <- merge(question_text_df, summary_stats, by.x = "Column_Name", by.y = "Column", all = TRUE)

# Remove the first column, Column_Name, from combined_df
combined_df <- combined_df[, -which(names(combined_df) == "Column_Name")]

# Print the combined table
#print(combined_df)

# Save the combined table in an excel sheet called "Table Test" in the same directory as the original sheet
addWorksheet(wb, "Table Test")
writeData(wb, "Table Test", combined_df)

# Save the workbook with the combined sheet
saveWorkbook(wb, "C:/Users/jtorr/OneDrive/DrPH/Dissertation/Dissertation Raw Materials/Table Test.xlsx", overwrite = TRUE)

# Print the columns that are in the component "Prioritize workplace physical safety"
print(components[["Prioritize workplace physical safety"]])
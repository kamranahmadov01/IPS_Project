# Load the package to read excel files
library(readxl)

# Read the covariates data from covariates.xlsx
covariates <- read_excel("/Users/kamranahmadov/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Prob&Stat/Project/covariates.xlsx")

# Read the biomarker data from biomarkers.xlsx
biomarker_data <- read_excel("/Users/kamranahmadov/Library/CloudStorage/OneDrive-UniversityofEdinburgh/Prob&Stat/Project/biomarkers.xlsx")
# Filter the data for '-0weeks'
biomarker_data <- biomarker_data[grep("-0weeks", biomarker_data$Biomarker), ]

# Extract a new PatiendID column from 'Biomarker' column 
biomarker_data$PatientID <- sub("-0weeks", "\\1", biomarker_data$Biomarker)

# Merge the biomarker data with covariates data based on PatientID
merged_data <- merge(covariates, biomarker_data, by = "PatientID")

# Replace "-" with "_" in all column names
new_colnames <- colnames(merged_data)  # Copy the existing column names
for (i in 1:length(new_colnames)) {
  new_colnames[i] <- gsub("-", "_", new_colnames[i])
}
colnames(merged_data) <- new_colnames# Assign the modified column names back to the data frame

#Fix other column names for the sake of simplicity
colnames(merged_data)[colnames(merged_data) == "Sex (1=male, 2=female)"] <- "Sex"
colnames(merged_data)[colnames(merged_data) == "Smoker (1=yes, 2=no)"] <- "Smoker"


# Separate data into high VAS (>= 5) and low VAS (< 5) groups
high_VAS <- merged_data[merged_data$`VAS_at_inclusion` >= 5, ]
low_VAS <- merged_data[merged_data$`VAS_at_inclusion` < 5, ]

##################################################################################

# Perform the t-test for IL-8 biomarker and print its p-value
cat("p-value for IL-8:", t.test(high_VAS$IL_8, low_VAS$IL_8)$p.value,"\n")

##################################################################################

# t-test for VEGF-A biomarker
cat("p-value for VEGF-A:", t.test(high_VAS$VEGF_A, low_VAS$VEGF_A)$p.value,"\n")

##################################################################################

# t-test for OPG biomarker
cat("p-value for OPG:", t.test(high_VAS$OPG, low_VAS$OPG)$p.value,"\n")

##################################################################################

# t-test for TGF-beta-1 biomarker
cat("p-value for TGF-beta-1:", t.test(high_VAS$TGF_beta_1, low_VAS$TGF_beta_1)$p.value,"\n")

##################################################################################

# t-test for IL-6 biomarker
cat("p-value for IL-6:", t.test(high_VAS$IL_6, low_VAS$IL_6)$p.value,"\n")

##################################################################################

# t-test for CXCL9 biomarker
cat("p-value for CXCL9:", t.test(high_VAS$CXCL9, low_VAS$CXCL9)$p.value,"\n")

##################################################################################

# t-test for CXCL1 biomarker
cat("p-value for CXCL1:", t.test(high_VAS$CXCL1, low_VAS$CXCL1)$p.value,"\n")

##################################################################################

# t-test for IL-18 biomarker
cat("p-value for IL-18:", t.test(high_VAS$IL_18, low_VAS$IL_18)$p.value,"\n")

##################################################################################

# t-test for CSF-1 biomarker
cat("p-value for CSF-1:", t.test(high_VAS$CSF_1, low_VAS$CSF_1)$p.value,"\n")

##################################################################################

# REGRESSION MODELLING

# The are empty cells in 'Vas_12months' column, that's why we get rid of those rows
merged_data <- merged_data[complete.cases(merged_data$Vas_12months), ]

# Data Splitting (80% for training, 20% for testing)
set.seed(123)  # Set a random seed for reproducibility
n <- nrow(merged_data) # Calculate the number of rows in 'merged_data'
train_indices <- sample(1:n, 0.8 * n) # Randomly select 80% of the rows for training
train_data <- merged_data[train_indices, ] # Create the training dataset
test_data <- merged_data[-train_indices, ] # Create the testing dataset

# Model Construction
model <- lm(Vas_12months ~ IL_8 + VEGF_A + OPG + TGF_beta_1 + IL_6 + CXCL9 + CXCL1 + IL_18 + CSF_1 + Age + Sex + Smoker, data = train_data)

# Model Fitting
summary(model)

# Model's prediction evaluation
predicted_values <- predict(model, newdata = test_data) # Predict VAS using the model on the testing data
actual_values <- test_data$Vas_12months # Get the actual 12-month VAS values from the testing data
mae <- mean(abs(predicted_values - actual_values)) # Calculate Mean Absolute Error
rmse <- sqrt(mean((predicted_values - actual_values)^2)) # Calculate Root Mean Square Error

# Print evaluation metrics
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Square Error (RMSE):", rmse, "\n")

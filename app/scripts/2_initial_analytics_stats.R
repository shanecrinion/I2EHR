# Sample & Clinical Data Overview

# Demographics Table

# 1. Number of samples
cat("Samples:", length(unique(clinical_data$patient_id)), "\n")

n_cases <- dim(clinical_data[clinical_data$`group:ch1`=='Matched Ornish Participant',])[1] / 3 # 3 samples per patient
cat("Cases:", n_cases, "\n")

n_controls <- dim(clinical_data[clinical_data$`group:ch1`=='Matched Control Group',])[1] / 3 # 3 samples per patient
cat("Controls:", n_controls, "\n")

# Extract by time point
cat('Samples by time-point:')
table(clinical_data$sample)

rm(n_cases, n_controls)

#2. Sample groups/conditions breakdown
baseline <- clinical_data[clinical_data$`sample`=='baseline',]

# Function for association tests
chi_clinical <- function(x, y, xname, yname){

  # Create association table
  contingency_table <- table(x, y)

  # Perform chi-square test
  chi_result <- chisq.test(contingency_table)

  # Display results
  cat("Chi-Square Test: ", xname, ' vs ', yname, '\n')
  cat("=====================================\n")
  print(head(contingency_table))
  cat("...\n")
  cat("Chi-square statistic:", round(chi_result$statistic, 3), "\n")
  cat("Degrees of freedom:", chi_result$parameter, "\n")
  cat("P-value:", format.pval(chi_result$p.value, digits = 3), "\n\n")

  if(chi_result$p.value < 0.05) {
    cat("Interpretation: Significant association (p < 0.05)\n")
  } else {
    cat("Interpretation: No significant association (p >= 0.05)\n")
  }}

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cat('Age:',
    '\n', '>Range: ', min(clinical_data$`age:ch1`), max(clinical_data$`age:ch1`),
    '\n', '>Median:', median(clinical_data$`age:ch1`),
    '\n', '>Mean:', mean(clinical_data$`age:ch1`),
    '\n', '>Mode:', Mode(round(clinical_data$`age:ch1`)))

chi_clinical(baseline$`age:ch1`, baseline$`diabetes:ch1`, 'Age', 'Diabetes')
chi_clinical(baseline$`age:ch1`, baseline$`cad:ch1`, 'Age', 'CAD')
chi_clinical(baseline$`age:ch1`, baseline$`1 year weight loss (%):ch1`, 'Age', '1 year weight loss (%)')


m_n <- dim(clinical_data[clinical_data$`gender:ch1`=='Male', ])[1] /3
f_n <- dim(clinical_data[clinical_data$`gender:ch1`=='Female', ])[1] /3

cat('Sex:',
    '\n', '>Male: ', m_n,
    '\n', '>Female: ', f_n,
    '\n', '>Ratio (M/F):',  m_n/f_n
    )

rm(m_n, f_n)

cat('Treatment groups:')

d_y <- dim(clinical_data[clinical_data$`diabetes:ch1`=='Yes', ])[1] /3
d_n <- dim(clinical_data[clinical_data$`diabetes:ch1`=='No', ])[1] /3

cat('Diabetes:',
    '\n', '>Yes: ', d_y,
    '\n', '>No: ', d_n,
    '\n', '>Ratio (Y/N):',  d_y/d_n
)

rm(d_y, d_n)

cad_y <- dim(clinical_data[clinical_data$`cad:ch1`=='Yes', ])[1] /3
cad_n <- dim(clinical_data[clinical_data$`cad:ch1`=='No', ])[1] /3

cat('CAD:',
    '\n', '>Yes: ', cad_y,
    '\n', '>No: ', cad_n,
    '\n', '>Ratio (Y/N):',  cad_y/cad_n
)

rm(cad_y, cad_n)

cat('1 year weight loss (%):',
    '\n', '>Range:', min(clinical_data$`1 year weight loss (%):ch1`), max(clinical_data$`1 year weight loss (%):ch1`),
    '\n', '>Median:', median(clinical_data$`1 year weight loss (%):ch1`),
    '\n', '>Mean:', mean(clinical_data$`1 year weight loss (%):ch1`),
    '\n', '>Standard Deviation:', sd(clinical_data$`1 year weight loss (%):ch1`))

# Association tests
# Age
chi_clinical(baseline$`age:ch1`, baseline$`diabetes:ch1`, 'Age', 'Diabetes')
chi_clinical(baseline$`age:ch1`, baseline$`cad:ch1`, 'Age', 'CAD')
chi_clinical(baseline$`age:ch1`, baseline$`1 year weight loss (%):ch1`, 'Age', '1 year weight loss (%)')

# Gender
chi_clinical(baseline$`gender:ch1`, baseline$`diabetes:ch1`, 'Gender', 'Diabetes')
chi_clinical(baseline$`gender:ch1`, baseline$`cad:ch1`, 'Gender', 'CAD')
chi_clinical(baseline$`gender:ch1`, baseline$`1 year weight loss (%):ch1`, 'Gender', '1 year weight loss (%)')

# Missing data summary
# write function to summarise data

generate_missing_summary <- function(clinical_data) {

  # 1. Overall missing statistics
  total_cells <- nrow(clinical_data) * ncol(clinical_data)
  total_missing <- sum(is.na(clinical_data))
  pct_missing_overall <- (total_missing / total_cells) * 100

  complete_cases <- sum(complete.cases(clinical_data))
  pct_complete <- (complete_cases / nrow(clinical_data)) * 100

  # 2. Missing data per variable
  missing_per_var <- data.frame(
    Variable = names(clinical_data),
    Missing_Count = colSums(is.na(clinical_data)),
    Missing_Percent = round((colSums(is.na(clinical_data)) / nrow(clinical_data)) * 100, 2),
    Complete_Count = colSums(!is.na(clinical_data))
  ) %>%
    arrange(desc(Missing_Percent))

  return(missing_per_var)
}

# generate missing report
generate_missing_summary(baseline)

# Clinical Variable Visualizations
#library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape data
summary_data <- clinical_data %>%
  select(`diabetes:ch1`, `gender:ch1`, `cad:ch1`) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  count(Variable, Value) %>%
  group_by(Variable) %>%
  mutate(
    percentage = round(n/sum(n)*100, 1),
    Variable = recode(Variable,
                      "diabetes:ch1" = "Diabetes",
                      "gender:ch1" = "Gender",
                      "cad:ch1" = "CAD")
  ) %>%
  ungroup()

# Define specific colors for each unique value
color_palette <- c(
  "Yes" = "#27ae60",        # Green for No
  "No" = "#e74c3c",       # Red for Yes
  "Female" = "#9b59b6",    # Purple for Female
  "Male" = "#3498db"       # Blue for Male
)

# Categorical data
ggplot(summary_data, aes(x = Variable, y = n, fill = Value)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(aes(label = paste0(n, " (", percentage, "%)")),
            position = position_stack(vjust = 0.5),
            color = "white",
            fontface = "bold",
            size = 4) +
  coord_flip() +
  scale_fill_manual(values = color_palette) +
  labs(title = "Clinical Variables Distribution",
       x = "",
       y = "Number of Samples",
       fill = "Category") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))

#Histograms/density plots for continuous variables (age, survival time)
# Age
ggplot(clinical_data, aes(x = `age:ch1`)) +
  geom_histogram(bins = 10, fill = "#b7d8d6", color = "#4d6466") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: ", age_stats$mean_age, " years\n",
                          "Median: ", age_stats$median_age, " years\n",
                          "SD: ", age_stats$sd_age),
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  labs(title = "Age Distribution",
       x = "Age (years)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Age
# Calculate statistics
age_stats <- clinical_data %>%
  summarise(
    mean_age = round(mean(`age:ch1`, na.rm = TRUE), 1),
    median_age = median(`age:ch1`, na.rm = TRUE),
    sd_age = round(sd(`age:ch1`, na.rm = TRUE), 1),
    min_age = min(`age:ch1`, na.rm = TRUE),
    max_age = max(`age:ch1`, na.rm = TRUE)
  )

# plot
ggplot(clinical_data, aes(x = `age:ch1`)) +
  geom_histogram(bins = 10, fill = "#b7d8d6", color = "#4d6466") +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: ", age_stats$mean_age, " years\n",
                          "Median: ", age_stats$median_age, " years\n",
                          "SD: ", age_stats$sd_age),
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  labs(title = "Age Distribution",
       x = "Age (years)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

### Weight loss
# Calculate statistics
# For continuous weight loss values
weight_stats <- clinical_data %>%
  summarise(
    mean_wl = round(mean(`1 year weight loss (%):ch1`, na.rm = TRUE), 1),
    median_wl = round(median(`1 year weight loss (%):ch1`, na.rm = TRUE), 1),
    sd_wl = round(sd(`1 year weight loss (%):ch1`, na.rm = TRUE), 1)
  )

ggplot(clinical_data, aes(x = `1 year weight loss (%):ch1`)) +
  geom_histogram(bins = 20, fill = "#4d6466", color = "white", alpha = 0.8) +
  geom_vline(xintercept = weight_stats$mean_wl,
             color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = Inf, y = Inf,
           label = paste0("Mean: ", weight_stats$mean_wl, " %\n",
                          "Median: ", weight_stats$median_wl, " %\n",
                          "SD: ", weight_stats$sd_wl),
           hjust = 1.1, vjust = 1.5, size = 4, fontface = "bold") +
  labs(title = "Weight Loss Distribution",
       x = "Weight Loss (%)",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Contingency tables for relationships between clinical factors



Gene Expression Quality Control

Distribution Checks

Boxplots of expression values across all samples (detect outliers)
Density plots showing overall expression distribution
MA plots (log ratio vs average expression)


Sample Correlation Heatmap

Hierarchical clustering of samples based on expression profiles
Helps identify batch effects or sample outliers
Can annotate with clinical variables


Principal Component Analysis (PCA)

2D/3D scatter plots colored by clinical groups
Variance explained by each PC
Helps visualize sample separation and identify confounders



Basic Differential Expression Preview

Top Variable Genes

Heatmap of most variable genes across samples
Helps identify genes driving sample differences


Volcano Plot (if comparing groups)

Log fold change vs. statistical significance
Quick view of differentially expressed genes



Summary Statistics Table

Expression Statistics

Number of genes/probes detected
Mean, median, range of expression values
Percentage of low-expression genes



Technical Implementation Tips

Use plotly for interactive plots
Include DT::datatable() for searchable/sortable tables
Add download buttons for filtered data/plots
Consider heatmaply for interactive heatmaps
Use limma or DESeq2 packages for preprocessing

Would you like code examples for any of these specific analyses in R Shiny?RetryClaude can make mistakes. Please double-check responses.

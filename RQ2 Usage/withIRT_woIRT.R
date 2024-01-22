library(ggplot2)
library(tidyr)
library(lubridate)

# Read the CSV file
data = read.csv("characteristics_repo.csv")

# Take the logarithm, adding 1 to avoid issues with zero values
data$size = log(data$size + 1)
data$contributors_count = log(data$contributors_count + 1)
data$total_issues_count = log(data$total_issues_count + 1)
data$stargazers_count = log(data$stargazers_count + 1)

# Convert date columns to Date class
created_data = as.Date(data$created_at)
current_data = as.Date("2023-12-31")
data$age = as.numeric(current_data - created_data) / 365.25

# Select relevant columns
rq_data = data[,c("size","has_IRT","contributors_count","total_issues_count","stargazers_count","age")]

# Reshape the data for plotting
rq_data = gather(rq_data, Group, Value, contributors_count, stargazers_count, total_issues_count, size, age)

# Customize the names on the X-axis for each independent variable
custom_labels = c("TeamSize", "ProjectStar", "ProjectIssue", "ProjectSize", "ProjectAge")

# Convert the "Group" column to a factor with custom labels
rq_data$Group = factor(rq_data$Group, levels = unique(rq_data$Group), labels = custom_labels)

# Create a boxplot
ggplot(rq_data, aes(x = Group, y = Value, fill = has_IRT)) +
  geom_boxplot(na.rm = TRUE, outlier.shape = NA) +
  labs(x = "", y = "Number of project characteristics", fill = "") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(color = "black", fill = NA))  # Add a border around the plot

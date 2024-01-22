library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)

data = read.csv("merged_data.csv")

data$size = log(data$size + 1)
data$contributors_count = log(data$contributors_count + 1)
data$total_issues_count = log(data$total_issues_count + 1)
data$stargazers_count = log(data$stargazers_count + 1)

created_data = as.Date(data$created_at)
current_data = as.Date("2023-12-31")
data$age = as.numeric(current_data - created_data) / 365.25

rq_data = data[, c("size", "Count", "contributors_count", "total_issues_count", "stargazers_count", "age")]
rq_data = gather(rq_data, Group, Value, contributors_count, stargazers_count, total_issues_count, size, age)

custom_labels = c("TeamSize", "ProjectStar", "ProjectIssue", "ProjectSize", "ProjectAge")

rq_data$Group = factor(rq_data$Group, levels = unique(rq_data$Group), labels = custom_labels)

rq_data$fenzu = ifelse(rq_data$Count < 2, "lessIRT",
                          ifelse(rq_data$Count > 3, "moreIRT", NA))

rq_data$fenzu = factor(rq_data$fenzu, levels = c("lessIRT", "moreIRT"))

# Calculate medians for each variable with and without considering "fenzu" groups
medians = rq_data %>%
  group_by(Group, fenzu) %>%
  summarize(
    median_Value = median(Value, na.rm = TRUE)
  )

medians1 = rq_data %>%
  group_by(Group) %>%
  summarize(
    median_Value = median(Value, na.rm = TRUE)
  )

# Manually add a dummy column for fenzu in medians1
medians1$fenzu = NA

# Plot
ggplot(rq_data, aes(x = Group, y = Value, fill = fenzu)) +
  geom_split_violin(na.rm = TRUE, trim = FALSE) +
  geom_crossbar(data = medians, aes(x = Group, y = median_Value, ymin = median_Value, ymax = median_Value, group = fenzu),
                position = position_dodge(width = 0.8), width = 0.5, fatten = 2, size = 0.3) +
  geom_crossbar(data = medians1, aes(x = Group, y = median_Value, ymin = median_Value, ymax = median_Value),
                position = position_dodge(width = 0.8), width = 0.5, fatten = 2, size = 0.3, color = "black", linetype = "dashed") +
  scale_fill_manual(values = c("#5C97BF", "#E4F1F4")) +
  labs(x = "", y = "Number of project characteristics", fill = "") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(panel.border = element_rect(color = "black", fill = NA))

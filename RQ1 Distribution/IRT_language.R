library(ggplot2)
library(plyr)

# Read the filtered CSV file
df_filtered = read.csv("top19language.csv")

# Use ddply to calculate the total count for each main_language
count_data = ddply(df_filtered, .(main_language), summarize, count = length(category))

# Order the data frame in descending order based on the total count in the count column
df_filtered = merge(df_filtered, count_data, by = "main_language")
df_filtered = df_filtered[order(-df_filtered$count), ]

# Set up color mapping
colors = c("0" = "#C77CFF", "1" = "#F8766D", "2" = "#7CAE00", "3" = "#00BFC4", "4" = "#ACE1EC")

# Create a stacked bar plot
ggplot(df_filtered, aes(x = reorder(main_language, -count), fill = reorder(category, -count, FUN = function(x) -sum(x)))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = colors) +
  labs(
    x = "Programming Languages",
    y = "Number of IRTs",
    fill = "Category") +
  theme_minimal() +
  theme(panel.border = element_rect(color = "black", fill = NA)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

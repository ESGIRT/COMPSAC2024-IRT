library(dplyr)
library(ggplot2)

# Read the data
yanhua_data = read.csv("withIRT_classify_with_lines.csv")

# Convert the date to POSIXct format
yanhua_data$irt_use_time = as.POSIXct(yanhua_data$irt_use_time, format = "%Y-%m-%d %H:%M:%S")

# Group by year and calculate the number of new BugReports each year
IRT_result = yanhua_data %>%
  mutate(year = lubridate::year(irt_use_time)) %>%
  group_by(year, category) %>%
  summarise(IRT_num = n()) %>%
  filter(category %in% c('1', '2', '3', '4', '0')) %>%
  arrange(year, category)

# Define color mapping for saturated colors
category_colors_saturated = c("1" = "#F8766D", "2" = "#7CAE00", "3" = "#00BFC4", "4" = "#ACE1EC", "0" = "#C77CFF")

# Plot the bar chart
ggplot(IRT_result, aes(x = factor(year), y = IRT_num, fill = factor(category))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = category_colors_saturated) +
  labs(x = "Year", y = "Number of each type of IRT", fill = "Category") +
  theme_minimal() +
  scale_y_continuous(breaks = c(0, 3000, 6000, 9000, 12000)) +
  theme(panel.border = element_rect(color = "black", fill = NA))

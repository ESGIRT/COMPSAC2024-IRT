library(lmerTest)
library(MuMIn)

# Read the data
data = read.csv("AM_F.csv")

# Convert date columns to Date class
created_data = as.Date(data$created_at)
current_data = as.Date("2023-12-31")

#---Project dimensions---
ProjectAge = scale(as.numeric(difftime(current_data, created_data, units = "days")) / 365.25)
ProjectSize = scale(data$size)
TeamSize = scale(data$contributors_count)
ProjectIssue = scale(data$total_issues_count)
ProjectStar = scale(data$stargazers_count)

#---IRT dimensions---
n_IRT_impact = scale(data$irt_impact)
n_IRT_counts = scale(data$IRT_counts)
n_IRT_lines = scale(data$IRT_lines)
n_time_interval = scale(data$time_interval)

# Take the logarithm, adding 0.5 to avoid forks with zero values
forks = scale(log(data$forks_count + 0.5))

data$group = factor(data$main_language)

# Fit the linear mixed-effects model
success_AM = lmer(
  formula = forks ~ ProjectAge +
    ProjectSize +
    TeamSize +
    ProjectIssue +
    ProjectStar +
    n_IRT_impact +
    n_IRT_counts +
    n_IRT_lines +
    n_time_interval +
    (1 | group),
  data = data, REML = FALSE
)

# Summarize the model
success_AM_summary = summary(success_AM)
success_AM_anova = anova(success_AM)
success_AM_GLMM = r.squaredGLMM(success_AM)

# Print the summaries
print(success_AM_summary)
print(success_AM_anova)
print(success_AM_GLMM)

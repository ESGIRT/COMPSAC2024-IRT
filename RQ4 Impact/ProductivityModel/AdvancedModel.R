library(lmerTest)
library(MuMIn)

# Read the data
data = read.csv("AM_C.csv")

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

commits = scale(log(data$commits_count))

data$group = factor(data$main_language)

# Fit the linear mixed-effects model
productivity_AM = lmer(
  formula = commits ~ ProjectAge +
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
productivity_AM_summary = summary(productivity_AM)
productivity_AM_anova = anova(productivity_AM)
productivity_AM_GLMM = r.squaredGLMM(productivity_AM)

# Print the summaries
print(productivity_AM_summary)
print(productivity_AM_anova)
print(productivity_AM_GLMM)

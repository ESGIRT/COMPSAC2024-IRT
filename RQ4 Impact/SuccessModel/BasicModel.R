library(lmerTest)
library(MuMIn)

# Read the data
data = read.csv("characteristics_repo.csv")

# Convert date columns to Date class
created_data = as.Date(data$created_at)
current_data = as.Date("2023-12-31")

#---Project dimensions---
ProjectAge = scale(as.numeric(difftime(current_data, created_data, units = "days")) / 365.25)
ProjectSize = scale(data$size)
TeamSize = scale(data$contributors_count)
ProjectIssue = scale(data$total_issues_count)
ProjectStar = scale(data$stargazers_count)

# Define a factor for whether IRT is used or not
useIRT = factor(data$has_IRT)

# Take the logarithm, adding 0.5 to avoid forks with zero values
forks = scale(log(data$forks_count + 0.5))

data$group = factor(data$main_language)

# Fit the linear mixed-effects model
success_BM = lmer(
  formula = forks ~ ProjectAge +
    ProjectSize +
    TeamSize +
    ProjectIssue +
    ProjectStar +
    useIRT +
    (1 | group),
  data = data, REML = FALSE
)

# Summarize the model
success_BM_summary = summary(success_BM)
success_BM_anova = anova(success_BM)
success_BM_GLMM = r.squaredGLMM(success_BM)

# Print the summaries
print(success_BM_summary)
print(success_BM_anova)
print(success_BM_GLMM)

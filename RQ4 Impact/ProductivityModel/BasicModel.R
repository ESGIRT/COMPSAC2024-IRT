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

# Log-transform commits
commits = scale(log(data$commits_count))

data$group = factor(data$main_language)

# Fit the linear mixed-effects model
productivity_BM = lmer(
  formula = commits ~ ProjectAge +
    ProjectSize +
    TeamSize +
    ProjectIssue +
    ProjectStar +
    useIRT +
    (1 | group),
  data = data, REML = FALSE
)

# Summarize the model
productivity_BM_summary = summary(productivity_BM)
productivity_BM_anova = anova(productivity_BM)
productivity_BM_GLMM = r.squaredGLMM(productivity_BM)

# Print the summaries
print(productivity_BM_summary)
print(productivity_BM_anova)
print(productivity_BM_GLMM)

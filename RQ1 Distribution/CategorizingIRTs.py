import pandas as pd
import re

# Read the original CSV file
df = pd.read_csv('withIRT.csv')

def classify_template(template):
    # Define regular expressions for each category
    bug_pattern = re.compile(r'(bug|issue|error)', re.IGNORECASE)
    feature_pattern = re.compile(r'(feature|enhancement)', re.IGNORECASE)
    documentation_pattern = re.compile(r'(doc|documentation)', re.IGNORECASE)
    question_pattern = re.compile(r'(question|support|help)', re.IGNORECASE)

    # Use regular expressions to search for keywords
    if bug_pattern.search(template):
        return 1
    elif feature_pattern.search(template):
        return 2
    elif documentation_pattern.search(template):
        return 3
    elif question_pattern.search(template):
        return 4
    else:
        return 0

# Add the classification results to a new column "Category"
df['category'] = df['IRT_name'].apply(classify_template)

# Get the count of each category
category_counts = df['category'].value_counts()

print(category_counts)

# Data from https://archive.ics.uci.edu/ml/machine-learning-databases/00350/
# Minimally cleaned by hand (changed encoding, saved as cc_def.csv)

# Metadata from source:s
# This research employed a binary variable, default payment (Yes = 1, No = 0), as the response variable. This study reviewed the literature and used the following 23 variables as explanatory variables: 
# X1: Amount of the given credit (NT dollar): it includes both the individual consumer credit and his/her family (supplementary) credit
# X2: Gender (1 = male; 2 = female)
# X3: Education (1 = graduate school; 2 = university; 3 = high school; 4 = others) 
# X4: Marital status (1 = married; 2 = single; 3 = others)
# X5: Age (year). 

# X6 - X11: History of past payment.
    # The measurement scale for the repayment status is: 
    #     -1 = pay duly
    #     1 = payment delay for one month
    #     2 = payment delay for two months
    #     . . .; 
    #     8 = payment delay for eight months
    #     9 = payment delay for nine months and above
#   X6: the repayment status in September, 2005
#   X7: the repayment status in August, 2005
#   X8: the repayment status in July, 2005
#   X9: the repayment status in June, 2005
#   X10: the repayment status in May, 2005
#   X11: the repayment status in April, 2005

# X12-X17: Amount of bill statement (NT dollar)
#    X12 = amount of bill statement in September, 2005
#    X13 = amount of bill statement in August, 2005
#    X14 = amount of bill statement in July, 2005
#    X15 = amount of bill statement in June, 2005
#    X16 = amount of bill statement in May, 2005
#    X17 = amount of bill statement in April, 2005

# X18-X23: Amount of previous payment (NT dollar)
# X18 = amount paid in September, 2005
# X19 = amount paid in August, 2005
# X20 = amount paid in July, 2005
# X21 = amount paid in June, 2005
# X22 = amount paid in May, 2005
# X23 = amount paid in April, 2005

a <- read.csv("cc_def.csv", stringsAsFactors = F)





























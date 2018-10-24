# Environemnt Views ---------------------------------------

# This is not actually a directly runnable script .
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")


# Company Environment ---------------------------------------

#Prequisties for this section
predictors <- salaryVsIndentation_predictors(c(companySizePrefix, companyTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)

stats <- df %>% salaryVsIndentation_group_summary("Company.Size")
gridExtra::grid.arrange(
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", "Company.Size", "Actual preference vs standard salary by company size"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Company.Size", "Predicted preference vs predicted salary by company size"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Company.Size", "Residual preference vs residual preference by company size"),
    nrow = 2
)

stats <- df %>% salaryVsIndentation_group_summary("Company.Type")
gridExtra::grid.arrange(
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", "Company.Type", "Standard salary vs actual preference by company type"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary", "Standard Salary", "Company.Type", "Standard salary vs predicted preference by company type"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary", "Standard Salary", "Company.Type", "Standard salary vs preference residual by company type"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Company.Type", "Residual salary vs preference residual by company typee")

)




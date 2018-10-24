# Tech Analysis Views ---------------------------------------

# This is not actually a directly runnable script .
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")

# Tech Prediction Views ---------------------------------------

noTechPredictors <- salaryVsIndentation_predictors(c(languagePrefix, frameworkPrefix, databasePrefix, platformPrefix, idePrefix, developerTypePrefix))
noTechDf <- survey_assemble() %>% salaryVsIndentation_augment(noTechPredictors)
noTechTagstats <- noTechDf %>% salaryVsIndentation_tags_summary()

# Programming language ---------------------------------------
predictors <- salaryVsIndentation_predictors(c(languagePrefix, developerTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
tagstats <- df %>% salaryVsIndentation_tags_summary()
stats <- tagstats %>% filter(Category == "Language", Count > 500)

gridExtra::grid.arrange(
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", "Name", "Actual preference vs standard salary by language"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Predicted preference vs predicted salary by language"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Residual preference vs residual salary by language"),
    ncol = 2
)

# IDE
predictors <- salaryVsIndentation_predictors(c(idePrefix, developerTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
tagstats <- df %>% salaryVsIndentation_tags_summary()

stats <- tagstats %>% filter(Category == "IDE", Count > 500, Name != "Emacs")
noTechStats <- noTechTagstats %>% filter(Category == "IDE", Count > 500, Name != "Emacs")

gridExtra::grid.arrange(
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", "Name", "Actual preference vs standard salary by language"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Predicted preference vs predicted salary by language"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Residual preference vs residual salary by language"),
    ncol = 2
)

gridExtra::grid.arrange(
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "No tech predicted preference by IDE"),
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "No tech residual preference by IDE"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Tech predicted preference by IDE"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by IDE"),
    nrow = 2
)

# Platform

predictors <- salaryVsIndentation_predictors(c(platformPrefix, developerTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
tagstats <- df %>% salaryVsIndentation_tags_summary()

stats <- tagstats %>% filter(Category == "Platform", Count > 500)
noTechStats <- noTechTagstats %>% filter(Category == "Platform", Count > 500)

gridExtra::grid.arrange(
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "No tech predicted preference by Platform"),
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "No tech residual preference by Platform"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Tech predicted preference by Platform"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Platform"),
    nrow = 2
)

# Database

predictors <- salaryVsIndentation_predictors(c(databasePrefix, developerTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
tagstats <- df %>% salaryVsIndentation_tags_summary()

stats <- tagstats %>% filter(Category == "DB", Count > 500)
noTechStats <- noTechTagstats %>% filter(Category == "DB", Count > 500)

gridExtra::grid.arrange(
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "No tech predicted preference by Database"),
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "No tech residual preference by Platform"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Tech predicted preference by Platform"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Platform"),
    nrow = 2
)

# Framework

predictors <- salaryVsIndentation_predictors(c(frameworkPrefix, developerTypePrefix))
df <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
tagstats <- df %>% salaryVsIndentation_tags_summary()

stats <- tagstats %>% filter(Category == "Framework", Count > 500)
noTechStats <- noTechTagstats %>% filter(Category == "Framework", Count > 500)

gridExtra::grid.arrange(
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "No tech predicted preference by Framework"),
    noTechStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "No tech residual preference by Framework"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space odds predicted", "Standard.Salary.Predicted", "Predicted Salary", "Name", "Tech predicted preference by Framework"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Framework"),
    nrow = 2
)

# Tech Side by side residual ---------------------------------------

# Which tech thingy is the most important
# Important residuals side-by-side

# This is going to take a bit of crunching!

noTechPredictors <- salaryVsIndentation_predictors(c(languagePrefix, frameworkPrefix, databasePrefix, platformPrefix, idePrefix, developerTypePrefix))
noTechDf <- survey_assemble() %>% salaryVsIndentation_augment(noTechPredictors)
noTechTagstats <- noTechDf %>% salaryVsIndentation_tags_summary()

techStats <- function(predictors, category) {
    techDf <- survey_assemble() %>% salaryVsIndentation_augment(predictors)
    techTagStats <- techDf %>% salaryVsIndentation_tags_summary()
    techTagStats %>% filter(Category == category, Count > 500)
}

#Language
languagePredictors <- salaryVsIndentation_predictors(c(languagePrefix, developerTypePrefix))
languageStats <- techStats(languagePredictors, "Language")

# IDE
idePredictors <- salaryVsIndentation_predictors(c(idePrefix, developerTypePrefix))
ideStats <- techStats(idePredictors, "IDE")

# Platform
platformPredictors <- salaryVsIndentation_predictors(c(platformPrefix, developerTypePrefix))
platformStats <- techStats(platformPredictors, "Platform")

# Database
databasePredictors <- salaryVsIndentation_predictors(c(databasePrefix, developerTypePrefix))
databaseStats <- techStats(databasePredictors, "DB")

gridExtra::grid.arrange(
    languageStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Language"),
    ideStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by IDE"),
    platformStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Platform"),
    databaseStats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Residual Salary", "Name", "Tech residual by Database"),
    nrow = 2
)

library(caret)
library(recipes)

survey_dmatrix <- function(survey_df, response) {
    tag_starts <- tag_prefixes %>% map(~ quo(starts_with(.)))
    x_data <- survey_df %>% select(Experience, !!! tag_starts)
    x <- data.matrix(x_data)
    y <- survey_df[[response]]
    dmatrix <- list(x = x, y = y, response = response, predictors = names(x_data))
    class(dmatrix) <- c("dmatrix", class(dmatrix))
    dmatrix
}

augment_salary <- function(survey_df, alpha = 1, s = "lambda.1se", seed = 7) {
    salaryDm <- survey_dmatrix(survey_df, "Standard.Salary")
    set.seed(seed)
    salaryFit <- glmnet::cv.glmnet(salaryDm$x, salaryDm$y, alpha = alpha)
    salaries <- survey_df$Standard.Salary
    salaryPredictions <- predict(salaryFit, newx = salaryDm$x, s = s)
    salaryPredictions <- c(salaryPredictions)
    salaryResiduals <- salaries - salaryPredictions
    
    augmented_df <- survey_df
    
    augmented_df$Standard.Salary.Predicted <- salaryPredictions
    augmented_df$Standard.Salary.Residual <- salaryResiduals
    augmented_df
    
}

augment_space_preference <- function(survey_df, alpha = 1, s = "lambda.1se", seed = 7) {
    preferSpaceDm <- survey_dmatrix(survey_df, "Prefer.Space")
    preferSpaceFit <- glmnet::cv.glmnet(preferSpaceDm$x, preferSpaceDm$y, alpha = alpha, family = "binomial")
    preferSpaceLogOdds <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = s, type = "link")
    preferSpaceLogOdds <- c(preferSpaceLogOdds)
    preferSpacePredicted <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = s, type = "class")
    preferSpacePredicted <- as.integer(preferSpacePredicted)
    
    augmented_df <- survey_df
    
    augmented_df$Space.Preference.Logodds <- preferSpaceLogOdds
    augmented_df$Space.Preference.Odds <- exp(preferSpaceLogOdds)
    augmented_df$Space.Preference.Predicted <- preferSpacePredicted
    augmented_df$Whitespace.Predicted <- factor(ifelse(preferSpacePredicted == 1, "Spaces", "Tabs"), levels = c("Tabs", "Spaces"))
    augmented_df$Whitespace.Confusion <- if_else(augmented_df$Space.Preference.Predicted != augmented_df$Prefer.Space, as.character(augmented_df$Whitespace), "Expected")
    levels(augmented_df$Whitespace.Confusion) <- c("Tabs", "Spaces", "Expected")
    
    augmented_df
}

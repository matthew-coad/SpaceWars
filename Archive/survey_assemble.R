
# Salary Vs Indentation Modelling --------------------------------

# Select survey predictors
survey_predictors <- function(includePrefix = NULL, discardPrefix = NULL) {
    predictors <- c("Experience", "Employment.Status", "Hours.Per.Week", "University", "Home.Remote", "Check.In.Code", "Highest.Education.Parents", names(survey_tagColumns()))
    if (!is.null(includePrefix)) {
        indexes <- includePrefix %>% map(~stri_startswith_fixed(predictors, .)) %>% transpose() %>% map(~any(flatten_lgl(.))) %>% flatten_lgl()
        predictors <- predictors[indexes]
    }

    if (!is.null(discardPrefix)) {
        indexes <- discardPrefix %>% map(~stri_startswith_fixed(predictors, .)) %>% transpose() %>% map(~!any(flatten_lgl(.))) %>% flatten_lgl()
        predictors <- predictors[indexes]
    }
    predictors
}

# salaryNoEffect_predictors <- c("SelfTaughtType_", "Home.Remote", "Highest.Education.Parents", "FormalEducation_")

salaryNoEffect_predictors <- c("SelfTaughtType_", "Home.Remote", "FormalEducation_") ## Leave highest education parents in so we can demonstate what ineffective variables look like
salaryAccountedFor_predictors <- c("Country_")

salary_predictors <- function(discardPrefix = NULL) survey_predictors(discardPrefix = c(discardPrefix, salaryNoEffect_predictors, salaryAccountedFor_predictors))

# indentationNoEffect_predictors <- c("University", "Home.Remote", "Employment.Status", "Highest.Education.Parents", "FormalEducation_")
indentationNoEffect_predictors <- c("University", "Home.Remote", "Employment.Status", "FormalEducation_") ## Leave highest education parents in so we can demonstate what ineffective variables look like
indentation_predictors <- function(discardPrefix = NULL) survey_predictors(discardPrefix = c(discardPrefix, indentationNoEffect_predictors))

variables_formula <- function(response, predictors) eval(parse(text = sprintf("%s ~ %s", response, paste(predictors, collapse = "+"))))

# Don't really know how to beat caret and broom into submission in regards to glmnet at this time.
# Just do it all direct. Is not so hard.

# Set up the inputs needed for the glmnet algorithms.
glmnet_dmatrix <- function(df, response, predictors) {
    formula <- variables_formula(response, predictors)
    x <- data.matrix(df[, predictors])
    y <- df[[response]]
    dmatrix <- list(x = x, y = y, response = response, predictors = predictors)
    class(dmatrix) <- c("dmatrix", class(dmatrix))
    dmatrix
}

# Augment the survey data with predictions regarding the standard salary and indentation preferences using glmnet
# Set the alpha parameter to switch bewtween Ridge and Lasso
# Set the s parameter to change the lamda selection
# Seed sets the random number seed input 
salaryVsIndentation_augment_glmnet <- function(df, model, salaryPredictors, indentationPredictors, alpha, s, seed) {

    salaryDm <- glmnet_dmatrix(df, "Standard.Salary", salaryPredictors)
    set.seed(seed)
    salaryFit <- glmnet::cv.glmnet(salaryDm$x, salaryDm$y, alpha = alpha)
    salaries <- df$Standard.Salary
    salaryPredictions <- predict(salaryFit, newx = salaryDm$x, s = s)
    salaryPredictions <- c(salaryPredictions)
    salaryResiduals <- salaries - salaryPredictions

    df$Standard.Salary.Predicted <- salaryPredictions
    df$Standard.Salary.Residual <- salaryResiduals

    preferSpaceDm <- glmnet_dmatrix(df, "Prefer.Space", indentationPredictors)
    preferSpaceFit <- glmnet::cv.glmnet(preferSpaceDm$x, preferSpaceDm$y, alpha = alpha, family = "binomial")
    preferSpaceLogOdds <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = s, type = "link")
    preferSpaceLogOdds <- c(preferSpaceLogOdds)
    preferSpacePredicted <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = s, type = "class")
    preferSpacePredicted <- c(preferSpacePredicted)

    df$Space.Preference.Logodds <- preferSpaceLogOdds
    df$Space.Preference.Odds <- exp(preferSpaceLogOdds)
    df$Space.Preference.Predicted <- preferSpacePredicted
    df$Whitespace.Confusion <- if_else(df$Space.Preference.Predicted != df$Prefer.Space, as.character(df$Whitespace), "Expected")
    levels(df$Whitespace.Confusion) <- c("Tabs", "Spaces", "Expected")

    df$Model <- model

    df
}

# What my official salaryVsIndentation augment function is right now
salaryVsIndentation_augment <- function(df, model = "Anonymous", salaryPredictors = salary_predictors(), indentationPredictors = indentation_predictors()) {
    salaryVsIndentation_augment_glmnet(df, model, salaryPredictors, indentationPredictors, 1, "lambda.1se", 7)
    
}

# Salary Vs Indentation Summaries--------------------------------

# Salary vs indentation summary by a grouping variable
salaryVsIndentation_group_summary <- function(df, groupVariable) {

    allSpacePreference <- mean(df$Prefer.Space)
    allSpaceOdds <- allSpacePreference / (1 - allSpacePreference)
    allSpaceLogodds <- log(allSpaceOdds)

    stats <- df %>%
            group_by_(groupVariable, "Model") %>%
            summarise(
                Count = n(),
                Salary = mean(Salary),
                Experience = mean(Experience),
                Standard.Salary = mean(Standard.Salary),
                Standard.Salary.Predicted = mean(Standard.Salary.Predicted),
                Standard.Salary.Residual = mean(Standard.Salary.Residual),
                Space.Preference = mean(Prefer.Space),
                Space.Odds = Space.Preference / (1 - Space.Preference),
                Space.Logodds = log(Space.Odds),
                Space.Logodds.Predicted = mean(Space.Preference.Logodds),
                Space.Logodds.Residual = Space.Logodds - Space.Logodds.Predicted,
                Space.Odds.Predicted = exp(Space.Logodds.Predicted),
                Space.Odds.Residual = exp(Space.Logodds.Residual)
            )
    stats
}

# Salary vs indentation summary for a specific "Tag variable".
# A tag being something like "Language_Java" which indicates that the responent has experience with Java

# Use with augmented data frames
salaryVsIndentation_tag_summary <- function(df, model, tagVariable) {

    td <- df %>% filter_(tagVariable)
    td <- td %>% filter(Model == model)

    Count <- nrow(td)
    stats <- tibble(Count)
    stats$Salary = mean(td$Salary)
    stats$Standard.Salary = mean(td$Standard.Salary)
    stats$Standard.Salary.Predicted = mean(td$Standard.Salary.Predicted)
    stats$Standard.Salary.Residual = mean(td$Standard.Salary.Residual)
    stats$Space.Preference = mean(td$Prefer.Space)
    stats$Space.Odds = stats$Space.Preference / (1 - stats$Space.Preference)
    stats$Space.Logodds = log(stats$Space.Odds)
    stats$Space.Logodds.Predicted = mean(td$Space.Preference.Logodds)
    stats$Space.Logodds.Residual = stats$Space.Logodds - stats$Space.Logodds.Predicted
    stats$Space.Odds.Predicted = exp(stats$Space.Logodds.Predicted)
    stats$Space.Odds.Residual = exp(stats$Space.Logodds.Residual)

    #familyTable <- table(td$Tech.Family)
    #techFamily = levels(techFamily)[which(familyTable == max(familyTable))[1]]
    parts <- stri_split_fixed(str = tagVariable, pattern = "_")[[1]]
    category <- parts[1]
    name <- parts[2]
    stats$Tag <- tagVariable
    stats$Name <- name
    stats$Category <- category
    stats$Model <- model
    #stats$Tech.Family <- techFamily
    stats
}

# Salary vs indentation summary for all identified tags
salaryVsIndentation_tags_summary <- function(df) {

    allSpacePreference <- mean(df$Prefer.Space)
    allSpaceOdds <- allSpacePreference / (1 - allSpacePreference)
    allSpaceLogodds <- log(allSpaceOdds)

    models <- unique(df$Model)
    tagVariables <- survey_tagColumns()
    stats <- map2(rep(models, each = length(tagVariables)), rep(tagVariables, length(models)), ~ salaryVsIndentation_tag_summary(dfc, .x, .y)) %>% bind_rows()
    stats <- stats %>% select(Model, Tag, Name, Category, everything())
}




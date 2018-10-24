library(tidyverse)
library(forcats)
library(broom)
library(stringi)

autoload <- FALSE

if (autoload) {
    source("survey_assemble.R")
}

source("survey_analysis.R")

#Country Reports --------------------------------

# company. An investigation ight reveal cultural preference.

# Profressional vs Amatuer vs part time analysis.

# Working efficiency vs sharing

# Company size.

# Tools

## Experience vs space preference

if (FALSE) {


    ## Some countries have skewed salary distributions.
    ## Some stat transform is needed I'm guessing. Leave for another day!
}

# Country Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Country")

    # Original vs predicted
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Salary", "Salary", "Country", "Salary vs Whitespace by country"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Standard Salary", "Country", "Predicted salary vs Whitespace by country"),
        nrow = 2
    )

    # Q. Is their a relationship between country and salary and space preference.
    stats %>% continuousVsContinuousByGroup_plot("Space.Preference", "Space preference", "Salary", "Salary", "Country", "Salary vs Whitespace by Country")
    # A. Yes clearly. Add to analysis


    # Q. Is their a relationship between country, salary and experience
    stats %>% continuousVsContinuousByGroup_plot("Experience", "Experience", "Salary", "Salary", "Country", "Salary vs Experience by Country")

    # Plots to show salary distribution
    df %>% continuousGroup_histograms("Salary", "Salary", "Country")
    df %>% continuousGroup_histograms("Standard.Salary", "Standard Salary", "Country")
    df %>% continuousGroup_histograms("Standard.Salary.Residual", "Residual Standard Salary", "Country")

    # Some diagnostics/development plots

    # Expect salariesa to be flat
    stats %>% continuousVsContinuousByGroup_plot("Space.Preference", "Space preference", "Standard.Salary", "Standard Salary", "Country", "Standard Salary vs Whitespace by Country")
}

# Experience Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Years.Program")

    # Original vs predicted
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Years.Program", "Salary vs Whitespace by programming experience"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Standard Salary", "Years.Program", "Standard Salary vs Predicted whitespace by programming experience"),
        nrow = 2
    )

    # Q. Is it still their independant of the country?
    stats %>% continuousVsContinuousByGroup_plot("Space.Logodds.Predicted", "Space preference predicted", "Standard.Salary.Residual", "Standard Salary Residual", "Years.Program")
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Space preference predicted", "Standard.Salary", "Standard Salary", "Years.Program")
    # A. Yes. Add it to the model

    ## Diagnostics/Develeopment
    # Straight vs predicted vs residual

    # When we first started our analysis vs entire model
    gridExtra::grid.arrange(
        survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble("Years.Program", experience_predictors) %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Standard Salary Residual", "Years.Program", "Residual odds for initial tech analysis model"),
        survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble("Years.Program", model_predictors("Years.Program")) %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Standard Salary Residual", "Years.Program", "Residual odds for final model")
    )
    #Hard to see but their are small differences. Experience is far more important

    # Their is non-linearity. Need to change the code to account and some reporting.
    
}

# Tech Analysis --------------------------------
if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Tech.Family")
    tagstats <- df %>% salaryVsPreferenceByTag_assemble()

    # Original vs predicted
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Tech.Family", "Salary vs Whitespace by tech family"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary", "Standard Salary", "Tech.Family", "Standard Salary vs Predicted whitespace tech family"),
        nrow = 2
    )

    #Languages
    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Language", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace by language"),
        tagstats %>% filter(Category == "Language", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Predicted Salary vs Predicted Whitespace by language"),
        nrow = 2
    )
    # Big salary difference
    # Big whitespace difference

    #Framework
    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Framework", Count > 500) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "Framework", Count > 500) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )
    # Medium salary difference
    # Medium whitespace difference

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "DB", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "DB", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "IDE", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "IDE", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Platform", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "Platform", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    #Low salary difference

    #Space.Preference = mean(Prefer.Space),
    #        Space.Odds = Space.Preference / (1 - Space.Preference),
    #        Space.Logodds = log(Space.Odds),
    #        Space.Logodds.Predicted = mean(Space.Preference.Logodds),
    #        Space.Logodds.Residual = Space.Logodds - Space.Logodds.Predicted - allSpaceLogodds,
    #        Space.Odds.Residual = exp(Space.Logodds.Residual)

}

# Gender Analysis --------------------------------
if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Tech.Family")
    tagstats <- df %>% salaryVsPreferenceByTag_assemble()

    # Original vs predicted
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Tech.Family", "Salary vs Whitespace by tech family"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary", "Standard Salary", "Tech.Family", "Standard Salary vs Predicted whitespace tech family"),
        nrow = 2
    )

    #Languages
    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Gender") %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace by gender"),
        tagstats %>% filter(Category == "Gender") %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Predicted Salary vs Predicted Whitespace by language"),
        nrow = 2
    )
    # Big salary difference
    # Big whitespace difference

    #Framework
    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Framework", Count > 500) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "Framework", Count > 500) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )
    # Medium salary difference
    # Medium whitespace difference

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "DB", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "DB", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "IDE", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "IDE", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    gridExtra::grid.arrange(
        tagstats %>% filter(Category == "Platform", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space Odds", "Standard.Salary", "Salary", "Name", "Salary vs Whitespace"),
        tagstats %>% filter(Category == "Platform", Count > 200) %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Salary", "Name", "Salary vs Prediced Whitespace"),
        nrow = 2
    )

    #Low salary difference

    #Space.Preference = mean(Prefer.Space),
    #        Space.Odds = Space.Preference / (1 - Space.Preference),
    #        Space.Logodds = log(Space.Odds),
    #        Space.Logodds.Predicted = mean(Space.Preference.Logodds),
    #        Space.Logodds.Residual = Space.Logodds - Space.Logodds.Predicted - allSpaceLogodds,
    #        Space.Odds.Residual = exp(Space.Logodds.Residual)

}


# Company Size Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Company.Size")

    # Original vs predicted
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Company.Size", "Salary vs Whitespace by Company Size"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary", "Standard Salary", "Company.Size", "Standard Salary vs Predicted whitespace by Company Size"),
        nrow = 2
    )
}

# Company Type Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Company.Type")

    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Company.Type", "Salary vs Whitespace by Company Type"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary", "Standard Salary", "Company.Type", "Standard Salary vs Predicted whitespace by Company Type"),
        nrow = 2
    )

}

# Formal education Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble() %>% salaryVsPreferencePredict_assemble()
    stats <- df %>% salaryVsPreferenceByGroup_assemble("Formal.Education")

    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space preference", "Standard.Salary", "Salary", "Company.Type", "Salary vs Whitespace by Company Type"),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary", "Standard Salary", "Company.Type", "Standard Salary vs Predicted whitespace by Company Type"),
        nrow = 2
    )

    # Q. Is their a relationship between group, salary and space preference.
    survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble("Formal.Education", formalEducation_predictors) %>% continuousVsContinuousByGroup_plot("Space.Preference", "Space preference", "Salary", "Salary", "Formal.Education", "Salary vs Whitespace by Formal Education")
    # A. Yes. But little

    # Q. Is it still their independant of the rest of the model?
    survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble("Formal.Education", model_predictors("Formal.Education")) %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Standard Salary Residual", "Formal.Education")
    # A. Yes. More than tech. Add to model


}

# Race analysis Analysis --------------------------------

if (FALSE) {

    group <- "Race"
    predictors <- model_predictors(group)
    survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble(group, model_predictors(group)) %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Standard Salary Residual", group)
    # Very few non anglo responants.
    # Actually I think its a tag list

}

# MajorUndergrad Analysis --------------------------------

if (FALSE) {

    group <- "Major.Undergrad"
    predictors <- model_predictors(group)
    survey_assemble() %>% salaryVsPreferencePredictByGroup_assemble(group, model_predictors(group)) %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Standard.Salary.Residual", "Standard Salary Residual", group)
    # Lots of variation, big difference in wage.
    # Add to model

}

# Model Analysis --------------------------------

if (FALSE) {

    df <- survey_assemble()
    # salary.predictors <- c("Experience", "Tech.Family", "Company.Size", "Formal.Education", "Major.Undergrad")
    salary.predictors <- model_variables[-which(model_variables == "Country")]
    salary.formula <- eval(parse(text = standardSalary_formula(salary.predictors)))

    seed <- 7
    salary.metric <- "RMSE"
    salary.control <- caret::trainControl(method = "cv", number = 5)

    rm(list = ls(pattern = "^salary\\.fit\\."))

    # Linear regression
    set.seed(seed)
    salary.fit.lm <- caret::train(salary.formula, data = df, method = "lm", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)
    # Robust Linear Model
    #set.seed(seed)
    #salary.fit.rlm <- train(salary.formula, data = df, method = "rlm", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)
    # Partial Least Squares
    set.seed(seed)
    salary.fit.pls <- caret::train(salary.formula, data = df, method = "pls", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)
    # Elasticnet
    set.seed(seed)
    salary.fit.enet <- caret::train(salary.formula, data = df, method = "enet", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)
    # glmnet
    set.seed(seed)
    salary.fit.glmnet <- caret::train(salary.formula, data = df, method = "glmnet", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)
    # Least Angle Regression
    set.seed(seed)
    salary.fit.lars <- caret::train(salary.formula, data = df, method = "lars", metric = salary.metric, preProc = c("center", "scale"), trControl = salary.control)

    salary.fitnames <- ls(pattern = "^salary\\.fit\\.")
    salary.fits <- salary.fitnames %>% map(get) %>% set_names(salary.fitnames)
    salary.resamps <- caret::resamples(salary.fits)
    dotplot(salary.resamps, metric = salary.metric)

    salary.fit <- salary.fit.lm

    varImp(salary.fit)
    plot(varImp(salary.fit))

    control <- trainControl(method = "cv", number = 5)
    seed <- 7
    metric <- "Accuracy"

    whitespace.predictors <- c("Country", salary.predictors)
    whitespace.formula <- eval(parse(text = whitespace_formula(whitespace.predictors)))

    rm(list = ls(pattern = "^whitespace\\.fit\\."))

    set.seed(seed)
    whitespace.fit.lda <- train(whitespace.formula, data = df, method = "lda", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Logistic Regression
    set.seed(seed)
    whitespace.fit.glm <- train(whitespace.formula, data = df, method = "glm", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Penalized Logistic Regression
    set.seed(seed)
    whitespace.fit.plr <- train(whitespace.formula, data = df, method = "plr", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Ordered Logistic or Probit Regression
    #set.seed(seed)
    #fit.polr <- train(whitespace.formula, data = df, method = "polr", metric = metric, preProc = c("center", "scale"), trControl = control)
    # Partial Least Squares
    set.seed(seed)
    whitespace.fit.pls <- train(whitespace.formula, data = df, method = "pls", metric = metric, preProc = c("center", "scale"), trControl = control)
    # GLMNET
    set.seed(seed)
    tunegrid <- expand.grid(.alpha = seq(0, 1, by = 0.1), .lambda = c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6))
    whitespace.fit.glmnet <- train(whitespace.formula, data = df, method = "glmnet", metric = metric, tuneGrid = tunegrid, preProc = c("center", "scale"), trControl = control)
    # sparce LDA
    set.seed(seed)
    whitespace.fit.sda <- train(whitespace.formula, data = df, method = "sparseLDA", metric = metric, preProc = c("center", "scale"), trControl = control)

    whitespace.fitnames <- ls(pattern = "^whitespace\\.fit\\.")
    whitespace.fits <- whitespace.fitnames %>% map(get) %>% set_names(whitespace.fitnames)
    whitespace.resamps <- caret::resamples(whitespace.fits)
    dotplot(whitespace.resamps, metric = metric)

    whitespace.fit <- whitespace.fit.glmnet

    varImp(whitespace.fit)
    plot(varImp(whitespace.fit))

}


# Tabs vs Space Analysis --------------------------------

preferenceAnalysis_assemble <- function(df, salaryPredictors, whitespacePredictors) {

    # So we have a model that corrects for a bunch o stuff.

    # So, to check whitespace vs. tabs

    # Generate records. Correct for variations.
    # Box plot Tabs vs Spaces residuals

    salaryPredictors <- model_variables
    whitespacePredictors <- model_variables
    df <- survey_assemble()

    standardSalaryFit <- standardSalary_fit(df, salaryPredictors)
    

    seed = 7
    set.seed(seed)
    whitespaceFit <- glm(whitespace.formula, df, family = "binomial")
    df$Prefer.Space.Predicted.Logodds <- predict(whitespaceFit)
    df$Prefer.Space.Predicted.Odds <- exp(df$Prefer.Space.Predicted.Logodds)
    df$Prefer.Space.Probability <- df$Prefer.Space.Predicted.Odds / (1 + df$Prefer.Space.Predicted.Odds)
    df$Prefer.Space.Predicted <- df$Prefer.Space.Probability < runif(nrow(df))
    df$Whitespace.Residual <- if_else(df$Prefer.Space.Predicted != df$Prefer.Space, as.character(df$Whitespace), "Expected")

    analysis <- df %>% select(Salary, Normal.Salary, Standard.Salary, Whitespace, Prefer.Space, Prefer.Space.Predicted.Logodds, Prefer.Space.Predicted.Odds, Prefer.Space.Probability, Prefer.Space.Predicted, Whitespace.Residual, one_of(whitespace.predictors))
    analysis
}

if (FALSE) {

    outliers <- FALSE
    top4 <- FALSE

    df <- survey_assemble(outliers)
    if (top4) {
        top4 <- df %>% group_by(Country) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(4) %>% .$Country
        df <- df %>% filter(Country %in% top4)
        df$Country <- df$Country%>% fct_drop()
    }
    analysis <- df %>% preferenceAnalysis_assemble(seed)

    dollarFormat <- scales::dollar_format(largest_with_cents = 0)
    spaceSalary <- analysis %>% filter(Whitespace.Residual == "Spaces") %>% .$Standard.Salary %>% median()
    tabsSalary <- analysis %>% filter(Whitespace.Residual == "Tabs") %>% .$Standard.Salary%>% median()
    cat("Spaces salary: ", dollarFormat(spaceSalary), "\n")
    cat("Tabs salary: ", dollarFormat(tabsSalary), "\n")

    analysis %>% ggplot(aes(x = Whitespace.Residual, y = Standard.Salary)) + geom_boxplot()
}


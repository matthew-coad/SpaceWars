# Assemble Survey --------------------------------

# Cleans the data and assemblies everything into a single dataset

survey_tagNames <- function() {
    allNames <- ls(.GlobalEnv)
    allNames %>% map(~get(.)) %>% map_lgl(~"tags" %in% class(.)) %>% allNames[.]
}

survey_tagColumns <- function() {
    tagNames <- survey_tagNames()
    cTags <- tagNames %>% map(~get(.)) %>% flatten()
    cTags
}

defaultSalaryMaxSD = 3
defaultOutliers = FALSE
standardCountry <- "United Kingdom"

# Radically different salaries between countries makes things ackward.
# Strip outliers and then use whats left over to scale the salary to a normalised distribution

survey_clean <- function(df, salaryMaxSD = defaultSalaryMaxSD) {

    salary <- df$Salary
    yearsProgram <- df$Years.Program
    medianSalary <- median(salary)
    sdSalary <- sd(salary)
    minSalary <- medianSalary - (salaryMaxSD * sdSalary)
    maxSalary <- medianSalary + (salaryMaxSD * sdSalary)
    outliers <- salary < minSalary | salary > maxSalary

    # Normalised salary 
    # After much humming and huring I think mean salaries would be more easily intereptable (IE. I can follow whats going on)
    dfcMeanSalary <- mean(df$Salary[!outliers])
    dfcSdSalary <- sd(df$Salary[!outliers])
    normalSalary <- (salary - dfcMeanSalary) / dfcSdSalary

    df$Outlier <- outliers
    df$Normal.Salary <- normalSalary
    df
}

# Assembly the survey data. Set outliers to true to include outliers
survey_assemble <- function(outliers = defaultOutliers, salaryMaxSD = defaultSalaryMaxSD) {

    columns <- splice(
        Salary = salary,
        Country = country,
        Whitespace = whitespace,
        Prefer.Space = preferSpace,
        Years.Program = yearsProgram,
        Experience = experience,
    # Tech.Family = techFamily,
        Company.Size = companySize,
        Formal.Education = formalEducation,
        Major.Undergrad = MajorUndergrad,
        University = University,
        Employment.Status = EmploymentStatus,
        Professional = Professional,
        Home.Remote = HomeRemote,
        Company.Type = CompanyType,
        Highest.Education.Parents = HighestEducationParents
    )

    columns <- append(columns, survey_tagColumns())

    df <- bind_cols(columns)
    df$Country <- df$Country %>% fct_reorder(df$Salary)
    df <- df %>% split(.$Country) %>% map(~survey_clean(., salaryMaxSD)) %>% bind_rows()

    standardSalaries <- df %>% filter(Country == standardCountry & !Outlier) %>% .$Salary
    df$Standard.Salary <- df$Normal.Salary * sd(standardSalaries) + mean(standardSalaries)

    df <- df %>% select(Salary, Normal.Salary, Standard.Salary, Whitespace, everything())
    df <- if (outliers) df else df %>% filter(!Outlier)
    df
}


# Model Building -----------------------------------------------

#All 
model_predictors <- function() {
    c(names(survey_tagColumns()), "Experience")
}

predictors_formula <- function(response, predictors) eval(parse(text = sprintf("%s ~ %s", response, paste(predictors, collapse = "+"))))
#whitespace_formula <- function(predictors = model_predictors()) eval(parse(text = sprintf("Prefer.Space  ~ %s", paste(predictors, collapse = "+"))))
#salary_formula <- function(predictors = model_predictors()) eval(parse(text = sprintf("Salary ~ %s", paste(predictors, collapse = "+"))))
#normalSalary_formula <- function(predictors = model_predictors()) eval(parse(text = sprintf("Normal.Salary ~ %s", paste(predictors, collapse = "+"))))
#standardSalary_formula <- function(predictors = model_predictors()) eval(parse(text = sprintf("Standard.Salary ~ %s", paste(predictors, collapse = "+"))))

glmnet_dmatrix <- function(df, response, predictors) {
    formula <- predictors_formula(response, predictors)
    x <- data.matrix(df[, predictors])
    y <- df[[response]]
    dmatrix <- list(x = x, y = y, response = response, predictors = predictors)
    class(dmatrix) <- c("dmatrix", class(dmatrix))
    dmatrix
}

glmnet_fitplot <- function(fit) {
    print(fit)
    par(mfrow = c(2, 2))
    plot(salaryFit, xvar = "norm")
    plot(salaryFit, xvar = "lambda")
    plot(salaryFit, xvar = "dev")
}

glmnet_alphaplot <- function(dm) {

    foldid = sample(1:10, size = length(dm$y), replace = TRUE)
    cv1 = glmnet::cv.glmnet(x, y, foldid = foldid, alpha = 1)
    cv.5 = glmnet::cv.glmnet(x, y, foldid = foldid, alpha = .5)
    cv0 = glmnet::cv.glmnet(x, y, foldid = foldid, alpha = 0)

    par(mfrow = c(2, 2))
    plot(cv1);
    plot(cv.5);
    plot(cv0)
    plot(log(cv1$lambda), cv1$cvm, pch = 19, col = "red", xlab = "log(Lambda)", ylab = cv1$name)
    points(log(cv.5$lambda), cv.5$cvm, pch = 19, col = "grey")
    points(log(cv0$lambda), cv0$cvm, pch = 19, col = "blue")
    legend("topleft", legend = c("alpha= 1", "alpha= .5", "alpha 0"), pch = 19, col = c("red", "grey", "blue"))

}
# Plot utility -----------------------------------------------

plot_sideBySide <- function(plot1, plot2) gridExtra::grid.arrange(plot1, plot2, ncol = 2)
plot_2x2 <- function(plot11, plot12, plot21, plot22) gridExtra::grid.arrange(plot11, plot12, plot21, plot22, ncol = 2)
plot_2x3 <- function(plot11, plot12, plot21, plot22, plot31, plot32) gridExtra::grid.arrange(plot11, plot12, plot21, plot22, ncol = 2)

# Continuous variable Analysis --------------------------------

continuousGroup_histogram <- function(df, xVariable, xLabel, groupVariable, group, title = NULL, bins = 30) {
    title <- if (is.null(title)) sprintf("%s", group) else title
    df <- df[df[[groupVariable]] == group,]
    xMean <- mean(df[[xVariable]])
    yMedian <- median(df[[xVariable]])
    ggplot(df, aes_string(x = xVariable)) +
        geom_histogram(bins = bins) +
        geom_vline(xintercept = xMean) +
        geom_vline(xintercept = yMedian, color = "blue") +
        xlab(xLabel) +
        ylab("Count") +
        ggtitle(title)
}

continuousGroup_histograms <- function(df, xVariable, xLabel, groupVariable, max = 9, bins = 30) {
    group <- df %>% group_by_(groupVariable) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(max) %>% .[[groupVariable]]
    plots <- group %>% map(~continuousGroup_histogram(df, xVariable, xLabel, groupVariable, ., bins = bins))
    do.call(gridExtra::grid.arrange, plots)
}

continuousGroup_density <- function(df, xVariable, xLabel, groupVariable, group, title = NULL) {
    title <- if (is.null(title)) sprintf("%s", group) else title
    df <- df[df[[groupVariable]] == group,]
    xMean <- mean(df[[xVariable]])
    yMedian <- median(df[[xVariable]])
    ggplot(df, aes_string(x = xVariable)) +
        geom_density() +
        geom_vline(xintercept = xMean) +
        geom_vline(xintercept = yMedian, color = "blue") +
        xlab(xLabel) +
        ggtitle(title)
}

continuousGroup_densities <- function(df, xVariable, xLabel, groupVariable, max = 9) {
    group <- df %>% group_by_(groupVariable) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(max) %>% .[[groupVariable]]
    plots <- group %>% map(~continuousGroup_density(df, xVariable, xLabel, groupVariable, .))
    do.call(gridExtra::grid.arrange, plots)
}

salaryVsPreferenceByGroup_assemble <- function(df, groupVariable) {

    stats <- df %>%
        group_by_(groupVariable) %>%
        summarise(
            Count = n(),
            Salary = mean(Salary),
            Normal.Salary = mean(Normal.Salary),
            Standard.Salary = mean(Standard.Salary),
            Space.Preference = mean(Whitespace == "Spaces"),
            Space.Odds = Space.Preference / (1 - Space.Preference),
            Space.Logodds = log(Space.Preference / (1 - Space.Preference))
        )
    stats
}

#NOT WORKING
salaryVsPreferencePredictByGroup_lmassemble <- function(df, groupVariable, predictors) {

    allSpacePreference <- mean(df$Prefer.Space)
    allSpaceOdds <- allSpacePreference / (1 - allSpacePreference)
    allSpaceLogodds <- log(allSpaceOdds)

    df <- df %>% add_predictions(fit, "Salary.Predicted") %>% add_residuals(salaryFit, "Salary.Residual")

    normalSalaryFit <- lm(normalSalary_formula(predictors), df)
    df <- df %>% add_predictions(normalSalaryFit, "Normal.Salary.Predicted") %>% add_residuals(normalSalaryFit, "Normal.Salary.Residual")

    standardSalaryFit <- lm(standardSalary_formula(predictors), df)
    df <- df %>% add_predictions(standardSalaryFit, "Standard.Salary.Predicted") %>% add_residuals(standardSalaryFit, "Standard.Salary.Residual")

    whitespaceFit <- glm(whitespace_formula(predictors), df, family = "binomial")
    df <- df %>% add_predictions(whitespaceFit, "Space.Preference.Logodds")

    stats <- df %>%
        group_by_(groupVariable) %>%
        summarise(
            Count = n(),
            Salary = mean(Salary),
            Salary.Predicted = mean(Salary.Predicted),
            Normal.Salary = mean(Normal.Salary),
            Normal.Salary.Predicted = mean(Normal.Salary.Predicted),
            Standard.Salary = mean(Standard.Salary),
            Standard.Salary.Predicted = mean(Standard.Salary.Predicted),
            Standard.Salary.Residual = mean(Standard.Salary.Residual),
            Space.Preference = mean(Prefer.Space),
            Space.Odds = Space.Preference / (1 - Space.Preference),
            Space.Logodds = log(Space.Odds),
            Space.Logodds.Predicted = mean(Space.Preference.Logodds),
            Space.Logodds.Residual = Space.Logodds - Space.Logodds.Predicted - allSpaceLogodds,
            Space.Odds.Residual = exp(Space.Logodds.Residual)
        )
    stats
}

salaryVsPreferencePredict_assemble <- function(df) {

    predictors <- model_predictors()
    salaryDm <- glmnet_dmatrix(df, "Standard.Salary", predictors)
    salaryFit <- glmnet::cv.glmnet(salaryDm$x, salaryDm$y, alpha = 1)
    salaries <- df$Standard.Salary
    salaryPredictions <- predict(salaryFit, newx = salaryDm$x, s = "lambda.1se")
    salaryResiduals <- salaries - salaryPredictions

    df$Standard.Salary.Predicted <- salaryPredictions
    df$Standard.Salary.Residual <- salaryResiduals

    preferSpaceDm <- glmnet_dmatrix(df, "Prefer.Space", predictors)
    preferSpaceFit <- glmnet::cv.glmnet(preferSpaceDm$x, preferSpaceDm$y, alpha = 1, family = "binomial")
    preferSpaceLogOdds <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = "lambda.1se", type = "link")
    preferSpacePredicted <- predict(preferSpaceFit, newx = preferSpaceDm$x, s = "lambda.1se", type = "class")

    df$Space.Preference.Logodds <- preferSpaceLogOdds
    df$Space.Preference.Predicted <- preferSpacePredicted
    df$Whitespace.Confusion <- if_else(df$Space.Preference.Predicted != df$Prefer.Space, as.character(df$Whitespace), "Expected")

    df
}

salaryVsPreferenceByGroup_assemble <- function(df, groupVariable) {

    allSpacePreference <- mean(df$Prefer.Space)
    allSpaceOdds <- allSpacePreference / (1 - allSpacePreference)
    allSpaceLogodds <- log(allSpaceOdds)

    stats <- df %>%
            group_by_(groupVariable) %>%
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

continuousVsContinuousByGroup_plot <- function(stats, xVariable, xLabel, yVariable, yLabel, groupVariable, title = NULL, minRespondents = 100) {
    title <- if (!is.null(title)) title else sprintf("%s vs %s by %s", xLabel, yLabel, groupVariable)
    stats <- stats %>% filter(Count >= minRespondents)
    ggplot(stats, aes_string(x = xVariable, y = yVariable, size = "Count")) +
        geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
        geom_count() +
        ggrepel::geom_text_repel(aes_string(label = groupVariable), size = 3.5) +
        scale_size_area(name = "Respondents") +
        xlab(xLabel) +
        ylab(yLabel) +
        ggtitle(title)
}

continuousVsContinuousByGroupColor_plot <- function(stats, xVariable, xLabel, yVariable, yLabel, groupVariable, colorVariable, title = NULL, minRespondents = 100) {
    title <- if (!is.null(title)) title else sprintf("%s vs %s by %s", xLabel, yLabel, groupVariable)
    stats <- stats %>% filter(Count >= minRespondents)
    ggplot(stats, aes_string(x = xVariable, y = yVariable, size = "Count")) +
        geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
        geom_count(aes_string(color = colorVariable)) +
        ggrepel::geom_text_repel(aes_string(label = groupVariable, color = colorVariable), size = 3.5) +
        scale_size_area(name = "Respondents") +
        xlab(xLabel) +
        ylab(yLabel) +
        ggtitle(title)
}

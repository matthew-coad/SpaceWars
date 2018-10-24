
library(tidyverse)
library(forcats)
library(broom)
library(modelr)
library(stringi)

rm(list = ls())


#Assemble Summary --------------------------------

predictor_variables <- c("Country","Experience")
whitespace_formula <- function(predictors) sprintf("Whitespace ~ %s", paste(predictors, collapse = "+"))
salary_formula <- function(predictors) sprintf("Salary ~ %s", paste(predictors, collapse = "+"))
normalSalary_formula <- function(predictors) sprintf("Normal.Salary ~ %s", paste(predictors, collapse = "+"))

df <- survey_assemble()
standardised = FALSE
groupVariable = "Experience"

salaryVsPreferenceByGroup_assemble <- function(df, groupVariable, standardised = TRUE) {

    df <- if (standardised) df %>% filter(!outlier) else df
    #salary <- if (standardised) df$Standard.Salary else df$Salary

    predictors <- predictor_variables[-which(predictor_variables == groupVariable)]

    #salaryFit <- lm(salary_formula(predictors), df)
    #normalSalaryFit <- lm(normalSalary_formula(predictors), df)
    #whitespaceFit <- glm(whitespace_formula(predictors), df, family = "binomial")

#    df <- if (standard) df %>% filter(!outlier) else df

#    companyfit <- lm(normalSalary ~ techPredicted + companySize, dfc)
#    df <- df %>% add_predictions(companyfit, "companyPredicted") %>% add_residuals(companyfit, "companyAdjustedSalary")

#    allfit <- lm(normalSalary ~ techPredicted + companySize + formalEducation + CompanyType + MajorUndergrad, dfc)
#    df <- df %>% add_predictions(allfit, "allPredicted") %>% add_residuals(allfit, "allAdjustedSalary")

    df %>%
        group_by_(groupVariable) %>%
        summarise(
            count = n(),
            Salary = mean(Salary),
            Normal.Salary = mean(Normal.Salary),
            Standard.Salary = sd(Standard.Salary),
            Space.Preference = mean(Whitespace == "Spaces")
        )

    #preferSpaces = sum(whitespace == "Spaces") / n(),
     # spaceOdds = preferSpaces / (1 - preferSpaces),
      #spaceLogOdds = log(spaceOdds)


    #tibble(salary = salary, whitespace = whitespace, group = group) %>%
    #group_by(group) %>%
    #summarise(
    #  Count = n(),
    #) %>%
    #mutate(rank = desc(min_rank(count))) %>%
    #mutate(rank = rank - min(rank) + 1) %>%
    #arrange(desc(count))
}


#Methodology <- survey.data$Methodology %>% fct_explicit_na() %>% as.factor()

#summary(survey.data$Methodology %>% fct_explicit_na() %>% as.factor())

#list(col = "Methodology", class = "tags", ordered = 0, allowna = 0, levels = structure(c("Agile", "Lean", "Scrum", "Extreme", "Pair", "Kanban",
#                                                                                         "Domain-driven design", "Waterfall", "PRINCE2", "Mob", "Evidence-based SWE"
#      ), na.action = structure(1L, class = "omit"))),
#      list(col = "VersionControl", class = "factor", ordered = 0, allowna = 1, levels = structure(c("Git", "Mercurial", "Zip file back-ups", "Team Foundation Server",
#                                                                                              "Subversion", "I use some other system", "I don't use version control",
#                                                                                              "Visual Source Safe", "Copying and pasting files to network shares",
#                                                                                              "Rational ClearCase"), na.action = structure(1L, class = "omit"))),
#      list(col = "CheckInCode", class = "factor", ordered = 0, allowna = 1, levels = structure(c("Multiple times a day", "A few times a week", "Just a few times over the year",
#                                                                                           "Never", "A few times a month", "Once a day"), na.action = structure(1L, class = "omit"))),
#      list(col = "ShipIt", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                            "Strongly agree")),
#      list(col = "OtherPeoplesCode", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                                      "Strongly agree")),
#      list(col = "ProjectManagement", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                                       "Strongly agree")),
#      list(col = "EnjoyDebugging", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                                    "Strongly agree")),
#      list(col = "InTheZone", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                               "Strongly agree")),
#      list(col = "DifficultCommunication", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                                            "Strongly agree")),
#      list(col = "CollaborateRemote", class = "factor", ordered = 1, allowna = 1, levels = c("Strongly disagree", "Disagree", "Somewhat agree", "Agree",
#                                                                                       "Strongly agree")),
#      list(col = "MetricAssess", class = "tags", ordered = 0, allowna = 0, levels = structure(c("Customer satisfaction", "On time/in budget", "Peers' rating",
#                                                                                          "Self-rating", "Benchmarked product performance", "Manager's rating",
#                                                                                          "Other", "Revenue performance", "Release frequency", "Bugs found",
 #                                                                                         "Hours worked", "Commit frequency", "Lines of code"), na.action = structure(1L, class = "omit"))),

#Stats ------------------------------------

stats.plot <- function(stats, title = NULL) {
    p <- ggplot(stats, aes(x = preferSpaces, y = median_salary, size = count)) +
        geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +
        geom_count() +
        ggrepel::geom_text_repel(aes(label = group), size = 3.5) +
        scale_size_area(name = "Respondents") +
        ylab("Median salary") +
        xlab("Spaces preference")
    if (!is.null(title))
        p <- p + ggtitle(title)
    p
}

#Groups ------------------------------------

group.top <- function(x, count) x[x$rank <= count,]$group

#Group histogram
group.histogram <- function(x, xLabel, group, select, bins = 20, limits = NULL) {
    data <- tibble(x = x, group = group) %>% filter(group == select)
    p <-
        ggplot(data) +
        geom_histogram(aes(x = x), bins = 20) +
        xlab(xLabel) +
        ylab(paste0(select, " count"))

    if (!is.null(limits)) {
        p <- p + coord_cartesian(xlim = limits)
    }
    p
}

group.preferencePlot <- function(salary, salaryLabel, group, groupLabel, whitespace) {
    data <- tibble(salary = salary, whitespace = whitespace, group = group)
    ggplot(data, aes(x = group, y = salary, fill = whitespace)) +
      geom_boxplot() +
      scale_fill_discrete(name = "Preference", breaks = c("Spaces", "Tabs")) +
      coord_flip() +
      theme_minimal() +
      xlab(groupLabel) +
      ylab(salaryLabel)
}

#9 plots in a 3-by-3 grid
plot9 <- function(plots) {
    do.call(gridExtra::grid.arrange, plots)
}

plot2 <- function(plots) {
    do.call(gridExtra::grid.arrange, splice(plots, list(ncol = 2)))
}

#Outliers -----------------------------------------

outliers.sdFromMedian <- function(x, deviations) {
    midValue <- median(x)
    sdValue <- sd(x)
    minValue <- midValue - (deviations * sdValue)
    maxValue <- midValue + (deviations * sdValue)
    x < minValue | x > maxValue
}

#Clean data ---------------------------------

d <- tibble(salary, country)

survey.loadclean <- function(maxDeviations = 2) {
    d <- tibble(salary, country, whitespace, yearsProgram, experience, techFamily, companySize, formalEducation, MajorUndergrad, University,
                EmploymentStatus, Professional, HomeRemote, CompanyType, DeveloperType, HoursPerWeek, EducationTypes, SelfTaughtTypes, Gender, HighestEducationParents, Race)

    d$country <- d$country %>% fct_reorder(d$salary)

    clean <- function(df) {
        outliers <- outliers.sdFromMedian(df$salary, maxDeviations)

        midValue <- median(df$salary[!outliers])
        sdValue <- sd(df$salary[!outliers])
        normalSalary <- (df$salary - midValue) / sdValue

        df$outliers <- outliers
        df$normalSalary <- normalSalary

        dfc <- df %>% filter(!outliers)

        experiencefit <- lm(normalSalary ~ experience, dfc)
        df <- df %>% add_predictions(experiencefit, "experiencePredicted") %>% add_residuals(experiencefit, "experienceAdjustedSalary")

        techfit <- lm(normalSalary ~ experience + techFamily, dfc)
        df <- df %>% add_predictions(techfit, "techPredicted") %>% add_residuals(techfit, "techAdjustedSalary")

        df
    }

    df <- d %>% split(.$country) %>% map(clean) %>% bind_rows()
    dfc <- df %>% filter(!outliers)

    companyfit <- lm(normalSalary ~ techPredicted + companySize, dfc)
    df <- df %>% add_predictions(companyfit, "companyPredicted") %>% add_residuals(companyfit, "companyAdjustedSalary")

    allfit <- lm(normalSalary ~ techPredicted + companySize + formalEducation + CompanyType + MajorUndergrad, dfc)
    df <- df %>% add_predictions(allfit, "allPredicted") %>% add_residuals(allfit, "allAdjustedSalary")

    df
}

#Global analysis ---------------------------------

if (FALSE) {
    globalTabsSalary <- survey.data %>% filter(whitespace == "Tabs") %>% .$Salary %>% median()
    globalSpacesSalary <- survey.data %>% filter(whitespace == "Spaces") %>% .$Salary %>% median()
    cat("Median tabs salary: ", format(globalTabsSalary), "\n")
    cat("Median spaces salary: ", format(globalSpacesSalary), "\n")
    cat("Median spaces advantage: ", format(globalSpacesSalary - globalTabsSalary), "\n")
}

#Country analysis ---------------------------------

country.loadstats <- function(data, normal = TRUE) {
    if (normal)
        stats(data$normalSalary, data$whitespace, data$country)
    else
        stats(data$salary, data$whitespace, data$country)
}

country.data <- survey.loadclean()
country.cdata <- country.data %>% filter(!outliers)
standardCountry <- "United Kingdom"
standardMedian <- country.cdata %>% filter(country == standardCountry) %>% .$salary %>% median()
standarSd <- country.cdata %>% filter(country == standardCountry) %>% .$salary %>% sd()

standardSalary <- function(x) x * standarSd + standardMedian

# Country salary vs preference
if (FALSE) {
    country.data %>% country.loadstats(FALSE) %>% stats.plot()
}

# Top 9 country salary histograms
if (FALSE) {
    top9Countries <- country.cdata %>% country.loadstats(FALSE) %>% group.top(9)
    top9Countries %>% map(~ group.histogram(country.cdata$salary, "Salary", country.cdata$country, .)) %>% plot9()
}

# Top 9 country normal salary histograms
if (FALSE) {
    top9Countries <- country.cdata %>% country.loadstats(FALSE) %>% group.top(9)
    top9Countries %>% map(~group.histogram(country.cdata$normalSalary, "Normal Salary", country.cdata$country, .)) %>% plot9()
}

# *** Country report ***
# Country preference, original vs normalized
if (FALSE) {
    gridExtra::grid.arrange(
        group.preferencePlot(country.cdata$salary, "Salary", country.cdata$country, "Country", country.cdata$whitespace),
        group.preferencePlot(country.cdata$normalSalary %>% standardSalary(), "Normal Salary", country.cdata$country, "Country", country.cdata$whitespace),
        ncol = 2
    )

    group.preferencePlot(country.cdata$allAdjustedSalary%>% standardSalary(), "Adjusted Salary", country.cdata$country, "Country", country.cdata$whitespace)

    countryTabSalary <- country.cdata %>% filter(whitespace == "Tabs") %>% .$normalSalary %>% standardSalary() %>% median()
    countrySpacesSalary <- country.cdata %>% filter(whitespace == "Spaces") %>% .$normalSalary %>% standardSalary() %>% median()
    cat("Median tabs standard salary: ", format(countryTabSalary), "\n")
    cat("Median spaces standard salary: ", format(countrySpacesSalary), "\n")
    cat("Median spaces stanard salary advantage: ", format(countrySpacesSalary - countryTabSalary), "\n")
}

#Experience analysis ---------------------------------

experience.data <- survey.loadclean()
experience.cdata <- experience.data %>% filter(!outliers)

## Experience vs space preference
if (FALSE) {
    stats(experience.cdata$normalSalary, experience.cdata$whitespace, experience.cdata$yearsProgram) %>% stats.plot()
}

countryExperiencePreferencePlot <- function(countrySelect) {
    cdata <- experience.cdata %>% filter(country == countrySelect)
    fit <- lm(normalSalary ~ experience, data = cdata)
    fit.all <- lm(normalSalary ~ experience, data = experience.cdata)
    cdata <- cdata %>% add_predictions(fit)

    grid <- cdata %>% data_grid(experience) %>% add_predictions(fit, "normalSalary") %>% add_predictions(fit.all, "allSalary")

    cdata$experience <- as.factor(cdata$experience)
    grid$experience <- as.factor(grid$experience)

    ggplot(cdata, aes(x = experience, y = normalSalary)) +
          geom_boxplot() +
          geom_point(data = grid, colour = "red", size = 2) +
          geom_point(data = grid, aes(y = allSalary), colour = "blue", size = 2) +
          scale_fill_discrete(name = "Preference", breaks = c("Spaces", "Tabs")) +
          theme_minimal() +
          xlab("Experience") +
          ylab("Normal salary") +
          ggtitle(countrySelect)
}

## Experience vs space preference. Top 9 countries
if (FALSE) {
    top9Countries <- experience.cdata %>% country.loadstats(FALSE) %>% group.top(9)
    top9Countries %>% map(countryExperiencePreferencePlot) %>% plot9()
}


if (FALSE) {
    gridExtra::grid.arrange(
        ggplot(experience.cdata) + geom_boxplot(aes(x = yearsProgram, y = salary)),
        ggplot(experience.cdata) + geom_boxplot(aes(x = yearsProgram, y = normalSalary)) + geom_boxplot(aes(x = yearsProgram, y = predictedSalary), fill = "green"),
        ggplot(experience.cdata) + geom_boxplot(aes(x = yearsProgram, y = residualSalary, fill = whitespace)),
        nrow = 2
    )
}

# *** Experience report ***
# Country preference, original vs normalized
if (FALSE) {
    gridExtra::grid.arrange(
        group.preferencePlot(experience.cdata$normalSalary %>% standardSalary(), "Normal Salary", experience.cdata$yearsProgram, "Experience", experience.cdata$whitespace),
        group.preferencePlot(experience.cdata$experienceAdjustedSalary %>% standardSalary(), "Adjusted Salary", experience.cdata$yearsProgram, "Experience", experience.cdata$whitespace),
        ncol = 2
    )

    group.preferencePlot(experience.cdata$allAdjustedSalary %>% standardSalary(), "Adjusted Salary", experience.cdata$yearsProgram, "Experience", experience.cdata$whitespace)

    experienceTabSalary <- experience.cdata %>% filter(whitespace == "Tabs") %>% .$experienceAdjustedSalary %>% standardSalary() %>% median()
    experienceSpacesSalary <- experience.cdata %>% filter(whitespace == "Spaces") %>% .$experienceAdjustedSalary %>% standardSalary() %>% median()
    cat("Median tabs experience adjusted salary: ", format(experienceTabSalary), "\n")
    cat("Median spaces experience adjusted salary: ", format(experienceSpacesSalary), "\n")
    cat("Median spaces experience adjusted salary advantage: ", format(experienceSpacesSalary - experienceTabSalary), "\n")
}

#Tech analysis ---------------------------------

tech.data <- survey.loadclean()
tech.cdata <- tech.data %>% filter(!outliers)

# *** Tech report ***
# Country preference, original vs normalized
if (FALSE) {
    gridExtra::grid.arrange(
        group.preferencePlot(tech.cdata$experienceAdjustedSalary %>% standardSalary(), "Experience adjusted Salary", tech.cdata$techFamily, "Tech family", tech.cdata$whitespace),
        group.preferencePlot(tech.cdata$techAdjustedSalary %>% standardSalary(), "Tech Adjusted Salary", tech.cdata$techFamily, "Tech family", tech.cdata$whitespace),
        ncol = 2
    )
    techTabSalary <- tech.cdata %>% filter(whitespace == "Tabs") %>% .$techAdjustedSalary %>% standardSalary() %>% median()
    techSpacesSalary <- tech.cdata %>% filter(whitespace == "Spaces") %>% .$techAdjustedSalary %>% standardSalary() %>% median()
    cat("Median tabs tech adjusted salary: ", format(techTabSalary), "\n")
    cat("Median spaces tech adjusted salary: ", format(techSpacesSalary), "\n")
    cat("Median spaces tech adjusted salary advantage: ", format(techSpacesSalary - techTabSalary), "\n")
}

#Stats for tags must be evaluated seperately for each tag and bound together

tagStat <- function(data, tagColumn) {
    tagdata <- data %>% filter_(sprintf("\`%s\`", tagColumn))
    familyTable <- table(tagdata$techFamily)
    count <- nrow(tagdata)
    median_salary = median(tagdata$salary)
    sd_salary = sd(tagdata$salary)
    preferSpaces = mean(tagdata$whitespace == "Spaces")
    techFamily = levels(techFamily)[which(familyTable == max(familyTable))[1]]
    parts <- stri_split_fixed(str = tagColumn, pattern = "_")[[1]]
    category <- parts[1]
    name <- parts[2]
    stat <- tibble(group = tagColumn, name, category, count, median_salary, sd_salary, preferSpaces, techFamily, familyTable = familyTable)
    stat
}

tagColumns <- function(data, colPrefix) {
    colnames(data)[stri_startswith_fixed(str = colnames(data), pattern = colPrefix)]
}

tagsStats <- function(data, colName, colPrefix) {
    prefixStats <- function(prefix)
        tagColumns(data, prefix) %>%
            map(tagStat, data = data, colName = colName)
    stats <- colPrefix %>% map(prefixStats) %>% flatten() %>% bind_rows() %>% arrange(desc(count))
    stats$techFamily <- as.factor(stats$techFamily)
    stats
}

#Company analysis ---------------------------------

company.data <- survey.loadclean()
company.cdata <- company.data %>% filter(!outliers)

company.cdata %>% select(techAdjustedSalary, companyAdjustedSalary)

# *** Company report ***
# Country preference, original vs normalized
if (FALSE) {
    gridExtra::grid.arrange(
        group.preferencePlot(company.cdata$techAdjustedSalary %>% standardSalary(), "Tech adjusted Salary", company.cdata$companySize, "Company", company.cdata$whitespace),
        group.preferencePlot(company.cdata$companyAdjustedSalary %>% standardSalary(), "Company Adjusted Salary", company.cdata$companySize, "Company", company.cdata$whitespace),
        ncol = 2
    )
    companyTabSalary <- company.cdata %>% filter(whitespace == "Tabs") %>% .$companyAdjustedSalary %>% standardSalary() %>% median()
    companySpacesSalary <- company.cdata %>% filter(whitespace == "Spaces") %>% .$companyAdjustedSalary %>% standardSalary() %>% median()
    cat("Median tabs company adjusted salary: ", format(companyTabSalary), "\n")
    cat("Median spaces company adjusted salary: ", format(companySpacesSalary), "\n")
    cat("Median spaces company adjusted salary advantage: ", format(companySpacesSalary - companyTabSalary), "\n")
}


#All analysis ---------------------------------

all.data <- survey.loadclean()
all.cdata <- all.data %>% filter(!outliers)

summary(lm(normalSalary ~ allPredicted + whitespace, all.data))


# *** All report ***
# Country preference, original vs normalized
if (FALSE) {
#    gridExtra::grid.arrange(
#        group.preferencePlot(all.cdata$techAdjustedSalary %>% standardSalary(), "Tech adjusted Salary", all.cdata$techFamily, "Tech family", all.cdata$whitespace),
#        group.preferencePlot(all.cdata$techAdjustedSalary %>% standardSalary(), "All Adjusted Salary", all.cdata$techFamily, "Tech family", all.cdata$whitespace),
#        ncol = 2
#    )

    allTabSalary <- all.cdata %>% filter(whitespace == "Tabs") %>% .$allAdjustedSalary %>% standardSalary() %>% median()
    allSpacesSalary <- all.cdata %>% filter(whitespace == "Spaces") %>% .$allAdjustedSalary %>% standardSalary() %>% median()
    cat("Median tabs all adjusted salary: ", format(allTabSalary), "\n")
    cat("Median spaces all adjusted salary: ", format(allSpacesSalary), "\n")
    cat("Median spaces all adjusted salary advantage: ", format(allSpacesSalary - allTabSalary), "\n")
}

scratch.data <- survey.loadclean()
scratch.cdata <- scratch.data %>% filter(!outliers)

scratch.statsPlot <- function(groupVariable) {
    gridExtra::grid.arrange(
        stats(scratch.cdata$allAdjustedSalary %>% standardSalary(), scratch.cdata$whitespace, scratch.cdata[[groupVariable]]) %>% filter(count > 200) %>% stats.plot(title = "All Adjusted Salary"),
        stats(scratch.cdata$techAdjustedSalary %>% standardSalary(), scratch.cdata$whitespace, scratch.cdata[[groupVariable]]) %>% filter(count > 200) %>% stats.plot(title = "Tech Adjusted Salary"),
        stats(scratch.cdata$normalSalary %>% standardSalary(), scratch.cdata$whitespace, scratch.cdata[[groupVariable]]) %>% filter(count > 200) %>% stats.plot(title = "Normal Salary"),
        group.preferencePlot(scratch.cdata$allAdjustedSalary %>% standardSalary(), "Adjusted Salary", scratch.cdata[[groupVariable]], groupVariable, scratch.cdata$whitespace),
        ncol = 2
    )
}

#companySize
#formalEducation
#CompanyType
#MajorUndergrad

if (FALSE) {
    scratch.statsPlot("country")
    scratch.statsPlot("yearsProgram")
    scratch.statsPlot("techFamily")
    scratch.statsPlot("companySize")
    scratch.statsPlot("formalEducation")
    scratch.statsPlot("MajorUndergrad")
    scratch.statsPlot("University")
    scratch.statsPlot("EmploymentStatus")
    scratch.statsPlot("Professional")
    scratch.statsPlot("HomeRemote")
    scratch.statsPlot("CompanyType")
    scratch.statsPlot("DeveloperType")
    # scratch.statsPlot(scratch.data$HoursPerWeek <- survey.data$HoursPerWeek
    scratch.statsPlot("EducationTypes")
    scratch.statsPlot("SelfTaughtTypes")
    scratch.statsPlot("Gender")
    scratch.statsPlot("HighestEducationParents")
    scratch.statsPlot("Race")
}

cat("Completed!!\n")
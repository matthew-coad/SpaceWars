# Country Analysis Views ---------------------------------------

# This is not actually a directly runnable script .
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

#Views related to country

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")

## Country outliers analysis --------------------------

## Histogram of US and India with outliers in/out
## Shows the effect of removing outliers

# US has pretty darn conventional distributions
# India has a pretty darn weird distibution

df <- survey_assemble()

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_histogram("Salary", "Salary", "Country", "United States", "US + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Salary", "Salary", "Country", "United States", "US - Outliers", 30),
    survey_assemble(TRUE) %>% continuousGroup_histogram("Salary", "Salary", "Country", "India", "India + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Salary", "Salary", "Country", "India", "India - Outliers", 30),
    nrow = 2
)

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_density("Salary", "Salary", "Country", "United States", "US + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Salary", "Salary", "Country", "United States", "US - Outliers"),
    survey_assemble(TRUE) %>% continuousGroup_density("Salary", "Salary", "Country", "India", "India + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Salary", "Salary", "Country", "India", "India - Outliers"),
    nrow = 2
)

# Same plot as above but on salaries scaled to mean of zero and SD of 1.
# Normalised salaries are part of the intermediate salaries but most reports show standard salaries
# Which are easier to relate too.

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_histogram("Normal.Salary", "Normal Salary", "Country", "United States", "US + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Normal.Salary", "Normal Salary", "Country", "United States", "US - Outliers", 30),
    survey_assemble(TRUE) %>% continuousGroup_histogram("Normal.Salary", "Normal Salary", "Country", "India", "India + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Normal.Salary", "Normal Salary", "Country", "India", "India - Outliers", 30),
    nrow = 2
)

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_density("Normal.Salary", "Normal Salary", "Country", "United States", "US + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Normal.Salary", "Normal Salary", "Country", "United States", "US - Outliers"),
    survey_assemble(TRUE) %>% continuousGroup_density("Normal.Salary", "Normal Salary", "Country", "India", "India + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Normal.Salary", "Normal Salary", "Country", "India", "India - Outliers"),
    nrow = 2
)

# Same plot again but showing Standard Salaries

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers", 30),
    survey_assemble(TRUE) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India + Outliers", 30),
    survey_assemble(FALSE) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers", 30),
    nrow = 2
)

gridExtra::grid.arrange(
    survey_assemble(TRUE) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers"),
    survey_assemble(TRUE) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India + Outliers"),
    survey_assemble(FALSE) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers"),
    nrow = 2
)


# I'm happy with the standard salaries and the outlier. Reporting mostly uses that from now on.

## Show US Salaries with different values for the max salary deviation value used to strip outliers
gridExtra::grid.arrange(
    survey_assemble(TRUE, 2) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US + Outliers", 30),
    survey_assemble(FALSE, 2) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers at Sd * 2", 30),
    survey_assemble(FALSE, 2.5) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers at Sd * 2.5", 30),
    survey_assemble(FALSE, 3) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers + Sd3", 30),
    nrow = 2
)

gridExtra::grid.arrange(
    survey_assemble(TRUE, 2) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US + Outliers"),
    survey_assemble(FALSE, 2) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers at Sd * 2"),
    survey_assemble(FALSE, 2.5) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers at Sd * 2.5"),
    survey_assemble(FALSE, 3) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "United States", "US - Outliers + Sd3"),
    nrow = 2
)


## Same for india.
gridExtra::grid.arrange(
    survey_assemble(TRUE, 2) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India + Outliers", 30),
    survey_assemble(FALSE, 2) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers at Sd * 2", 30),
    survey_assemble(FALSE, 2.5) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers at Sd * 2.5", 30),
    survey_assemble(FALSE, 3) %>% continuousGroup_histogram("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers + Sd3", 30),
    nrow = 2
)

gridExtra::grid.arrange(
    survey_assemble(TRUE, 2) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India + Outliers"),
    survey_assemble(FALSE, 2) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers at Sd * 2"),
    survey_assemble(FALSE, 2.5) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers at Sd * 2.5"),
    survey_assemble(FALSE, 3) %>% continuousGroup_density("Standard.Salary", "Standard Salary", "Country", "India", "India - Outliers + Sd3"),
    nrow = 2
)

# The skewed salary distributions!  --------------------------------------------------

df <- survey_assemble() %>% salaryVsIndentation_augment()

# Hangon. In my residuals report why are both residuals negative? Shouldnt they even up?
# I think its got to do with those weird skewed salary distributions.

# Show it for everyone.
# Yes you can see the skewness
df %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary), color = "black") +
    geom_vline(xintercept = mean(df$Standard.Salary), color = "black") +
    geom_density(aes(x = Standard.Salary.Predicted), color = "blue") +
    geom_vline(xintercept = mean(df$Standard.Salary.Predicted), color = "blue")

# Should be worse for India
df %>%
    filter(Country == "India") %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary), color = "black") +
    geom_vline(xintercept = mean(df$Standard.Salary), color = "black") +
    geom_density(aes(x = Standard.Salary.Predicted), color = "blue") +
    geom_vline(xintercept = mean(df$Standard.Salary.Predicted), color = "blue")

# Should be better for United States
df %>%
    filter(Country == "United States") %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary), color = "black") +
    geom_vline(xintercept = mean(df$Standard.Salary), color = "black") +
    geom_density(aes(x = Standard.Salary.Predicted), color = "blue") +
    geom_vline(xintercept = mean(df$Standard.Salary.Predicted), color = "blue")
#Yes that big skewness is gone

# The East European qustion --------------------------------------------------

df <- survey_assemble() %>% salaryVsIndentation_augment()

# Someone said something about East European countries. Polands got a bit of data
binomialCountrySalaryPlot <- function(df, country) {
    df %>%
        filter(Country == country) %>%
        ggplot() +
        geom_vline(xintercept = median(df[df$Country == country,]$Salary)) +
        geom_vline(xintercept = median(df[df$Country == country,]$Salary) / 12) +
        geom_density(aes(x = Salary), color = "black") +
        ggtitle(paste0(country, ", Respondents = ", nrow(df[df$Country == country,])))
}
binomialCountrySalaryWhitespacePlot <- function(df, country) {
    df %>%
        filter(Country == country) %>%
        ggplot() +
        geom_vline(xintercept = median(df[df$Country == country,]$Salary)) +
        geom_vline(xintercept = median(df[df$Country == country,]$Salary) / 12) +
        geom_density(aes(x = Salary, color = Whitespace)) +
        ggtitle(paste0(country, ", Respondents = ", nrow(df[df$Country == country,])))
}


binomialCountrySalaryPlot(df, "Poland")
#Yes. There is that bimodal salary distribution that was reported

# Lets plot all countries in blocks of 9
country_block <- function(countries, block, max) {
    x <- 1:max + ((block - 1) * max)
    x <- x[x <= length(countries)]
    countries[x]
}
binomial_country_block_plot <- function(df, countries, block, max) {
    block <- country_block(countries, block, max)
    binomalPlots <- block %>% map(~binomialCountrySalaryPlot(df, .))
    do.call(gridExtra::grid.arrange, binomalPlots)
}
binomial_country_block_whitespace_plot <- function(df, countries, block, max) {
    block <- country_block(countries, block, max)
    binomalPlots <- block %>% map(~binomialCountrySalaryWhitespacePlot(df, .))
    do.call(gridExtra::grid.arrange, binomalPlots)
}

# For each block identify the countries that appear to have a binomial salary distribution
# with a peak or the major block at the median salary / 12
allCountries <- levels(df$Country)
binomial_country_block_plot(df, allCountries, 1, 9)
monthlyBinomialCountries_1 = c("Pakistan", "Indonesia", "Ukraine", "Russian Federation")

binomial_country_block_plot(df, allCountries, 2, 9)
monthlyBinomialCountries_2 = c("Lithuania", "Romania", "Serbia", "Bulgaria", "Poland")

binomial_country_block_plot(df, allCountries, 3, 9)
monthlyBinomialCountries_3 = c("Hungary", "South Africa")

binomial_country_block_plot(df, allCountries, 4, 9)
monthlyBinomialCountries_4 = c("Sweden")

binomial_country_block_plot(df, allCountries, 5, 9)
monthlyBinomialCountries_5 = c("Denmark", "Israel")

monthlyBinomialCountries = c(monthlyBinomialCountries_1, monthlyBinomialCountries_2, monthlyBinomialCountries_3, monthlyBinomialCountries_4, monthlyBinomialCountries_5)
binomial_country_block_whitespace_plot(df, monthlyBinomialCountries, 1, 4)

binomial_country_block_whitespace_plot(df, monthlyBinomialCountries, 2, 4)

binomial_country_block_whitespace_plot(df, monthlyBinomialCountries, 3, 4)

binomial_country_block_whitespace_plot(df, monthlyBinomialCountries, 4, 4)

# How much of an impact is it?
# Plot the density plot for spaces vs tabs using standard salary
df %>%
    ggplot() +
    geom_vline(xintercept = median(df$Standard.Salary)) +
    geom_vline(xintercept = median(df$Standard.Salary) / 12) +
    geom_density(aes(x = Standard.Salary, color = Whitespace))
# We can see the bump at salary/12. Across the entire dataset its pretty minor.

# The different space vs tabs salary distributions -----------------------------

# Plot the density plot for spaces vs tabs using standard salary
df %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Whitespace))

# But this plot shows the different distributions of spaces vs salaries. A higher tabs peak but 
# a different peak for spaces. And a weird lump at aroound 40K

# Is it present in the US. Lots of "clean" data
df %>%
    filter(Country == "United States") %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Whitespace))
# Yes. Even worse
# Actually you can really see three distinct populations. 3 Distinct humps!

#Repeat for the united kingdom
df %>%
    filter(Country == "United Kingdom") %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Whitespace))
# No 3 humps. Not something universal.

#India for good measure
df %>%
    filter(Country == "India") %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Whitespace))
#Its got its own weirdness

# Country vs Indentation preference -----------------------------

df <- survey_assemble() %>% salaryVsIndentation_augment()
stats <- df %>% salaryVsIndentation_group_summary("Country")

dfc <- survey_assemble() %>% salaryVsIndentation_augment(indentationPredictors = survey_predictors(discardPrefix = c(indentationNoEffect_predictors, "Country")))
statsc <- dfc %>% salaryVsIndentation_group_summary("Country")

# Show the relationship between country and space odds
# Have to use actual salary or it shows a straight line!
gridExtra::grid.arrange(
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Salary", "Actual Salary", "Country", "Actual salary vs indentation odds by country"),
    stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Salary", "Actual Salary", "Country", "Actual salary vs indentation residual by country"),
    statsc %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Salary", "Actual Salary", "Country", "Actual salary vs indentation odds by country"),
    statsc %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Space odds residual", "Salary", "Actual Salary", "Country", "Actual salary vs indentation residual by country"),
    nrow = 2
)

gridExtra::grid.arrange(
    statsc %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Salary", "Actual Salary", "Country", "Actual salary vs actual preference by country"),
    statsc %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted space odds", "Salary", "Actual Salary", "Country", "Actual salary vs predicted preference by country"),
    statsc %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Residual space odds", "Salary", "Actual Salary", "Country", "Actual salary vs preference residual by country"),
    nrow = 2
)

#Differnet countries have dramtically space preferences.

#The country sure matters. Whats the predicted space preference

gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Predicted Space Odds", "Standard.Salary.Predicted", "Standard Salary", "Years.Program", "Standard Salary vs Predicted whitespace by programming experience"),
        nrow = 2
    )







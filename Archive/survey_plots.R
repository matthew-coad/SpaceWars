
# Continuous variable plots --------------------------------

# Plot histogram of a continuous variable for a specific subset of data
# Use with survey_assemble
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
        ggtitle(title) +
        theme_minimal()
}

# Plot histograms of a continuous variable for a group of subsets
# Use with survey_assemble
continuousGroup_histograms <- function(df, xVariable, xLabel, groupVariable, max = 9, bins = 30) {
    group <- df %>% group_by_(groupVariable) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(max) %>% .[[groupVariable]]
    plots <- group %>% map(~continuousGroup_histogram(df, xVariable, xLabel, groupVariable, ., bins = bins))
    do.call(gridExtra::grid.arrange, plots)
}

# Plot density of a continuous variable for a specific subset of data
# Use with survey_assemble
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
        ggtitle(title) +
        theme_minimal()
}

continuousGroup_densities <- function(df, xVariable, xLabel, groupVariable, max = 9) {
    group <- df %>% group_by_(groupVariable) %>% summarise(count = n()) %>% arrange(desc(count)) %>% head(max) %>% .[[groupVariable]]
    plots <- group %>% map(~continuousGroup_density(df, xVariable, xLabel, groupVariable, .))
    do.call(gridExtra::grid.arrange, plots)
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
        ggtitle(title) +
        theme_minimal()
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

# Analysis plots ------------------------------------------------------------

salaryVsIndentationGroupEffect_analysis <- function(groupPrefix, group, groupTitle, minCount = 0) {
    salaryPredictors <- salary_predictors(discardPrefix = c(groupPrefix))
    indentationPredictors <- indentation_predictors(discardPrefix = c(groupPrefix))
    df <- survey_assemble() %>% salaryVsIndentation_augment(salaryPredictors, indentationPredictors)

    stats <- df %>% salaryVsIndentation_group_summary(group) %>% filter(Count > minCount)
    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", group, sprintf("%s - indentation vs salary", groupTitle)),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Base space odds", "Standard.Salary.Predicted", "Base salary", group, sprintf("%s - base indentation vs salary", groupTitle)),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Residual space odds", "Standard.Salary.Residual", "Residual Salary", group, sprintf("%s - residual indentation vs salary", groupTitle)),
        ncol = 2
    )
}

salaryVsIndentationTagEffect_analysis <- function(tagPrefix, tagCategory, tagTitle, minCount = 0) {

    salaryPredictors <- salary_predictors(discardPrefix = c(tagPrefix))
    indentationPredictors <- indentation_predictors(discardPrefix = c(tagPrefix))
    df <- survey_assemble() %>% salaryVsIndentation_augment(salaryPredictors, indentationPredictors)
    tagstats <- df %>% salaryVsIndentation_tags_summary()
    stats <- tagstats %>% filter(Category == tagCategory, Count > minCount )

    gridExtra::grid.arrange(
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds", "Space odds", "Standard.Salary", "Standard Salary", "Name", sprintf("%s - indentation vs salary", tagTitle)),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Predicted", "Base space odds", "Standard.Salary.Predicted", "Base salary", "Name", sprintf("%s - base indentation vs salary", tagTitle)),
        stats %>% continuousVsContinuousByGroup_plot("Space.Odds.Residual", "Residual space odds", "Standard.Salary.Residual", "Residual Salary", "Name", sprintf("%s - residual indentation vs salary", tagTitle)),
        ncol = 2
    )
}



## --------------------------------------------
## Is white Significant ?
## --------------------------------------------

# This script executes a machine learning experiment that tests the hypothesis:

# Is a developers whitespace preference in formatting their code significant in predicting
# their salary?

# Methodology:

# Collect together a set of variables believed to be significant in predicting salaries.
# Compare training model with white space with the result of shuffling the whitespace

# Experiment prefix is "space_sig_"

## ----- Setup -----
    
library(tidyverse)
library(caret)
library(recipes)
library(emc)

## ----- Experiment Definition -----

#' Analysis "Version". Should be incremented if a change invalidates prior cached results.
get_space_sig_version <- function() 11


#' Algorithms to test
get_space_sig_algorithm_info <- function(test_mode = TRUE) {
    
    long_runtime <- c("bagEarth", "rf")
    exclude <- NULL
    
    test_algorithms <- c("lm")
    algorithms <- emc_algorithms() %>% filter(core, regression, !algorithm %in% exclude)
    if (test_mode) {
        algorithms <- algorithms %>% filter(algorithm %in% test_algorithms)
    }
    
    algorithms
}

#' The two different treatments we are testing
get_space_sig_treatments <- function() c("Control", "ShuffleSpace")

#' The processing options we are testing
get_space_sig_preprocess <- function(type = c("Final", "All")) {
    type <- match.arg(type, c("Final", "All"))
    switch (type,
            Final = c("None"),
            All = c("None", "Pls9")
    )
}

#' Get the data needed for the experiment from the survey
as_space_sig_data <- function(survey_df) {
    df <- 
        survey_df %>% 
        mutate(Space_Preference = if_else(Whitespace == "Spaces", 1, 0)) %>%
        select(
            Salary = Standard.Salary,     # Outcome
            Space_Preference,             # Variable in question
            Experience,
            starts_with(languagePrefix),  # And common input data
            starts_with(frameworkPrefix), 
            starts_with(databasePrefix), 
            starts_with(platformPrefix),
            starts_with(idePrefix),
            
            starts_with(versionPrefix),
            starts_with(checkInPrefix),
            starts_with(methodologyPrefix),
            
            # starts_with(countryPrefix),
            starts_with(companySizePrefix),
            starts_with(formalEducationPrefix),
            starts_with(majorUndergradPrefix),
            starts_with(companyTypePrefix),
            starts_with(developerTypePrefix),
            starts_with(genderPrefix)
        )
    df
}

#' Task label
get_space_sig_label <- function(algorithm, treatment, preprocess) {
    label = paste0("space_sig - train ", algorithm, " ", treatment, " preprocess ", preprocess, " v", get_space_sig_version())
    label
}

#' Path to train 
get_space_sig_cache_path <- function(algorithm, treatment, preprocess) {
    path <- paste0("./Cache/space_sig/Train_v", get_space_sig_version(), "_", algorithm, "_", treatment, "_", preprocess)
    path
}

#' The combined experiment parameters
get_space_sig_parameters <- function(algorithms = get_space_sig_algorithm_info()$algorithm, 
                                     treatments = get_space_sig_treatments(), 
                                     preprocess = get_space_sig_preprocess())  {
    
    parameters <- expand.grid(treatment = treatments, algorithm = algorithms, preprocess = preprocess) %>% as_tibble()
    parameters <- parameters %>%
        mutate(label = get_space_sig_label(algorithm, treatment, preprocess)) %>%
        mutate(cache_path = get_space_sig_cache_path(algorithm, treatment, preprocess))
    parameters
}


#' Run the "is space significant" experiment.
run_space_sig <- function(parameters) {
    
    space_sig_data <- as_space_sig_data(get_survey())
    
    trd <- list(
        Control = space_sig_data,
        ShuffleSpace = space_sig_data %>% mutate(Space_Preference = sample(Space_Preference))
    )
    
    # Pre processing options
    base_recipe <- recipe(Salary ~ ., trd$Control) %>% 
        step_center(Experience) %>% 
        step_scale(Experience)
    
    recipe <- list(
        # No preprocessing
        None = base_recipe,
        
        # Partial Least Squares
        Pls9 = base_recipe %>% step_pls(all_predictors(), outcome = "Salary", num = 9) 
    )

    control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

    # RMSE metric
    metric <- "RMSE"
    
    emc <- emc_bind(
        parameters,

        emc_record(
            train = {
                set.seed(50)
                train(
                    recipe[[preprocess]],
                    data = trd[[treatment]],
                    method = algorithm,
                    metric = metric,
                    trControl = control)
            },

            .label = label,

            .cache_path = cache_path,

            # We'll look at the messages later
            .verbose = FALSE
        ),

        # Overall training performance
        emc_performance(train),

        # And the resamples performance
        emc_resamples(train)
    )
    emc
}

## ----- Experiment Queries -----

as_space_sig_status <- function(emc) {
    select(emc, algorithm, treatment, preprocess, RMSE, errors = train_errors, warnings = train_warnings) %>%
        arrange(algorithm, treatment, preprocess)
}

as_space_sig_resamples <- function(emc) {
    emc %>%
        filter(train_errors == 0) %>%
        select(algorithm, treatment, resamples) %>%
        unnest(resamples)
}

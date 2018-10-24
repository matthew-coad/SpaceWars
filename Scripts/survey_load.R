# Load Survey --------------------------------

# Loads the survey data, sorts out the factors, does some feature engineering.
# Just loads everything into to the global environment on execution

# Note: This code was an early part of my self-study and is not currently up to my standards
#       But I want to just get this analysis done and dusted!

library(tidyverse)
library(forcats)
library(broom)
library(stringi)

whitespace.levels <- c("Tabs", "Spaces")

spacewars_data_path <- function(filename) path.expand(file.path(".", "Data", filename))
spacewars_cache_path <- function(cache_name) path.expand(file.path(".", "Cache", paste0(cache_name, ".RDS")))

memorise <- function(eval, cache_name) {
    path <- spacewars_cache_path(cache_name)
    if (file.exists(path)) {
        return (readRDS(path))
    }
    value <- eval()
    saveRDS(value, path)
    value
}

load_survey <- function() {
    col_types <- cols(
        .default = col_character(),
        Respondent = col_integer(),
        CareerSatisfaction = col_integer(),
        JobSatisfaction = col_integer(),
        HoursPerWeek = col_integer(),
        StackOverflowSatisfaction = col_integer(),
        Salary = col_double(),
        ExpectedSalary = col_double()
    )
    
    file_path <- spacewars_data_path("survey.csv")
    survey.alldata <- read_csv(file_path, col_types = col_types)
    survey.data <- survey.alldata %>% filter(!is.na(Salary) & !is.na(TabsSpaces) & TabsSpaces %in% whitespace.levels)
    survey.data
}

get_survey_data <- function() {
    memorise(load_survey, "survey_data")
}

assemble_core_data <- function(survey.data) {
    
    ## Whitespace column ---------------------------

    whitespace <- survey.data$TabsSpaces %>% factor(levels = whitespace.levels)
    preferSpace <- whitespace == "Spaces"

    # Salary column --------------------------------
    salary <- survey.data$Salary

    # Country column -------------------------------
    country <- survey.data$Country %>% fct_explicit_na() %>% fct_lump(prop = 0.002)

    # Experience column --------------------------
    yearsProgram <-
      survey.data$YearsProgram %>%
      fct_explicit_na() %>%
      fct_recode(
          "Less 1" = "(Missing)",
          "Less 1" = "Less than a year",
          "1 year" = "1 to 2 years",
          "2 years" = "2 to 3 years",
          "3 years" = "3 to 4 years",
          "4 years" = "4 to 5 years",
          "5 years" = "5 to 6 years",
          "6 years" = "6 to 7 years",
          "7 years" = "7 to 8 years",
          "8 years" = "8 to 9 years",
          "9 years" = "9 to 10 years",
          "10 years" = "10 to 11 years",
          "11 years" = "11 to 12 years",
          "12 years" = "12 to 13 years",
          "13 years" = "13 to 14 years",
          "14 years" = "14 to 15 years",
          "15 years" = "15 to 16 years",
          "16 years" = "16 to 17 years",
          "17 years" = "17 to 18 years",
          "18 years" = "18 to 19 years",
          "19 years" = "19 to 20 years",
          "20 or more" = "20 or more years"
      ) %>%
      factor(
        levels = c(
          "Less 1",
          "1 year",
          "2 years",
          "3 years",
          "4 years",
          "5 years",
          "6 years",
          "7 years",
          "8 years",
          "9 years",
          "10 years",
          "11 years",
          "12 years",
          "13 years",
          "14 years",
          "15 years",
          "16 years",
          "17 years",
          "18 years",
          "19 years",
          "20 or more"
        ), ordered = TRUE)

    # Its convenient to have the years programming experience as a numeric as long as
    # we consider that 20 years is the > 20 years category.
    experience.years = c(
      "(Missing)" = 0,
      "Less than a year" = 0,
      "1 to 2 years" = 1,
      "2 to 3 years" = 2,
      "3 to 4 years" = 3,
      "4 to 5 years" = 4,
      "5 to 6 years" = 5,
      "6 to 7 years" = 6,
      "7 to 8 years" = 7,
      "8 to 9 years" = 8,
      "9 to 10 years" = 9,
      "10 to 11 years" = 10,
      "11 to 12 years" = 11,
      "12 to 13 years" = 12,
      "13 to 14 years" = 13,
      "14 to 15 years" = 14,
      "15 to 16 years" = 15,
      "16 to 17 years" = 16,
      "17 to 18 years" = 17,
      "18 to 19 years" = 18,
      "19 to 20 years" = 19,
      "20 or more years" = 20)
    experience <-
      survey.data$YearsProgram %>%
      fct_explicit_na("(Missing)") %>%
      as.character() %>%
      map_dbl(~experience.years[.])
    
    tibble(
        Whitespace = whitespace,
        Prefer.Space = preferSpace,
        Salary = salary,
        Country = country,
        Years.Program = yearsProgram,
        Experience = experience
    )
}

quotemeta <- function(string) stringr::str_replace_all(string, "(\\W)", "\\\\\\1")
tags <- function(.Object) .Object[!is.na(.Object)] %>% stri_split_fixed(";") %>% flatten() %>% stri_trim_both() %>% unique()

tag_prefixes <- character(0)

clear_prefixes <- function() {
    tag_prefixes <<- character(0)
}

register_prefix <- function(prefix) {
    tag_prefixes <<- c(tag_prefixes, prefix)
}

get_prefixes <- function() {
    tag_prefixes
}

asTags <- function(.Object, prefix) {
    register_prefix(prefix)
    tags <- tags(.Object)
    tagValues <- function(.Object, tag) stri_detect_regex(.Object, stri_c("(^|\\s)", quotemeta(tag), "($|\\;)")) %>% Hmisc::impute(FALSE) %>% as.logical()
    names <- make.names(paste0(prefix, tags))
    columns <- tags %>% purrr::map(tagValues, .Object = .Object) %>% set_names(names)
    class(columns) <- c("tags", class(columns))
    columns
}

languagePrefix <- "Language_"
frameworkPrefix <- "Framework_"
databasePrefix <- "DB_"
platformPrefix <- "Platform_"
idePrefix <- "IDE_"

versionPrefix <- "Version_"
checkInPrefix <- "CheckIn_"
methodologyPrefix <- "Methodology_"

tech_prefixes <- c(
    languagePrefix,
    frameworkPrefix,
    databasePrefix,
    platformPrefix,
    idePrefix,
    
    versionPrefix,
    checkInPrefix,
    methodologyPrefix
)

if (!exists("tech_colnames")) {
    tech_colnames <- NULL
}

get_tech_colnames <- function() {
    tech_colnames
}

assemble_tech_data <- function(survey_data) {
    
    #The technologies columns are composed of 5 columns, each being a semi-column delimilted list of "tags" that the
    #indicate the respondent was worked with that technology
    
    #We need it tidy!
    
    #Expand the list of tech tags into a series of seperate boolean columns that is true if they have used that tag
    
    languageExperience <- asTags(survey_data$HaveWorkedLanguage, languagePrefix)
    frameworkExperience <- asTags(survey_data$HaveWorkedFramework, frameworkPrefix)
    databaseExperience <- asTags(survey_data$HaveWorkedDatabase, databasePrefix)
    platformExperience <- asTags(survey_data$HaveWorkedPlatform, platformPrefix)
    ideExperience <- asTags(survey_data$IDE, idePrefix)
    
    versionExperience <- asTags(survey_data$VersionControl, versionPrefix)
    methodologyExperience <- asTags(survey_data$Methodology, methodologyPrefix)
    
    tech.columns <- list(
        languageExperience,
        frameworkExperience,
        databaseExperience,
        platformExperience,
        ideExperience,
        versionExperience
    ) %>% flatten()
    
    tech.colnames <- names(tech.columns)
    tech.colnames[which(tech.colnames == "Language_C..")] = "Language_Cpp"
    tech.colnames[which(tech.colnames == "Language_C.")] = "Language_Csharp"
    tech.colnames[which(tech.colnames == "Language_F.")] = "Language_Fsharp"
    tech_colnames <<- tech.colnames
    names(tech.columns) <- tech.colnames
    
    tech.combined <- bind_cols(tech.columns)
    tech.combined
}

assemble_methodology_data <- function(survey_data) {
    
    methodologyExperience <- asTags(survey_data$Methodology, methodologyPrefix)
    
    methodology.columns <- list(
        methodologyExperience
    ) %>% flatten()
    
    methodology.combined <- bind_cols(!!! methodologyExperience)
    methodology.combined
}


assemble_tech_families <- function(survey_data, tech_data) {
    #Technologies tend to get used together. Use kmeans to group technologies into "families"
    
    techFamilies.count <- 5
    set.seed(7)
    tech.kmeans <- kmeans(tech_data, techFamilies.count, nstart = 20)
    
    techFamily <- 
        tech.kmeans$cluster %>%
        as.character() %>%
        as.factor()
    
    # But what do the tech clusters mean? **
    
    # For each tech family, determine how often each technology is indicated.
    # Allocate each tech to the family that appears the most in
    # For each family order the techs allocated to it by the number of times it appears in total.
    # Then name each family by the 4 techs that appear the most.
    
    #The implementation could probably be better!
    
    tech.stats <- tech_data %>% gather(key = tech, value = used, Language_Java:IDE_Komodo) %>% group_by(tech) %>% summarise(allcount = sum(used))
    tech.familystats <-
        tech_data %>%
        mutate(TechFamily = techFamily) %>%
        gather(key = tech, value = used, Language_Java:IDE_Komodo) %>%
        group_by(TechFamily, tech) %>%
        summarise(count = sum(used)) %>%
        inner_join(tech.stats, by = "tech") %>%
        mutate(familyRatio = count / allcount) %>%
        mutate(ratioRank = min_rank(familyRatio), countRank = min_rank(count), allRank = min_rank(allcount))
    
    tech.family <- function(.tech) 
        tech.familystats %>% 
        filter(tech == .tech) %>% 
        arrange(desc(familyRatio)) %>% 
        head(1) %>% 
        select(TechFamily, tech, allcount)
    
    tech_colnames <- get_tech_colnames()
    tech.families <- tech_colnames %>% map_df(tech.family) %>% bind_rows()
    tech.familyname <- function(.techFamily) 
        tech.families %>% 
        filter(TechFamily == .techFamily) %>% 
        arrange(desc(allcount)) %>% 
        head(3) %>% 
        .$tech %>% 
        map_chr(~stri_split_fixed(., "_")[[1]][2]) %>% 
        paste(., collapse = ", ")
    
    
    levels(techFamily) <- levels(techFamily) %>% map_chr(tech.familyname)
    
    techFamilyPrefix <- "TechFamily_"
    techFamilies <- asTags(techFamily, techFamilyPrefix)
    
    #Freak me out it works! Being an analyst and a domain expert is neat. Which was the point.
    #When we use 5 clusters we more or less get

    # JavaScript, MySQL, Vim, Sublime.Text  - Opensource web developers\linux heads
    # SQL, Visual.Studio, Csharp, SQL.Server  - Microsoft flock
    # Python, Cpp, C, PyCharm  - Hardcore low-level devs
    # Android, SQLite, iOS, Android.Studio Java, - Mobile/Apple knights
    # IntelliJ, Eclipse, Oracle - Java faithful
    
    bind_cols(
        list(
            Tech.Family =techFamily
        ),
        !!!techFamilies
    )
}

countryPrefix <- "Country_"
companySizePrefix <- "CompanySize_"
formalEducationPrefix <- "FormalEducation_"
majorUndergradPrefix <- "MajorUndergrad_"
companyTypePrefix <- "CompanyType_"
developerTypePrefix <- "DevType_"
educationTypePrefix <- "EducationType_"
selfTaughtPrefix <- "SelfTaughtType_"
genderPrefix <- "Gender_"
racePrefix <- "Race_"


# The kitchen sink -----------------------------------

assembly_kitchen_sink_data <- function(survey.data, core_data) {
    
    countries <- asTags(core_data$Country, countryPrefix)

    companySize <- survey.data$CompanySize %>% fct_explicit_na() %>% as.factor()
    companySizes <- asTags(survey.data$CompanySize, companySizePrefix)
    
    formalEducation <- survey.data$FormalEducation %>% fct_explicit_na() %>% as.factor()
    formalEducations <- asTags(survey.data$FormalEducation, formalEducationPrefix)
    
    MajorUndergrad <- survey.data$MajorUndergrad %>% fct_explicit_na() %>% as.factor()
    majorUndergrads <- asTags(survey.data$MajorUndergrad, majorUndergradPrefix)
    
    University <- survey.data$University %>% fct_explicit_na() %>% as.factor()
    EmploymentStatus <- survey.data$EmploymentStatus %>% fct_explicit_na() %>% as.factor()
    Professional <- survey.data$Professional %>% fct_explicit_na() %>% as.factor()
    HomeRemote <- survey.data$HomeRemote %>% fct_explicit_na() %>% as.factor()
    CheckInCode <- survey.data$CheckInCode %>% fct_explicit_na() %>% as.factor()
    
    CompanyType <- survey.data$CompanyType %>% fct_explicit_na() %>% as.factor()
    companyType <- asTags(survey.data$CompanyType, companyTypePrefix)
    
    developerType <- asTags(survey.data$DeveloperType, developerTypePrefix)
    
    #Hours per week could be useful, but the data is a disaster. Forget about it.
    #Desperate times! Assume hours per week is 40 for missing records
    HoursPerWeek <- survey.data$HoursPerWeek
    HoursPerWeek[is.na(HoursPerWeek)] = 40
    
    educationTypes <- asTags(survey.data$EducationTypes, educationTypePrefix)
    
    selfTaughtTypes <- asTags(survey.data$SelfTaughtTypes, selfTaughtPrefix)
    
    genders <- asTags(survey.data$Gender, genderPrefix)
    
    HighestEducationParents <- survey.data$HighestEducationParents %>% fct_explicit_na() %>% as.factor()
    
    races <- asTags(survey.data$Race, racePrefix)
    
    bind_cols(
        !!! countries,
        !!! companySizes,
        !!! formalEducations,
        !!! majorUndergrads,
        list(
            University =University, 
            Employment.Status = EmploymentStatus,
            Professional = Professional,
            Home.Remote = HomeRemote,
            Check.In.Code = CheckInCode
        ),
        !!! companyType,
        !!! developerType,
        list(Hours.Per.Week = HoursPerWeek),
        !!! educationTypes,
        !!! selfTaughtTypes,
        !!! genders,
        list(Highest.Education.Parents = HighestEducationParents),
        !!! races
    )

}


# Assemble Survey --------------------------------

# Cleans the data and assemblies everything into a single dataset

defaultSalaryMaxSD = 3
defaultOutliers = FALSE
standardCountry <- "United Kingdom"

# Radically different salaries between countries makes things ackward.
# Strip outliers and then use whats left over to scale the salary to a normalised distribution

survey_clean_country <- function(df, salaryMaxSD = defaultSalaryMaxSD) {
    
    salary <- df$Salary
    yearsProgram <- df$Years.Program
    medianSalary <- median(salary)
    sdSalary <- sd(salary)
    minSalary <- medianSalary - (salaryMaxSD * sdSalary)
    maxSalary <- medianSalary + (salaryMaxSD * sdSalary)
    outliers <- salary < minSalary | salary > maxSalary
    
    dfcMedianSalary <- median(df$Salary[!outliers])
    dfcSdSalary <- sd(df$Salary[!outliers])
    normalSalary <- (salary - dfcMedianSalary) / dfcSdSalary
    
    df$Outlier <- outliers
    df$Normal.Salary <- normalSalary
    df
}

# Assembly all the source data into one data frame.
# Set outliers to TRUE to include outliers and set the salaryMaxSD to change from
# the default SD multiplier used to identify outliers.
survey_assemble <- function(outliers = defaultOutliers, salaryMaxSD = defaultSalaryMaxSD) {

    clear_prefixes()    
    survey_data <- get_survey_data()
    core_data <- assemble_core_data(survey_data)
    tech_data <- assemble_tech_data(survey_data)
    tech_family_data <- assemble_tech_families(survey_data, tech_data)
    methodology_data <- assemble_methodology_data(survey_data)
    kitchen_sink_data <- assembly_kitchen_sink_data(survey_data, core_data)
    
    df <- bind_cols(
        core_data,
        tech_data,
        tech_family_data,
        kitchen_sink_data
    )
    df$Country <- df$Country %>% fct_reorder(df$Salary)
    df <- df %>% split(.$Country) %>% map(~survey_clean_country(., salaryMaxSD)) %>% bind_rows()
    
    standardSalaries <- df %>% filter(Country == standardCountry & !Outlier) %>% .$Salary
    df$Standard.Salary <- df$Normal.Salary * sd(standardSalaries) + median(standardSalaries)
    
    df$Outlier <- df$Outlier | df$Standard.Salary < 0
    
    df <- df %>% select(Salary, Normal.Salary, Standard.Salary, Whitespace, Outlier, Country, everything())
    df <- if (outliers) df else df %>% filter(!Outlier)
    
    df <- df %>% mutate_if(is_logical, as.integer)
    
    df
}

get_survey <- function() {
    memorise(survey_assemble, "survey")
}



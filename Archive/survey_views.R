
# Views ---------------------------------------

# This is not actually a runnable script that produces something
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")

#Tabs vs Spaces - The mystery --------------------------------------------------

# Prequisite
df <- survey_assemble() %>% salaryVsIndentation_augment()

# Print Tabs vs spaces. Actual Salaries

dollarFormat <- scales::dollar_format(largest_with_cents = 0)
spaceSalary <- df %>% filter(Whitespace == "Spaces") %>% .$Salary %>% median()
spaceCount <- df %>% filter(Whitespace == "Spaces") %>% nrow()
tabsSalary <- df %>% filter(Whitespace == "Tabs") %>% .$Salary %>% median()
tabsCount <- df %>% filter(Whitespace == "Tabs") %>% nrow()
cat("Spaces salary: ", dollarFormat(spaceSalary), " count: ", spaceCount, "\n")
cat("Tabs salary: ", dollarFormat(tabsSalary), " count: ", tabsCount, "\n")

df %>% ggplot(aes(x = Whitespace, y = Standard.Salary)) + geom_boxplot()


# Print Tabs vs spaces. Standard Salaries

dollarFormat <- scales::dollar_format(largest_with_cents = 0)
spaceSalary <- df %>% filter(Whitespace == "Spaces") %>% .$Standard.Salary %>% median()
spaceCount <- df %>% filter(Whitespace == "Spaces") %>% nrow()
tabsSalary <- df %>% filter(Whitespace == "Tabs") %>% .$Standard.Salary %>% median()
tabsCount <- df %>% filter(Whitespace == "Tabs") %>% nrow()
cat("Spaces salary: ", dollarFormat(spaceSalary), " count: ", spaceCount, "\n")
cat("Tabs salary: ", dollarFormat(tabsSalary), " count: ", tabsCount, "\n")

df %>% ggplot(aes(x = Whitespace, y = Standard.Salary)) + geom_boxplot()

#Tabs vs Spaces - The real deal --------------------------------------------------

# Print residual spaces vs tabs after removing other considerations

dollarFormat <- scales::dollar_format(largest_with_cents = 0)
spaceSalary <- df %>% filter(Whitespace.Confusion == "Spaces") %>% .$Standard.Salary.Residual %>% mean()
spaceCount <- df %>% filter(Whitespace.Confusion == "Spaces") %>% nrow()
tabsSalary <- df %>% filter(Whitespace.Confusion == "Tabs") %>% .$Standard.Salary.Residual %>% mean()
tabsCount <- df %>% filter(Whitespace.Confusion == "Tabs") %>% nrow()
cat("Spaces salary: ", dollarFormat(spaceSalary), " count: ", spaceCount, "\n")
cat("Tabs salary: ", dollarFormat(tabsSalary), " count: ", tabsCount, "\n")

df %>% ggplot(aes(x = Whitespace.Confusion, y = Standard.Salary.Residual)) + geom_boxplot()

# Salary - Variable importance --------------------------------------------------

# The full set of variables

#[1] "Experience" "CompanySize_10.000.or.more.employees" "CompanySize_5.000.to.9.999.employees"
#[4] "CompanySize_1.000.to.4.999.employees" "CompanySize_10.to.19.employees" "CompanySize_500.to.999.employees"
#[7] "CompanySize_100.to.499.employees" "CompanySize_20.to.99.employees" "CompanySize_Fewer.than.10.employees"
#[10] "CompanySize_I.don.t.know" "CompanySize_I.prefer.not.to.answer" "CompanyType_Publicly.traded.corporation"
#[13] "CompanyType_Privately.held.limited.company..not.in.startup.mode" "CompanyType_Sole.proprietorship.or.partnership..not.in.startup.mode" "CompanyType_I.don.t.know"
#[16] "CompanyType_Venture.funded.startup" "CompanyType_Government.agency.or.public.school.university" "CompanyType_Pre.series.A.startup"
#[19] "CompanyType_Non.profit.non.governmental.organization.or.private.school.university" "CompanyType_Something.else" "CompanyType_State.owned.company"
#[22] "CompanyType_I.prefer.not.to.answer" "DB_MySQL" "DB_PostgreSQL"
#[25] "DB_Oracle" "DB_SQL.Server" "DB_Redis"
#[28] "DB_MongoDB" "DB_SQLite" "DB_Cassandra"
#[31] "DevType_Other" "DevType_Embedded.applications.devices.developer" "DevType_Web.developer"
#[34] "DevType_DevOps.specialist" "DevType_Quality.assurance.engineer" "DevType_Mobile.developer"
##[37] "DevType_Desktop.applications.developer" "DevType_Graphics.programming" "DevType_Developer.with.a.statistics.or.mathematics.background"
#[40] "DevType_Systems.administrator" "DevType_Data.scientist" "DevType_Graphic.designer"
#[43] "DevType_Database.administrator" "DevType_Machine.learning.specialist" "EducationType_Self.taught"
#[46] "EducationType_Coding.competition" "EducationType_Hackathon" "EducationType_Open.source.contributions"
#[49] "EducationType_On.the.job.training" "EducationType_Part.time.evening.course" "EducationType_Online.course"
#[52] "EducationType_Bootcamp" "EducationType_Industry.certification" "FormalEducation_Bachelor.s.degree"
#[55] "FormalEducation_Professional.degree" "FormalEducation_Master.s.degree" "FormalEducation_Some.college.university.study.without.earning.a.bachelor.s.degree"
#[58] "FormalEducation_Doctoral.degree" "FormalEducation_Secondary.school" "FormalEducation_I.never.completed.any.formal.education"
#[61] "FormalEducation_Primary.elementary.school" "FormalEducation_I.prefer.not.to.answer" "Framework_React"
#[64] "Framework_Spark" "Framework_AngularJS" "Framework_Node.js"
#[67] "Framework_Cordova" "Framework_.NET.Core" "Framework_Xamarin"
#[70] "Framework_Hadoop" "Framework_Firebase" "Gender_Male"
#[73] "Gender_Female" "Gender_Other" "Gender_Transgender"
#[76] "Gender_Gender.non.conforming" "IDE_Sublime.Text" "IDE_Vim"
#[79] "IDE_IntelliJ" "IDE_Emacs" "IDE_Xcode"
#[82] "IDE_Notepad.." "IDE_Visual.Studio" "IDE_Android.Studio"
#[85] "IDE_Eclipse" "IDE_PHPStorm" "IDE_Visual.Studio.Code"
#[88] "IDE_Atom" "IDE_RubyMine" "IDE_PyCharm"
#[91] "IDE_NetBeans" "IDE_IPython...Jupyter" "IDE_Coda"
#[94] "IDE_RStudio" "IDE_TextMate" "IDE_Light.Table"
#[97] "IDE_Zend" "IDE_Komodo" "Language_Java"
#[100] "Language_PHP" "Language_Python" "Language_Assembly"
#[103] "Language_C" "Language_C.." "Language_Clojure"
#[106] "Language_JavaScript" "Language_Matlab" "Language_Rust"
#[109] "Language_SQL" "Language_Swift" "Language_Scala"
#[112] "Language_C." "Language_F." "Language_Ruby"
#[115] "Language_Go" "Language_CoffeeScript" "Language_Perl"
#[118] "Language_TypeScript" "Language_Smalltalk" "Language_Objective.C"
#[121] "Language_VB.NET" "Language_Visual.Basic.6" "Language_Haskell"
#[124] "Language_VBA" "Language_Elixir" "Language_Groovy"
#[127] "Language_Dart" "Language_R" "Language_Erlang"
#[130] "Language_Lua" "Language_Julia" "Language_Hack"
#[133] "Language_Common.Lisp" "MajorUndergrad_Computer.science.or.software.engineering" "MajorUndergrad_Computer.engineering.or.electrical.electronics.engineering"
#[136] "MajorUndergrad_Computer.programming.or.Web.development" "MajorUndergrad_Information.technology..networking..or.system.administration" "MajorUndergrad_Mathematics.or.statistics"
#[139] "MajorUndergrad_Management.information.systems" "MajorUndergrad_Something.else" "MajorUndergrad_A.business.discipline"
#[142] "MajorUndergrad_A.social.science" "MajorUndergrad_A.humanities.discipline" "MajorUndergrad_A.non.computer.focused.engineering.discipline"
#[145] "MajorUndergrad_A.natural.science" "MajorUndergrad_Psychology" "MajorUndergrad_I.never.declared.a.major"
#[148] "MajorUndergrad_Fine.arts.or.performing.arts" "MajorUndergrad_A.health.science" "Platform_Mac.OS"
#[151] "Platform_Linux.Desktop" "Platform_Amazon.Web.Services..AWS." "Platform_Windows.Desktop"
#[154] "Platform_Microsoft.Azure" "Platform_Android" "Platform_Raspberry.Pi"
#[157] "Platform_Mainframe" "Platform_iOS" "Platform_Arduino"
#[160] "Platform_Serverless" "Platform_SharePoint" "Platform_Windows.Phone"
#[163] "Platform_WordPress" "Platform_Salesforce" "Race_White.or.of.European.descent"
#[166] "Race_Native.American..Pacific.Islander..or.Indigenous.Australian" "Race_Hispanic.or.Latino.Latina" "Race_Middle.Eastern"
#[169] "Race_I.don.t.know" "Race_South.Asian" "Race_I.prefer.not.to.say"
#[172] "Race_East.Asian" "Race_Black.or.of.African.descent" "SelfTaughtType_Official.documentation"
#[175] "SelfTaughtType_Trade.book" "SelfTaughtType_Textbook" "SelfTaughtType_Stack.Overflow.Q.A"
#[178] "SelfTaughtType_Non.Stack.online.communities" "SelfTaughtType_Company.internal.community" "SelfTaughtType_Friends.network"
#[181] "SelfTaughtType_Built.in.help" "SelfTaughtType_Other" "SelfTaughtType_Stack.Overflow.Docs"
#[184] "SelfTaughtType_Tutoring.mentoring"

# Variables by salary importance ----------------------

# ** Large Effect **

# Country
# IDE
# Language
# DB
# EducationType
# MajorUndergrad
# Experience
# Version_

# ** Minor Effect **

# Race_
# Platform_
# DevType_
# CompanyType_
# CompanySize_
# Framework_

# Gender_
# FormalEducation_
# Hours.Per.Week

# ** No Effect **

# FormalEducation_
# University
# Employment.Status
# Methodology_
# Home.Remote
# Highest.Education.Parents

salaryPredictors <- salary_predictors()
fullIndentationPredictors <- indentation_predictors()
#df <- survey_assemble() %>% filter(Country == "United States")
df <- survey_assemble()
dff <- df %>% salaryVsIndentation_augment(salaryPredictors, fullIndentationPredictors)

noEffectPredictors <- c("IDE")
subsetPredictors <- survey_predictors(discardPrefix = noEffectPredictors)
dfs <- df %>% salaryVsIndentation_augment(salaryPredictors, subsetPredictors)
dff %>%
    ggplot() +
    geom_density(aes(x = Space.Preference.Logodds)) +
    geom_density(data = dfs, aes(x = Space.Preference.Logodds), linetype = 3)


# Salary - Variable importance --------------------------------------------------

# Experience
# CompanySize
# CompanyType
# DevType
# MajorUndergrad - Something in the middle
# EducationType
# FormalEducation

# Platform - Important
# Language - Important
# IDE - Small impact
# Methodology

# ** Little importance

# Employment.Status
# DB
# Framework
# University

# ** Unimportant variables 

# Race no effect
# Gender - No effect
# SelfTaughtType has no effect
# Highest.Education.Parents
# Home.Remote
# Version

fullPredictors <- salaryVsIndentation_predictors("Country")
subsetPredictors <- salaryVsIndentation_predictors(c("Country", "SelfTaughtType", "Race", "Gender", "University", "Version_"))
supersetPredictors <- c(subsetPredictors, additionalPredictors)
df <- survey_assemble() %>% filter(Country == "United States")
dff <- df %>% salaryVsIndentation_augment(fullPredictors)
dfs <- df %>% salaryVsIndentation_augment(subsetPredictors)
dff %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Whitespace), linetype = 2) +
    geom_density(aes(x = Standard.Salary.Predicted, color = Whitespace)) +
    geom_density(data = dfs, aes(x = Standard.Salary.Predicted, color = Whitespace), linetype = 3)

# Salary - Fit vs predicted --------------------------------------------------

# salaryVsIndentation_augment_glmnet <- function(df, predictors, alpha, s, seed)

fullPredictors <- salaryVsIndentation_predictors()
df <- survey_assemble()
dff <- df %>% salaryVsIndentation_augment_glmnet(fullPredictors, 0, "lambda.1se", 7)
dff %>%
    ggplot() +
    geom_density(aes(x = Standard.Salary, color = Prefer.Space), linetype = 2) +
    geom_density(aes(x = Standard.Salary.Predicted, color = Prefer.Space)) +
    geom_density(aes(x = Standard.Salary.Predicted, color = Space.Preference.Predicted), linetype = 3)


# Conclusion. The skewed salary distributions

# Repeat the sal

# Plot density of salary vs predictions should show it.

# company. An investigation ight reveal cultural preference.

# Profressional vs Amatuer vs part time analysis.

# Working efficiency vs sharing

# Tools

## Experience vs space preference



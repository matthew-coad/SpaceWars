# Background Views ---------------------------------------

# This is not actually a directly runnable script .
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")

# Experience ---------------------------------------

#Prequisties for this section

# Highest Education Parents.
# This plot is used to demonstate the models ability to focus on effects

salaryVsIndentationGroupEffect_analysis("Highest.Education.Parents", "Highest.Education.Parents", "Parents education", 500)

beepr::beep()

# EducationType

salaryVsIndentationTagEffect_analysis("EducationType_", "EducationType", "Education type", 500)

beepr::beep()

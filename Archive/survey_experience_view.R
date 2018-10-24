# Experience Views ---------------------------------------

# This is not actually a directly runnable script .
# Its a bunch of script fragments that plots various stuff that I invoke immediately to test/explore etc.

source("survey_load.R")
source("survey_assemble.R")
source("survey_plots.R")

# Experience ---------------------------------------

salaryVsIndentationGroupEffect_analysis("Experience", "Years.Program", "Programming experience")

beepr::beep()


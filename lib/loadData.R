# Description: This code is for loading data and hard coded variables. 
# Author: Piyali Das
# Modified By: Piyali Das
# Date Created: Oct 07, 2017
# Date Modified: Sep 10, 2018
# 4 oct 2018 - jp: moved data processes to WriteData script so dashboard only has to load an .Rdata file

load(file = "data/OMAS_data.Rdata")

barColors <- c("#700D1C","#BEC0C4","#2A537F","#FFD700","#B22222","#FFE4E1")

options(survey.lonely.psu="certainty")

multi100 <- function(x){ return(x*100) }

# years surveyed
years <- c("2004", "2008", "2010", "2012", "2015", "2017")


# indicators based on year
# PD note: 1. Check for Other/Private insurance 2. many indicators not present for 2004  3. Check for Demographics of county fips
#superset of indicators
adult_indicators <- list("Insurance Status" = c("Employer-Sponsored" = "ins_esi",
                                                "Private" = "ins_private",
                                                "Medicaid" = "ins_medicaid",
                                                "Medicare, no Medicaid" = "ins_medicareonly",
                                                "Exchange Coverage" = "ins_exchange",
                                                "Uninsured" = "ins_uninsured"), 
                         "Health Status" = c("Poor/Fair Overall Health" = "hs_selfrated", 
                                             "Obese" = "hs_obese", 
                                             "Diabetes" = "hs_diabetes", 
                                             "Injured Due to a Fall" = "hs_injuredfall", 
                                             "Among Those who Fell, Injury Resulted in Healthcare Visit" = "hs_fallcare", 
                                             "14 or More Mentally Distressed Days" = "hs_mentaldistress"), 
                         "Health Behavior" = c("Smoking" = "hb_smoking", 
                                               "Alcohol Drinking" = "hb_alcohol", 
                                               "Binge Drinking" = "hb_binge", 
                                               "Misuse Prescription Pain Medicine" = "hb_painmed"), 
                         "Unmet Needs" = c("Dental Care" = "unmet_dental", 
                                           "Vision Care" = "unmet_vision", 
                                           "Mental Health Care" = "unmet_mental", 
                                           "Prescription Medication" = "unmet_rx", 
                                           "Other Unmet Healthcare Needs" = "unmet_others"), 
                         "Access to Care" = c("Usual Source of Care" = "ac_usualsource", 
                                              "Harder to Secure than 3 Years Ago" = "ac_hardersecure"),
                         "Demographics" = c("Race-Ethnicity" = "dem_raceeth", 
                                            "Gender" = "dem_gender", 
                                            "Income Group" = "dem_income", 
                                            "Marital Status" = "dem_marital", 
                                            "Employment Status" = "dem_employment", 
                                            "Family Composition" = "dem_famtype", 
                                            "Education Level" = "dem_educ"))

child_indicators <- list("Insurance Status" = c("Child Employer-Sponsored" = "ins_esi",
                                                "Medicaid" = "ins_medicaid",
                                                "Uninsured" = "ins_uninsured",
                                                "Other" = "ins_other"), 
                         "Health Status" = c("Poor/Fair Overall Health" = "hs_selfrated", 
                                             "Obese" = "hs_obese"), 
                         "Health Behavior" = c("Had Fruit Juice Yesterday" = "hb_juice",
                                               "Had Soda Yesterday" = "hb_soda"), 
                         "Unmet Needs" = c("Dental Care" = "unmet_dental", 
                                           "Vision Care" = "unmet_vision",
                                           "Prescription Medication" = "unmet_rx"), 
                         "Access to Care" = c("Usual Source of Care" = "ac_usualsource"),
                         "Demographics" = c("Race-Ethnicity" = "dem_raceeth", 
                                            "Gender" = "dem_gender", 
                                            "Income Group" = "dem_income"))

# list of demographic variables
dems_list <- c("dem_raceeth", "dem_educ", "dem_famtype", "dem_income", "dem_gender", "dem_marital", "dem_employment")

# PD Note: Subpops are added using filter variables 
adult_subpops <-  c("All"="none", 
              "Males"="subpop_male", 
              "Females"="subpop_female", 
              "Special Health Care Needs"="filter_specialhealth", 
              "Adults with a disability"="filter_dd")

child_subpops <-  c("All" = "none",
                    "Males"="subpop_male", 
                    "Females"="subpop_female", 
                    "Special Health Care Needs"="filter_specialhealth",
                    "Disability"="filter_disability")

# county grouping inputs
countyGroups <- c("ADAMH", "OEI", "AAA")

# age ranges
adult_agesLower <- c("19", "25", "35", "45", "55", "65", "75")
adult_agesUpper <- c("24", "34", "44", "54", "64", "74", "75+")
child_agesLower <- c("0", "7", "13")
child_agesUpper <- c("6", "12", "17")

# stratification groups
stratasAll <- c("None"="none", "Gender"="dem_gender", 
                "Race-Ethnicity"="dem_raceeth", "County Type"="dem_countytype", 
                "ADAMH"="geo_adamh", "OEI/Non-OEI"="geo_oei", 
                "AAA" = "geo_aaa")
stratasNoGender <- c("None"="none", "Race-Ethnicity"="dem_raceeth", 
                     "County Type"="dem_countytype",
                     "ADAMH"="geo_adamh", "OEI/Non-OEI"="geo_oei", 
                     "AAA" = "geo_aaa")
geo_strata <- c("geo_aaa", "geo_adamh", "geo_oei", "dem_countytype")

# Creating dataframes to extrcat labels of stratifier
strataAllframe <- cbind(stratas=rownames(data.frame(stratasAll)) , data.frame(stratasAll))

# federal poverty lines
fplGroupsLower <- c("0%" = 0, "101%" = 101, "151%" = 151, "201%" = 201, "301%" = 301)
fplGroupsUpper <- c("100%" = 100, "150%" = 150, "200%" = 200, "300%" = 300, ">301%" = 501)

# fplGroupsLower <- c("0%" , "101%", "151%" , "201%" , "301%" )
# fplGroupsUpper <- c("100%" , "150%" , "200%", "300%", ">301%" )

### customized conditional dplyr functions 
conditionally <- function(fun){
  function(first_arg, ..., execute){
    if(execute) return(fun(first_arg, ...))
    else return(first_arg)
  }
}
cond_filter <- conditionally(filter)
cond_select <- conditionally(select)
cond_group_by <- conditionally(group_by)
cond_rename <- conditionally(rename)
cond_mutate <- conditionally(mutate)


##order lists for factors 
list_dem_income <- list( categoryorder = "array",
                         categoryarray = c("100% FPL or less",
                                           "101%-150% FPL",                                           
                                           "151%-200% FPL",
                                           "201%-300% FPL",
                                           "301% FPL or more"))
list_dem_marital <- list(categoryorder = "array",
                         categoryarray = c("Never Married",
                                           "Unmarried Couple",
                                           "Married",
                                           "Divorced or Separated"))
list_dem_educ <- list(categoryorder = "array",
                      categoryarray = c())
list_dem_employment <- list(categoryorder = "array",
                            categoryarray = c())
list_strat_insurance <- list(categoryorder = "array",
                             categoryarray = c())

### for instances where all estimates at the county level are suppressed
data.frame(geo_countyname = as.character(ohio$NAME)) %>% mutate(
  label_1 = paste0("<u><b>",geo_countyname, "</b></u>"),
  label_2 = paste("<i>Percent</i>: Insufficent Data"),
  label_3 = paste("<i>Count</i>: Insufficent Data"),
  indicator = "Insufficient Data") %>%
  mutate(label_full = paste(label_1, label_2, label_3, sep = "<br/>")) %>%
  mutate(label_html = lapply(label_full, function(x) HTML(x)))  -> empty_ohio


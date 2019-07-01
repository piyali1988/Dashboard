#### write data
## initial 4 oct 2018 papi04
## latest 29 apr 2019 papi04 via odde01

#.libPaths( c( .libPaths(), "U:\\papi04\\Rlibs") )

library(tidyverse)
library(readr)
library(geojsonio)
library(forcats)
library(magrittr)

### write all data files used by OMAS dashboard into a single .Rdata file

#load in all child data
# data_child_2004 <- read_csv("../data/child_2004.csv")
# data_child_2008 <- read_csv("../data/child_2008.csv")
data_child_2010 <- read_csv("../data/child_2010.csv")
data_child_2012 <- read_csv("../data/child_2012.csv")
data_child_2015 <- read_csv("../data/child_2015.csv")
data_child_2017 <- read_csv("../data/child_2017.csv")

#load in all adult data
# data_adult_2004 <- read_csv("../data/adult_2004.csv")
# data_adult_2008 <- read_csv("../data/adult_2008.csv")
data_adult_2010 <- read_csv("../data/adult_2010.csv")
data_adult_2012 <- read_csv("../data/adult_2012.csv")
data_adult_2015 <- read_csv("../data/adult_2015.csv")
data_adult_2017 <- read_csv("../data/adult_2017.csv")

#load utility datasets needed
poptext <- read_csv("data/helpPopUp.csv")

## ohio county shape files
ohio <- geojson_read("data/OH-39-ohio-counties.json", what="sp")

## geo_strat shape fipes
load("data/geo_strat_shapes.Rdata")

## ohio county names/locations
oh_cent <- read.csv("data/ohio_county_centroids.csv", stringsAsFactors = FALSE)[-1,] %>%
  rename("county" = "County..2.")

## labels
OMAS_labels <- read_csv("data/dashboardText.csv")

# data_adult_2004 %>%
#   mutate(year = 2004,
#          masterid = masterid %>% as.character()) -> data_adult_2004_y
# 
# data_adult_2008 %>%
#   mutate(year = 2008,
#          masterid = masterid %>% as.character()) -> data_adult_2008_y

data_adult_2010 %>%
  mutate(year = 2010,
         masterid = masterid %>% as.character()) -> data_adult_2010_y

data_adult_2012 %>%
  mutate(year = 2012,
         masterid = masterid %>% as.character()) -> data_adult_2012_y

data_adult_2015 %>%
  mutate(year = 2015,
         masterid = masterid %>% as.character()) -> data_adult_2015_y

data_adult_2017 %>%
  mutate(year = 2017,
         masterid = masterid %>% as.character()) -> data_adult_2017_y

data_adult_full <- bind_rows(#data_adult_2004_y,
                             #data_adult_2008_y,
                             data_adult_2010_y, 
                             data_adult_2012_y, 
                             data_adult_2015_y,
                             data_adult_2017_y) %>%
  mutate(filter_age1 = case_when(filter_age == "19-24" ~ 21.5,
                                 filter_age == "25-34" ~ 29.5,
                                 filter_age == "35-44" ~ 39.5,
                                 filter_age == "45-54" ~ 49.5,
                                 filter_age == "55-64" ~ 59.5,
                                 filter_age == "65-74" ~ 69.5,
                                 filter_age == "75+" ~ 75.9,
                                 filter_age == "998" | filter_age == "999" ~ NA_real_)) %>%
  mutate(dem_income1 = case_when(dem_income == "100% FPL or less" ~ 50,
                                 dem_income == "100-138% FPL" | dem_income == "101%-138% FPL" ~ 116,
                                 dem_income == "139-206% FPL" | dem_income == "139%-206% FPL" ~ 172,
                                 dem_income == "207-300% FPL" | dem_income == "207%-300% FPL" ~ 254,
                                 dem_income == "301% FPL or more" ~ 300.5),
         geo_adamh = geo_adamh %>% as.character() %>% trimws(),
         geo_aaa = geo_aaa %>% as.character() %>% trimws(),
         geo_oei = geo_oei %>% as.character() %>% trimws()) %>%
  mutate(dem_countytype = recode_factor(dem_countytype,
                                        "Rural Non-Appalachian" = "Rural Non-Appalachian",
                                        "Appalachian" = "Appalachian",
                                        "Suburban" =  "Suburban",
                                        "Metropolitan" =  "Metropolitan",
                                        .ordered = TRUE) %>% as.character() %>% trimws() %>% as.factor()) %>%
  mutate( dem_employment = dem_employment %>% as.character(),
          dem_employment = ifelse(dem_employment == "DK/RF"| is.na(dem_employment), "Unknown", dem_employment),
          dem_marital = dem_marital %>% as.character(),
          dem_marital = ifelse(dem_marital == "DK/RF" | is.na(dem_marital), "Unknown", dem_marital),
          dem_marital = ifelse(dem_marital == "Divorced/Separated", "Divorced or Separated", dem_marital),
          dem_raceeth = dem_raceeth %>% as.character(),
          dem_raceeth = ifelse(dem_raceeth == "White/Other", "White or Other", dem_raceeth)) #%>%
  # mutate(geo_aaa = fct_explicit_na(geo_aaa, na_level =  NA_character_),
  #        geo_adamh = fct_explicit_na(geo_adamh, na_level =  NA_character_),
  #        geo_oei = fct_explicit_na(geo_oei, na_level =  NA_character_),
  #        dem_countytype = fct_explicit_na(dem_countytype, na_level =  NA_character_),
  #        dem_countyfips = fct_explicit_na(dem_countyfips, na_level =  NA_character_))

rm(#data_adult_2004_y,
   #data_adult_2008_y, 
   data_adult_2010_y, 
   data_adult_2012_y, 
   data_adult_2015_y,
   data_adult_2017_y)

rm(#data_adult_2004,
   #data_adult_2008, 
   data_adult_2010, 
   data_adult_2012, 
   data_adult_2015,
   data_adult_2017)

# data_child_2004 %>%
#   mutate(year = 2004,
#          masterid = masterid %>% as.character()) -> data_child_2004_y
# 
# data_child_2008 %>%
#   mutate(year = 2008,
#          masterid = masterid %>% as.character()) -> data_child_2008_y

data_child_2010 %>%
  mutate(year = 2010,
         masterid = masterid %>% as.character()) -> data_child_2010_y

data_child_2012 %>%
  mutate(year = 2012,
         masterid = masterid %>% as.character()) -> data_child_2012_y

data_child_2015 %>%
  mutate(year = 2015,
         masterid = masterid %>% as.character()) -> data_child_2015_y

data_child_2017 %>%
  mutate(year = 2017,
         masterid = masterid %>% as.character()) -> data_child_2017_y

data_child_full <- bind_rows(#data_child_2004_y,
                             #data_child_2008_y,
                             data_child_2010_y,
                             data_child_2012_y,
                             data_child_2015_y,
                             data_child_2017_y) %>%
  mutate(dem_raceeth = dem_raceeth %>% as.character(),
         dem_raceeth = ifelse(dem_raceeth == "White/Other", "White or Other", dem_raceeth),
         dem_raceeth = ifelse(dem_raceeth == "Black/AfricanAmerican", "Black", dem_raceeth)) %>%
  mutate( #dem_countytype = fct_explicit_na(dem_countytype, na_level= NA_character_),
          #dem_income = fct_explicit_na(dem_income, na_level= NA_character_),
          #dem_raceeth = fct_explicit_na(dem_raceeth, na_level= NA_character_),
          #dem_gender = fct_explicit_na(dem_gender, na_level= NA_character_),
          filter_age = fct_explicit_na(filter_age, na_level= NA_character_)) %>%
  mutate(filter_age1 = case_when(filter_age == "0-5" ~ 2.5,
                                 filter_age == "6-11" ~ 8.5,
                                 filter_age == "12-17" ~ 14.5,
                                 filter_age == "998" | filter_age == "999" ~ NA_real_)) %>%
  mutate(dem_income1 = case_when(dem_income == "100% or less" | dem_income == "100% FPL or less" ~ 50,
                                 dem_income == "101%-138% FPL" | dem_income == "101%-138%" ~ 116,
                                 dem_income == "139%-206% FPL" | dem_income == "139%-206%" ~ 172,
                                 dem_income == "207%-300% FPL" | dem_income == "207%-300%" ~ 254,
                                 dem_income == "301% or more" | dem_income == "301% FPL or more" ~ 300.5)) %>%
  mutate(dem_countytype = recode_factor(dem_countytype,
                                        "Metroplitan" =  "Metropolitan", ## delete this line once this issue is fixed in stata 
                                        #"Metropolitan" =  "Metropolitan", ## uncomment this line once issue is fixed in stata
                                        "Suburban" =  "Suburban",
                                        "Appalachian" = "Appalachian",
                                        "Rural Non-Appalachian" = "Rural Non-Appalachian",
                                        .ordered = TRUE))

rm(#ata_child_2004_y,
   #data_child_2008_y,
   data_child_2010_y,
   data_child_2012_y,
   data_child_2015_y,
   data_child_2017_y)

rm(#data_child_2004,
   #data_child_2008,
   data_child_2010,
   data_child_2012,
   data_child_2015,
   data_child_2017)

## create vars lists

data_adult_full %>% mutate_all(as.character) %>%
  gather(var_name, obs, -year ) %>% ##create a long rectangle
  filter(!is.na(obs )) %>% ## filter out year-var combos that are empty
  select(-obs) %>% distinct() %>% ## keep only unique year-var pairs
  arrange(year) -> ind_yr_a

data_child_full %>% mutate_all(as.character) %>%
  gather(var_name, obs, -year ) %>% ##create a long rectangle
  filter(!is.na(obs )) %>% ## filter out year-var combos that are empty
  select(-obs) %>% distinct() %>% ## keep only unique year-var pairs
  arrange(year) -> ind_yr_c


#### modifications to custom shape files to add centroid information
## remove extraneous space in "Huron County Mental Health and Addiction Services "
ohio_adamh<- ohio_adamh %>% mutate(geo_adamh = geo_adamh %>% as.character() %>% trimws() %>% as.factor())

## compute centroids
ohio_aaa <- ohio_aaa %>% mutate(
  centroids = st_centroid(geometry))
## split centroid coordinates into two separate variables
xy_aaa <- do.call(rbind, st_geometry(ohio_aaa$centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
#assign separate varaibles back to original custom shape file
ohio_aaa$cent_lon <- xy_aaa$lon
ohio_aaa$cent_lat <- xy_aaa$lat

ohio_adamh <- ohio_adamh %>% mutate(
  centroids = st_centroid(geometry))
xy_adamh <- do.call(rbind, st_geometry(ohio_adamh$centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
ohio_adamh$cent_lon <- xy_adamh$lon
ohio_adamh$cent_lat <- xy_adamh$lat

ohio_oei <- ohio_oei %>% mutate(
  centroids = st_centroid(geometry))
xy_oei <- do.call(rbind, st_geometry(ohio_oei$centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
ohio_oei$cent_lon <- xy_oei$lon
ohio_oei$cent_lat <- xy_oei$lat

ohio_ctype <- ohio_ctype %>% mutate(
  centroids = st_centroid(geometry))
xy_ctype <- do.call(rbind, st_geometry(ohio_ctype$centroids)) %>% 
  as_tibble() %>% setNames(c("lon","lat"))
ohio_ctype$cent_lon <- xy_ctype$lon
ohio_ctype$cent_lat <- xy_ctype$lat

## drop centroid columns
ohio_aaa %<>% select(-centroids)
ohio_adamh %<>% select(-centroids)
ohio_oei %<>% select(-centroids)
ohio_ctype %<>% select(-centroids)

## move OEI centroid to franklin county
ohio_oei[2,]$cent_lon <- -83.01045
ohio_oei[2,]$cent_lat <- 39.96964

## adjust centroids for county type to actually be in one of the relevant counties
## metropolitan  == franklin 
ohio_ctype[2,]$cent_lon <- -83.01045
ohio_ctype[2,]$cent_lat <- 39.96964
## appalachian move southeast slightly
ohio_ctype[1,]$cent_lon <- -82.11979
ohio_ctype[1,]$cent_lat <- 39.56374
## suburban == medina county
ohio_ctype[4,]$cent_lon <- -81.93957
ohio_ctype[4,]$cent_lat <- 41.04605
## rural non app is fine where it is


## fix AAA
ohio_aaa %<>% mutate(
  geo_aaa = geo_aaa %>% as.character(),
  geo_aaa = case_when(geo_aaa == "AAA (11)" ~ "Area Agency on Aging 11",
                      geo_aaa == "AAA (2)" ~ "Area Agency on Aging 2",
                      geo_aaa == "AAA (9)" ~ "Area Agency on Aging 9",
                      geo_aaa == "AAA (7)" ~ "Area Agency on Aging 7",
                      geo_aaa == "Agency on Aging, AAA (3)" ~ "Area Agency on Aging 3",
                      geo_aaa == "Area Office on Aging of Northwestern Ohio, AAA (4)" ~ "AAA of Northwestern Ohio",
                      geo_aaa == "Buckeye Hills AAA (8)" ~ "Buckeye Hills AAA",
                      geo_aaa == "Central Ohio AAA (6)" ~ "Central Ohio AAA",
                      geo_aaa == "Council on Aging of Southwestern Ohio, AAA (1)" ~ "Council on Aging of Southwestern Ohio",
                      geo_aaa == "Direction Home Akron Canton Area Agency on Aging (10B)" ~ "Direction Home Akron Canton AAA",
                      geo_aaa == "Ohio District 5 Area Agencies on Aging, AAA (5)" ~ "Ohio District 5 AAA",
                      geo_aaa == "Western Reserve AAA (10A)" ~ "Western Reserve AAA"))

#remove temporary list of coordinates
rm(xy_aaa, xy_adamh, xy_oei, xy_ctype)

save(data_adult_full, data_child_full, 
     ind_yr_a, ind_yr_c,
     ohio, OMAS_labels,
     ohio_aaa, ohio_adamh, ohio_oei, ohio_ctype,
     poptext, oh_cent, file="data/OMAS_data.Rdata")

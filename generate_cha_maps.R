################################################################################
###########    Ohio Community Health Assessment Map Generator   ################
################################################################################
# This code is designed to automate the process of generating county-level
# maps of census information for an entire state.
# Written by Dustin Ratliff, MPH - dustin.ratliff@gmail.com

### Install Packages
install.packages("tidyverse",
                 "knitr",
                 "Cairo",
                 "sf",
                 "viridis",
                 "tidycensus")

### Load Packages

#General Packages
library(tidyverse)
library(knitr)

#Visualization Packages

library(Cairo)
library(sf)
library(viridis)

#Census Packages
library(tidycensus)


# Set environment for tidycensys

census_api_key("d7f13444facd6127f91279f4004d6bc413ca7218")
options(tigris_use_cache = TRUE)

# The following functions create easy-to-apply themes for the maps 
# that will be generated later

map_theme <- function() {
  theme(panel.background = element_blank()) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

map_theme_percentage <- function(Percentage, ...) {
  ggplot(data = ..., aes(fill = Percentage, color = Percentage)) +
    geom_sf() +
    coord_sf(crs = 26915) +
    scale_fill_viridis(name = "Percentage") +
    scale_color_viridis(name = "Percentage") +
    theme(panel.background = element_blank()) +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
}

# chart_theme <- function() {
#   theme(panel.background = element_blank()) +
#     theme(axis.title.x = element_blank(),
#           axis.ticks.x = element_blank()) +
#     theme(#axis.title.y = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks.y = element_blank())
# }

# Set variables to pull from the tidycensus/Census API; these are table names

vars <-  {
  c(
    "B25034_002",
    "B25034_001",
    "B25034_003",
    "B25034_004",
    "B01002_001",
    "B19083_001",
    "B17001_001",
    "B17001_002",
    "B02001_001",
    "B02001_002",
    "B01001_003",
    "B01001_020",
    "B01001_021",
    "B01001_022",
    "B01001_023",
    "B01001_024",
    "B01001_025",
    "B01001_027",
    "B01001_044",
    "B01001_045",
    "B01001_046",
    "B01001_047",
    "B01001_048",
    "B01001_049",
    "B08141_001",
    "B08141_002",
    "B19013_001",
    "B02001_001",
    "B02001_002",
    "B02001_003",
    "B02001_004",
    "B02001_005",
    "B02001_006",
    "B02001_007",
    "B02001_008",
    "B02001_009",
    "B02001_010",
    "B03002_001",
    "B03002_002",
    "B03002_003",
    "B03002_004",
    "B03002_005",
    "B03002_006",
    "B03002_007",
    "B03002_008",
    "B03002_009",
    "B03002_010",
    "B03002_011",
    "B03002_012",
    "B03002_013",
    "B03002_014",
    "B03002_015",
    "B03002_016",
    "B03002_017",
    "B03002_018",
    "B03002_019",
    "B03002_020",
    "B03002_021",
    "B15002_001",
    "B15002_002",
    "B15002_003",
    "B15002_004",
    "B15002_005",
    "B15002_006",
    "B15002_007",
    "B15002_008",
    "B15002_009",
    "B15002_010",
    "B15002_011",
    "B15002_012",
    "B15002_013",
    "B15002_014",
    "B15002_015",
    "B15002_016",
    "B15002_017",
    "B15002_018",
    "B15002_019",
    "B15002_020",
    "B15002_021",
    "B15002_022",
    "B15002_023",
    "B15002_024",
    "B15002_025",
    "B15002_024",
    "B15002_025",
    "B15002_026",
    "B15002_027",
    "B15002_028",
    "B15002_029",
    "B15002_030",
    "B15002_031",
    "B15002_032",
    "B15002_033",
    "B15002_034",
    "B15002_035",
    "B15003_001",
    "B15003_002",
    "B15003_003",
    "B15003_004",
    "B15003_005",
    "B15003_006",
    "B15003_007",
    "B15003_008",
    "B15003_009",
    "B15003_010",
    "B15003_011",
    "B15003_012",
    "B15003_013",
    "B15003_014",
    "B15003_015",
    "B15003_016",
    "B15003_017",
    "B15003_018",
    "B15003_019",
    "B15003_020",
    "B15003_021",
    "B15003_022",
    "B15003_023",
    "B15003_024",
    "B15003_025",
    "B25105_001",
    "B01003_001",
    "B27001_001",
    "B27001_002",
    "B27001_003",
    "B27001_004",
    "B27001_005",
    "B27001_006",
    "B27001_007",
    "B27001_008",
    "B27001_009",
    "B27001_010",
    "B27001_011",
    "B27001_012",
    "B27001_013",
    "B27001_014",
    "B27001_015",
    "B27001_016",
    "B27001_017",
    "B27001_018",
    "B27001_019",
    "B27001_020",
    "B27001_021",
    "B27001_022",
    "B27001_023",
    "B27001_024",
    "B27001_025",
    "B27001_026",
    "B27001_027",
    "B27001_028",
    "B27001_029",
    "B27001_030",
    "B27001_031",
    "B27001_032",
    "B27001_033",
    "B27001_034",
    "B27001_035",
    "B27001_036",
    "B27001_037",
    "B27001_038",
    "B27001_039",
    "B27001_040",
    "B27001_041",
    "B27001_042",
    "B27001_043",
    "B27001_044",
    "B27001_045",
    "B27001_046",
    "B27001_047",
    "B27001_048",
    "B27001_049",
    "B27001_050",
    "B27001_051",
    "B27001_052",
    "B27001_053",
    "B27001_054",
    "B27001_055",
    "B27001_056",
    "B27001_057",
    "B25106_001",
    "B25106_002"
  )
}

# Set the year to use to query the Census API

year <- 2017

# Get data for all of the Census tables in the "vars" vector for
# the while state of Ohio, then add a "county" column

state_acs_tract <- get_acs(
  state = "OH",
  #county = county,
  year = year,
  geography = "tract",
  variables = vars,
  output = "wide",
  geometry = TRUE
)

state_acs_tract <- state_acs_tract %>%
  mutate(county = gsub("^[^,]*, ","",NAME)) %>%
  mutate(county = gsub(" County, Ohio","",county)) 

# The following lines creates a vector with mutations on tables of interest.
# These tables have been converted to percentages of the entire poulation when
# it is appropriate.

state_acs_tract <- {state_acs_tract %>%
  mutate(
    GINI = B19083_001E,
    Poverty = B17001_002E/B17001_001E,
    Under5 = B01001_003E + B01001_027E,
    Older64Male = B01001_020E + B01001_021E + B01001_022E + B01001_023E + B01001_024E + B01001_025E,
    Older64Female = B01001_044E + B01001_045E + B01001_046E + B01001_047E + B01001_048E + B01001_049E,
    Older64 = Older64Male + Older64Female,
    NoVehicle = B08141_002E / B08141_001E,
    MedianIncome = B19013_001E,
    White = B03002_003E,
    Black = B03002_004E,
    AIAN = B03002_005E,
    Asian = B03002_006E,
    NHPI = B03002_007E,
    OtherRace = B03002_008E,
    MultipleOrOtherRace = B03002_010E,
    Hispanic = B03002_012E,
    NonHispanic = (B03002_001E-B03002_012E),
    LessThanNinthGrade = (
      B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
        B15003_009E + B15003_010E + B15003_011E + B15003_012E
    ),
    NinthToTwelthNoDiploma = (B15003_013E + B15003_014E + B15003_015E + B15003_016E),
    LessThanHighSchool = (B15003_013E + B15003_014E + B15003_015E + B15003_016E +B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + 
                            B15003_009E + B15003_010E + B15003_011E + B15003_012E),
    HighSchool = (B15003_017E + B15003_018E),
    SomeCollegeNoDegree = (B15003_019E + B15003_020E),
    AssociatesDegree = B15003_021E,
    BachelorsDegree = B15003_022E,
    MastersDegree = B15003_023E,
    ProfessionalSchoolDegree = B15003_024E,
    DoctorateDegree = B15003_025E,
    GraduateOrProfessional = (B15003_023E + B15003_024E + B15003_025E) ,
    HouseholdIncomeTowardsHousing = (B25105_001E * 12) / B19013_001E *
      100,
    HousingCosts = B25105_001E,
    Population = B01003_001E,
    MaleNoInsurance = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + 
                         B27001_023E + B27001_026E + B27001_029E),
    FemaleNoInsurance = (B27001_033E + B27001_036E + B27001_039E + B27001_042E + B27001_045E + B27001_048E + 
                           B27001_051E + B27001_054E + B27001_057E),
    NoInsurance = (B27001_005E + B27001_008E + B27001_011E + B27001_014E + B27001_017E + B27001_020E + 
                     B27001_023E + B27001_026E + B27001_029E +B27001_033E + B27001_036E + B27001_039E + 
                     B27001_042E + B27001_045E + B27001_048E + B27001_051E + B27001_054E + B27001_057E),
    OwnHouse = B25106_002E
  )}


# The following code will create a maps folder and a folder for each county 
# in the state in the working directory. For example, if your working directory
# is C:/R/, this will create a folder at C:/R/maps and sub folders for each
# county in the state

dir.create(paste0(getwd(),"/maps/"))
                   
for (i in unique(state_acs_tract$county)){
  folder<-dir.create(paste0(getwd(),"/maps/",i))
}


# Develop maps for all counties for census table B17001

for (i in unique(state_acs_tract$county)){
  poverty_map <- state_acs_tract %>%
    filter(county == i) %>%
    mutate(Percentage = Poverty*100) %>%
    map_theme_percentage(Percentage=Percentage) +
    
    labs(title = "Percentage of Population Living in Poverty by Census Tract",
         subtitle = bquote(.(i) ~ "County," ~ "Ohio," ~ .(year)),
         caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))
  ggsave(
    plot = poverty_map,
    file = paste0(
      "maps/",i,"/", year,"_","poverty_map.png"
        ),
    width = 6,
    height = 4.5,
    type = "cairo-png"
  )
  
}

# Develop maps for all counties for census table B27001

for (i in unique(state_acs_tract$county)){
  
  no_health_insurance_map <- state_acs_tract %>%
    filter(county == i) %>%
    select(
      GEOID,
      NAME,
      NoInsurance,
      B27001_001E
    ) %>%
    mutate(Percentage = (NoInsurance/B27001_001E)*100) %>%
    map_theme_percentage(Percentage = Percentage)+
    labs(title = "Percentage of Individuals with No Insurance by Census Tract",
         subtitle = bquote(.(i) ~ "County," ~ "Ohio," ~ .(year)),
         caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))
  
  ggsave(
    plot = no_health_insurance_map,
    file = paste0(
      "maps/",i,"/", year,"_","no_health_insurance_map.png"
    ),
    width = 6,
    height = 4.5,
    type = "cairo-png"
  )
  
}

#Generate maps for educational attainment

for (i in unique(state_acs_tract$county)){
  
  educational_attainment_map <- state_acs_tract %>%
    filter(county == i) %>%
    select(
      GEOID,
      NAME,
      LessThanHighSchool,
      HighSchool,
      SomeCollegeNoDegree,
      AssociatesDegree,
      BachelorsDegree,
      GraduateOrProfessional,
      B15003_001E
    ) %>%
    rename("Less Than High School" = LessThanHighSchool) %>%
    rename("Some College, No Degree" = SomeCollegeNoDegree) %>%
    rename("High School or Equivalent" = HighSchool) %>%
    rename("Associate's Degree" = AssociatesDegree) %>%
    rename("Bachelor's Degree" = BachelorsDegree) %>%
    rename("Graduate or Professional Degree" = GraduateOrProfessional) %>%
    gather(key = EducationalAttainment, value = percentage, -GEOID, -NAME, -geometry, -B15003_001E) %>%
    mutate(percentage = percentage/B15003_001E) %>%
    mutate(EducationalAttainment =
             factor(
               EducationalAttainment,
               levels = c(
                 "Less Than High School",
                 "High School or Equivalent",
                 "Some College, No Degree",
                 "Associate's Degree",
                 "Bachelor's Degree",
                 "Graduate or Professional Degree"
               )
             )) %>%
    mutate(percentage = percentage * 100) %>%
    ggplot(aes(fill = percentage, color = percentage)) +
    facet_wrap(~ EducationalAttainment) +
    geom_sf() +
    coord_sf(crs = 26915) +
    scale_fill_viridis(name = "Percentage") +
    scale_color_viridis(name = "Percentage") +
    labs(title = "Highest Level of Educational Attainment by Census Tract",
         subtitle = bquote(.(i) ~ "County," ~ "Ohio," ~ .(year)),
         caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau")) +
    map_theme()
  
  ggsave(
    plot = educational_attainment_map,
    file = paste0(
      "maps/",i,"/", year,"_","educational_attainment_map.png"
    ),
    width = 12,
    height = 9,
    type = "cairo-png"
  )
}

#Generate maps for race/ethnicity

for (i in unique(state_acs_tract$county)){
  race_map <- state_acs_tract %>%
    filter(county == i) %>%
    mutate(OtherRace = (B03002_010E +B03002_008E + B03002_005E + B03002_007E)/B03002_001E) %>%
    select(GEOID,
           NAME,
           White,
           Black,
           Asian,
           OtherRace,
           Hispanic,
           NonHispanic,
           geometry) %>%
    rename("Non-Hispanic, White" = White) %>%
    rename("Non-Hispanic, Black or African American" = Black) %>%
    rename("Non-Hispanic, Asian" = Asian) %>%
    rename("Non-Hispanic, Multiple or Other Race" =OtherRace) %>%
    rename("Hispanic, Any Race" = Hispanic) %>%
    rename("Non-Hispanic, Any Race" = NonHispanic) %>%
    gather(key = race, value = population, -GEOID, -NAME, -geometry) %>%
    mutate(race = factor(race,levels = c(
      "Non-Hispanic, Asian",
      "Non-Hispanic, Black or African American",
      "Non-Hispanic, Multiple or Other Race",
      "Non-Hispanic, White",
      "Hispanic, Any Race",
      "Non-Hispanic, Any Race"))) %>%
    
    ggplot(aes(fill = population, color = population)) +
    facet_wrap(~ race) +
    geom_sf() +
    coord_sf(crs = 26915) +
    scale_fill_viridis(name = "Population") +
    scale_color_viridis(name = "Population") +
    labs(title = "Population Density by Race by Census Tract",
         subtitle = bquote(.(i) ~ "County," ~ "Ohio," ~ .(year)),
         caption = bquote("Source:" ~ .(year) ~ "American Community Survey, U.S. Census Bureau"))+
    map_theme()
  
  ggsave(
    plot = race_map,
    file = paste0(
      "maps/",i,"/", year,"_","race_map.png"
    ),
    width = 12,
    height = 9,
    type = "cairo-png"
  )
  
}
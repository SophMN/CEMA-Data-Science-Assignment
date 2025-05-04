#Load the necessary packages
library(tidyverse)
library(readxl)
library(sf)
library(lme4)
library(RColorBrewer)
library(viridis)
library(janitor)
library(broom)

#Load the data 
hiv <- read.csv("data/HIV data 2000-2023.csv")
high_burden_countries <- read.csv("output/high_burden_countries.csv")
child_mortality <- read.csv("data/dataset_datascience.csv")

#Inspect HIV dataset
class(hiv)
colnames(hiv)
glimpse(hiv)
head(hiv)
View(hiv)

#Extract the estimated number of HIV cases
hiv <- hiv %>%
  mutate(EstimatedNumber = str_extract(Value, "^[^\\[]+") %>% 
           str_remove_all(., "[< ]") %>% 
           as.numeric())

head(hiv)
View(hiv)
class(hiv$EstimatedNumber)
write.csv(hiv, file = "output/hiv.csv")
head(hiv)
  
#Calculate HIV burden as of 2023
hiv_2023 <- hiv %>% 
  filter(Period == 2023)

#Compute the global burden of HIV
global_burden <- sum(hiv_2023$EstimatedNumber, na.rm = TRUE)

#Arrange the countries by HIV burden
hiv_2023 <- hiv_2023 %>% 
  arrange(desc(EstimatedNumber)) %>%
  mutate(CumulativeSum = cumsum(EstimatedNumber),
         CumulativePercentage = CumulativeSum / global_burden)

#Identify the high burden countries
high_burden_countries <- hiv_2023 %>% 
  filter(CumulativePercentage <= 0.76)
high_burden_countries
View(high_burden_countries)
write.csv(high_burden_countries, file = "output/high_burden_countries.csv")

#Get the list of the high burden countries
top_countries <- high_burden_countries$Location

#Filter the HIV data of the top countries
hiv_countries <- hiv %>% 
  filter(Location %in% top_countries)
head(hiv_countries)
View(hiv_countries)

#Plot the trends in HIV cases in high burden countries
hiv_countries %>%
  filter(!is.na(EstimatedNumber)) %>% 
  ggplot(aes(x = Period, y = EstimatedNumber / 100, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "plasma") +
  labs(x = "Year", y = "Estimated Cases (x100)", 
       title = "Trend of HIV Cases in High Burden Countries")

#Group the HIV cases by WHO region
hiv_who_region <- hiv_2023 %>% 
  filter(!is.na(EstimatedNumber)) %>%
  group_by(ParentLocationCode) %>% 
  arrange(ParentLocationCode, desc(EstimatedNumber)) %>% 
  mutate(RegionalTotal = sum(EstimatedNumber), 
         CumulativeSum = cumsum(EstimatedNumber),
         CumulativePercentage = CumulativeSum / RegionalTotal) %>% 
  ungroup()
head(hiv_who_region)  
View(hiv_who_region)

#Identify the top countries in each WHO region
high_burden_who <- hiv_who_region %>% 
  filter(CumulativePercentage <= 0.75)
View(high_burden_who)

#Get the list of the high burden countries
top_countries_who <- high_burden_who$Location

#Filter the HIV data of the top countries in each WHO region
hiv_countries_who <- hiv %>% 
  filter(Location %in% top_countries_who) %>% 
  filter(!is.na(EstimatedNumber))
View(hiv_countries_who)

#Plot the trend in HIV cases per WHO region
#Africa
hiv_countries_who %>%
  filter(ParentLocationCode == "AFR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber / 1e6, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases (Millions)",
       title = "Trend of HIV Cases in High Burden Countries in Africa")

#Americas
hiv_countries_who %>% 
  filter(ParentLocationCode == "AMR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber / 1e5, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases (x100,000)", 
       title = "Trend of HIV Cases in High Burden Countries in the Americas")

#Eastern Mediterranean
hiv_countries_who %>% 
  filter(ParentLocationCode == "EMR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber / 1e5, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases (x100,000)", 
       title = "Trend of HIV Cases in High Burden Countries in Eastern Mediterranean")

#Europe
hiv_countries_who %>% 
  filter(ParentLocationCode == "EUR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases", 
       title = "Trend of HIV Cases in High Burden Countries in Europe")

#South East Asia
hiv_countries_who %>% 
  filter(ParentLocationCode == "SEAR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber, color = Location)) +
  geom_point() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases", 
       title = "Trend of HIV Cases in High Burden Countries in SE Asia")

#Western Pacific
hiv_countries_who %>% 
  filter(ParentLocationCode == "WPR") %>% 
  ggplot(aes(x = Period, y = EstimatedNumber, color = Location)) +
  geom_line() +
  theme_minimal() +
  scale_color_viridis_d(option = "turbo") +
  labs(x = "Year", y = "Estimated Cases", 
       title = "Trend of HIV Cases in High Burden Countries in Western Pacific")

#Load poverty data
multidimensional_poverty <- read_excel("data/multidimensional_poverty.xlsx", 
                                       skip = 2)
glimpse(multidimensional_poverty)
multidimensional_poverty <- multidimensional_poverty %>% 
  rename(Region = `...1`, CountryCode = `...2`, 
         Economy = `...3`, ReportingYear = `...4`, 
         SurveyName = `...5`, SurveyYear = `...6`, 
         SurveyCoverage = `...7`, WelfareType = `...8`, 
         SurveyCompatibility = `...9`, Monetary = `Monetary (%)`, 
         EducationalAttainment = `Educational attainment (%)`, 
         EducationalEnrollment = `Educational enrollment (%)`, 
         Electricity = `Electricity (%)`, 
         Sanitation = `Sanitation (%)`, 
         DrinkingWater = `Drinking water (%)`, 
         MultidimensionalPovertyHeadcountRatio = `...16`)
glimpse(multidimensional_poverty)

#Change specific columns to numeric
multidimensional_poverty <- multidimensional_poverty %>% 
  mutate(across(c(EducationalAttainment, EducationalEnrollment, 
                  Electricity, Sanitation, DrinkingWater), 
                ~ as.numeric(str_replace(., "-", NA_character_))))
glimpse(multidimensional_poverty)

#Convert percentages into decimals
multidimensional_poverty <- multidimensional_poverty %>% 
  mutate(across(c(Monetary, EducationalAttainment, EducationalEnrollment, 
         Electricity, Sanitation, DrinkingWater, 
         MultidimensionalPovertyHeadcountRatio), ~ .x / 100))
View(multidimensional_poverty)

#Import HIV data
hiv <- read.csv("output/hiv.csv")
head(hiv)

#Inspect the country names in both datasets
setdiff(hiv$Location, multidimensional_poverty$Economy)
setdiff(multidimensional_poverty$Economy, hiv$Location)

#Match the country names between datasets for effective merging
country_matching <- c(
  "Democratic Republic of the Congo" = "Congo, Democratic Republic of",
  "United Republic of Tanzania" = "Tanzania",
  "United States of America" = "United States",
  "Iran (Islamic Republic of)" = "Iran, Islamic Republic of",
  "Republic of Korea" = "Korea, Republic of",
  "Republic of Moldova" = "Moldova",
  "Czechia" = "Czech Republic",
  "Slovakia" = "Slovak Republic",
  "Bolivia (Plurinational State of)" = "Bolivia",
  "T\x9frkiye" = "Turkiye",
  "Netherlands (Kingdom of the)" = "Netherlands",
  "Egypt" = "Egypt, Arab Republic of",
  "Kyrgyzstan" = "Kyrgyz Republic")
hiv <- hiv %>% 
  mutate(Location = recode(Location, !!!country_matching))
setdiff(multidimensional_poverty$Economy, hiv$Location)

#Merge the HIV and multidimensional poverty datasets
hiv_poverty <- left_join(hiv, multidimensional_poverty, 
                         by = c("Location" = "Economy"))
View(hiv_poverty)

#Remove missing values
hiv_poverty <- hiv_poverty %>% 
  filter(!is.na(Monetary),
         !is.na(EducationalAttainment), 
         !is.na(EducationalEnrollment), 
         !is.na(Electricity), 
         !is.na(Sanitation), 
         !is.na(DrinkingWater), 
         !is.na(MultidimensionalPovertyHeadcountRatio), 
         !is.na(EstimatedNumber))

#Change the Period variable to a factor
hiv_poverty <- hiv_poverty %>% 
  mutate(Period = as.factor(Period)) %>% 
  glimpse()

#Mixed effects model
model <- lmer(EstimatedNumber ~ MultidimensionalPovertyHeadcountRatio + 
                Monetary + EducationalAttainment + EducationalEnrollment + 
                Electricity + Sanitation + DrinkingWater + 
                (1 | Location) + (1 | Period), data = hiv_poverty)
summary(model)
tidy(model)

#Question 2
#Import data 
child_mortality <- read.csv("data/dataset_datascience.csv")
glimpse(child_mortality)
View(child_mortality)

#Filter data for the EAC
eac <- c("United Republic of Tanzania", "Uganda", "Somalia", 
         "South Sudan", "Rwanda", "Kenya", 
         "Burundi", "Democratic Republic of the Congo")
eac_child_mortality <- child_mortality %>% 
  filter(Geographic.area %in% eac)
View(eac_child_mortality)

#Rename Tanzania for easier merging with shapefile
eac_child_mortality <- eac_child_mortality %>% 
  mutate(Geographic.area = ifelse (Geographic.area == 
              "United Republic of Tanzania", "Tanzania", Geographic.area))

#Load the shapefiles of each country
burundi <- st_read("data/gadm41_BDI_shp/gadm41_BDI_0.shp")
drc <- st_read("data/gadm41_COD_shp/gadm41_COD_0.shp")
kenya <- st_read("data/gadm41_KEN_shp/gadm41_KEN_0.shp")
rwanda <- st_read("data/gadm41_RWA_shp/gadm41_RWA_0.shp")
somalia <- st_read("data/gadm41_SOM_shp/gadm41_SOM_0.shp")
southsudan <- st_read("data/gadm41_SSD_shp/gadm41_SSD_0.shp")
tanzania <- st_read("data/gadm41_TZA_shp/gadm41_TZA_0.shp")
uganda <- st_read("data/gadm41_UGA_shp/gadm41_UGA_0.shp")

#Merge all the shapefiles 
eac_sf <- rbind(burundi, drc, kenya, rwanda, 
                somalia, southsudan, tanzania, uganda)

#Filter the under-five mortality data as of 2023
under_five <- eac_child_mortality %>% 
  filter(Indicator == "Under-five mortality rate", Series.Year == "2023")

#Filter neonatal mortality data as of 2023
neonatal <- eac_child_mortality %>% 
  filter(Indicator == "Neonatal mortality rate", Series.Year == "2023")

#Merge EAC map with mortality data
#Under-five mortality
eac_under_five <- eac_sf %>% 
  left_join(under_five, by = c("COUNTRY" = "Geographic.area"))
write.csv(eac_under_five, file = "output/eac_under_five.csv")
eac_under_five <- read.csv("output/eac_under_five.csv")
#Neonatal mortality
eac_neonatal <- eac_sf %>% 
  left_join(neonatal, by = c("COUNTRY" = "Geographic.area"))
write.csv(eac_neonatal, file = "output/eac_neonatal.csv")
eac_neonatal <- read.csv("output/eac_neonatal.csv")

#Visualise under-five mortality
eac_under_five %>% 
  filter(COUNTRY == "Kenya") %>% 
  ggplot(aes(fill = Observation.Value)) +
  geom_sf() +
  scale_fill_viridis_c(option = "plasma") +
  theme_minimal() +
  labs(title = "Under-five Mortality Rates in Kenya", 
       fill = "Deaths per 1,000 live births")

#Plot under-five average mortality rates
#Extract years for easier visualisation
eac_child_mortality <- eac_child_mortality %>% 
  mutate(Year = as.numeric(str_extract(Series.Year, "^\\d{4}")))

#Calculate the average mortality per year and indicator
eac_mortality_avg <- eac_child_mortality %>% 
  group_by(Indicator, Geographic.area, Year) %>% 
  summarise(AverageMortality = mean(Observation.Value, na.rm = TRUE))
View(eac_mortality_avg)

eac_mortality_avg %>% 
  filter(Indicator == "Under-five mortality rate") %>% 
  ggplot(aes(x = Year, y = AverageMortality, color = Geographic.area)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1955, 2023, by = 10)) +
  scale_color_brewer(palette = "Set1") +
  labs(x = "Year", y = "Average deaths per 1000 live births", 
       title = "Under-five mortality rates over time in EAC", 
       color = "Country")

#Plot neonatal mortality rate
eac_mortality_avg %>% 
  filter(Indicator == "Neonatal mortality rate") %>% 
  ggplot(aes(x = Year, y = AverageMortality, color = Geographic.area)) +
  geom_line() +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1955, 2023, by = 10)) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Year", y = "Average deaths per 1000 live births", 
       title = "Neonatal mortality rates over time in EAC", 
       color = "Country")










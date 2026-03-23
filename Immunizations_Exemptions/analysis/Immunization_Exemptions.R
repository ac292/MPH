#' 
#' * Data Set Overview
#' 
#' We will use the *Vaccination Coverage among Kindergartners* data set available at
#' [https://data.cdc.gov/Vaccinations/Vaccination-Coverage-and-Exemptions-among-Kinderga/ijqb-a7ye/about_data]. 
#' The population studied is that of children enrolled in Kindergarten in public schools in the United states. The study reports on vaccination 
#' and exemption rates for the school years spanning 2009-2010 to 2024-2025. The data was collected mainly through the census report, which is a 
#' mandated collection of data reported by all public school administrators. 
#' 
#' # Loading Data
#' 
## --------------------------------------------------------------------------------------

# Set working directory 
# C:\Users\alexm\OneDrive\Documents\CLASSES\VTPEH6270\Immunizations _K
setwd("C:/Users/alexm/REPOS/MPH/Immunizations_Exemptions/analysis")
# Load file in df "data_vaccination" from excel file 
# downloaded from CDC "Vaccination_Coverage_and_Exemptions_among_Kindergartners_20260201"
data_K <- 
  read.csv("../data/Vaccination_Coverage_and_Exemptions_among_Kindergartners_20260201.csv",header=TRUE)

# Preview the data
names(data_K)

# count the number of rows
nobs_data_K <- nrow(data_K)

#' The data set is not labeled using best practices (e.g., uses unusual characters like "..."). We clean the formatting using the `janitor` package.
#' 
## --------------------------------------------------------------------------------------

# install.packages("janitor")

# library(janitor)
library(janitor)
cleaned_data_K <- clean_names(data_K)

# Preview the cleaned data
names(cleaned_data_K)


#' 
#' # Survey the data included in the data set
#' *We count the number of observations in the original data set and also the number 
#' of geographical areas surveyed and the type of survey that fed the data set.*
#' 
## ----results='hide'--------------------------------------------------------------------
# count the number of observations
nrow(cleaned_data_K)

# what are the unique values in some of the character columns
unique(cleaned_data_K$geography_type)
unique(cleaned_data_K$survey_type)

#' 
#' # Simplification and survey of data set
#' *The data set contains data that is either redundant or un unnecessary for the analysis planned at this time. 
#' We create a new data frame after removing this data that we do not deem necessary at this time. We will go 
#' back to the original data set for following analysis. 
#' 
## --------------------------------------------------------------------------------------
# remove the columns geography_type, footnotes & survey_type

cleaned_data_K <- 
  cleaned_data_K[, !names(cleaned_data_K) %in% 
                   c("geography_type","footnotes","survey_type")]


#' 
#' 
#' *Which vaccines are considered in the data set? *
#' 
## --------------------------------------------------------------------------------------

#the Vaccines/Exemption considered are: unique values in variable vaccine_exemption
unique(cleaned_data_K$vaccine)

#' 
#' The considered vaccines or type of exemptions are: 
#' - Hepatitis B 
#' - Varicella 
#' - Polio
#' - Mumps, Measles, Rubella
#' - Diphteria Tetanos Pertussis
#' 
#' When Kindergartners are not vaccinated, an exemption must be submitted: 
#' - Medical Exemption
#' - Non-Medical Exemption
#' 
#' 
## ----results='hide'--------------------------------------------------------------------

#the dimensions considered are: unique values in variable geography  
unique(cleaned_data_K$geography)

#' 
#' *Which areas are represented in the data set?*
#' 
#' The represented geographical areas are:
#' 
#' We will create a new data set that only contains data per state- 
#' we will remove the data for the whole USA and the data that is specific 
#' to cities (NYC, Houston, San Antonio).
#' 
## --------------------------------------------------------------------------------------

# cleaned_data_K %>%
# filter(grepl("^TX-", geography))
class(cleaned_data_K)
options(max.print = 10000)

library(dplyr)
library(stringr)
cleaned_data_K <- cleaned_data_K %>%
  filter(
    !str_starts(geography, regex("TX-|NY-", ignore_case = TRUE)) &
    !str_detect(geography, regex("United States|U\\.S\\. Median", ignore_case = TRUE))
  )

# count the typical total population surveyed in one school year for one type of Vaccine

library(dplyr)
pop_K <- cleaned_data_K %>%
  filter(vaccine_exemption == "Varicella", school_year == "2024-25", 
         dose == "UTD (unknown disease history)") %>%
  summarise(total = sum(population_size, na.rm = TRUE)) %>%
  pull(total)

# check for no redundancy in the population_size
pop_K_varicella_2024 <- cleaned_data_K %>%
  filter(vaccine_exemption == "Varicella", school_year == "2024-25",
         dose == "UTD (unknown disease history)")

# place commas for the thousands
pop_K = format(pop_K, big.mark = ",")

mean_pop_K <- mean(pop_K_varicella_2024$population_size, na.rm = TRUE)
max_pop_K <- max(pop_K_varicella_2024$population_size, na.rm = TRUE)
mean_pos_K <- format(round(mean_pop_K, 0), big.mark = ",")


#' 
#' The data set countains `r nobs_data_K` observations. 
#' 
#' The data set draws from a very large population: for example: the number of 
#' children for which Varicella vaccination data was reported in school year 2024-25 is `r pop_K`. 
#' The average number of children surveyed per geographical area is `r mean_pop_K`. 
#' Let's look at a histogram of population surveyed for Varicella immunization in school year 2024-25.
#' 
## --------------------------------------------------------------------------------------

library(ggplot2)

ggplot(pop_K_varicella_2024, aes(x = population_size)) +
  geom_histogram(
    bins = 20,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = NULL,  # remove x-axis label,
    caption = "Population surveyed in various USA geographical area.
    \n Data is for Varicella 2024-25 school year"
  ) +
  theme(
    plot.caption = element_text(size = 11, hjust = 0)
  )

#' 
#' This is not a very informative histogram.
#' 
#' *What is the time period covered by the study? 
#' Indicate all school years sampled* \
#' 
#' 
## --------------------------------------------------------------------------------------

# find minimum value of school_year
unique(cleaned_data_K$school_year)

# substract "_XX" from cleaned_data_K$school_year start at digit 1 end at digit 4
cleaned_data_K$year <- as.numeric(substr(cleaned_data_K$school_year, 1, 4))

# find min and max year of study
min(cleaned_data_K$year)
max(cleaned_data_K$year)

#' 
#' *  The study spans from `r min(cleaned_data_K$year)` to `r max(cleaned_data_K$year)`.  
#' We need to create a new numerical column "year" to describe the 20XX-XX+1 school year as 20XX. 
#' 
#' *What type of data do you expect the estimated vaccination coverage to be? What is it in the original data set? How can you correct this?*
#' 
#' * The estimate value of vaccination coverage is a percentage and thus should be a numerical value. It is encoded in the original data set as a character. It needs to be re-encoded as a numerical value. First step is to remove all the NR or NA values in the estimate column. \color{black} 
## --------------------------------------------------------------------------------------

# Check class
class(cleaned_data_K$estimate)

# find out data where the data is not numerical
# list rows where estimate is non numerical
# unique(cleaned_data_K$estimate)

# filter out data when non numerical
library(tidyverse)
data_numerical_K <- filter(cleaned_data_K, estimate !="Nreq") 
data_numerical_K <- filter(data_numerical_K, estimate !="NReq") 
data_numerical_K <- filter(data_numerical_K, estimate !="NR") 
# Test: list rows in output data frame where estimate is non numerical
# unique(data_numerical_K$estimate)


#' * The estimate value of vaccination coverage is a percentage and is now ready to 
#' be encoded in a numerical value. 
#' 
## --------------------------------------------------------------------------------------

# Class conversion
# convert estimate column as  as numeric
# installing the package dplyr to use the mutate function
library(dplyr)
data_numerical_K <- data_numerical_K %>%
 mutate(estimate = as.numeric(estimate))

# Check output for first line of data after conversion and 
# compare with same before conversion
data1a <- first(data_numerical_K$estimate)
data1b <- first(cleaned_data_K$estimate)

# Check range of values for estimate variable
# find minimum value of estimate value
min(data_numerical_K$estimate)
# find maximum value of estimate value
max(data_numerical_K$estimate)


#' Vaccination coverage is now recorded as a numerical value.
#' The face value of the first line of data was `r data1a` before 
#' class conversion and is now still `r data1b`, hence the data was 
#' not altered during class conversion. 
#' 
#'  Focus on the Analysis of Vaccination Exemptions
#' 
## ----results='hide'--------------------------------------------------------------------

# filter the data for exemptions
# using package tydiverse
library(tidyverse)
data_exemption <- filter(data_numerical_K, 
                      vaccine_exemption =="Exemption") 
data_exemption_nonmed <- filter(data_exemption, 
                      dose =="Non-Medical Exemption") 

# find minimum value of estimate for Non-Medical Exemptions
min(data_exemption_nonmed$estimate)
# find maximum value of value for exemptions
max(data_exemption$estimate)

# find which geography has minimum value of value for Non-Medical Exemptions
# find year and geography for minimum value of estimate  for Non-Medical Exemptions
year_min_exemption <- data_exemption_nonmed[which.min(data_exemption_nonmed$estimate),]
print(year_min_exemption$year)
print(year_min_exemption$geography)


# find which geography has maximum value of value for exemptions
# find year and geography for maximum value of estimate exemption
year_max_exemption <- data_exemption[which.max(data_exemption$estimate),]
print(year_max_exemption$year)
print(year_max_exemption$geography)

# Subset the data to Idaho, that has the highest % of exemptions
data_exemption_Idaho <- filter(data_exemption, 
                      geography =="Idaho") 


#' 
#' # Table for Vaccination Exemptions
#' 
# make a nice table for the data
library(kableExtra)
tbl <- data_exemption %>%
  group_by(geography) %>%
  summarise(
    mean_estimate = mean(estimate, na.rm = TRUE),
    sd_estimate = sd(estimate, na.rm = TRUE),
    mean_exemptions = mean(number_of_exemptions, na.rm = TRUE),
    mean_population = mean(population_size, na.rm = TRUE)
  ) %>%
  arrange(desc(mean_estimate)) %>%
  kable(
    caption = "Vaccination exemptions by state (averaged over years 2009-2025)",
    digits = 2,
    format = "html"
  ) %>%
  kable_styling(full_width = FALSE)

# output table
save_kable(tbl, "../output/table_exemptions_by_state.html")
#' 
#' 
#' # Raw plotting of Vaccination Exemptions for each Geographical Area
#' 
#' 
## ----fig.width=10, fig.height=14-------------------------------------------------------

# make a ggplot with multi facets for different states
# plot the data after installing the ggplot2 package
library(ggplot2)

my_plot <- ggplot(data_exemption, 
       aes(year,estimate, color=dose)) +
  geom_point(size=1, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( title = "Vaccination Exemptions rates for Kindergartners",
        subtitle = "(CDC data)",
        x = "Year", 
        y = "Vaccination Exemption Rates (%) ",
        color = "Type of Exemption ") + 
  scale_x_continuous(breaks = c(2010, 2020), 
                     labels = c("2010", "2020")) + 
  facet_wrap(geography ~.,ncol=5) +
  geom_line()

# save plot to a output file
ggsave(
  filename = "../output/Immunization_Exemptions_by_State.png",
  plot = my_plot,
  width = 8.5,
  height = 10, 
  dpi =300
)



#' 
#' * The geographical area that has the highest level of vaccination exemptions  (`r max(data_exemption$estimate)`%) is 
#'  `r year_max_exemption$geography` in the school year starting `r year_max_exemption$year `. The state that has the lowest 
#'  level of Non-Medical Exemptions is Maine. 
#' 
#' *First Subset the data to Idaho, which has the highest % of exemptions*
#' 
#' *Make a plot showing the estimated vaccination exemption rate across years for Idaho*
#' 
#' *Generate a* ***publication-ready*** *plot.*
#' 
#' 
## ----results='hide'--------------------------------------------------------------------

# plot the data after installing the ggplot2 package
library(ggplot2)
Idaho_plot <- ggplot(data_exemption_Idaho, 
       aes(year,estimate, color=dose)) +
  geom_point(size=3, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( title = "Vaccination Exemptions rates for Kindergartners",
        subtitle = "(CDC data for Idaho State)",
        x = "Year", 
        y = "Vaccination Exemption Rates (%) ",
        color = "Type of Exemption") +
  scale_x_continuous(breaks = c(2009, 2014, 2019, 2024), 
                     labels = c("2009", "2014", "2019", "2024")) + 
  geom_line()

# save plot to a output file
ggsave(
  filename = "../output/Immunization_Exemptions_Idaho.png",
  plot = Idaho_plot,
  width = 5.25,
  height = 3, 
  dpi =300
)


# find minimum value of estimate for non-medical exemptions for Idaho
data_exemption_Idaho_nonmed <- filter(data_exemption_Idaho, 
                      dose =="Non-Medical Exemption") 
print(min(data_exemption_Idaho_nonmed$estimate))
# find maximum value of estimate for non-medical exemptions for Idaho
print(max(data_exemption_Idaho_nonmed$estimate))

# find minimum value of estimate for medical exemptions for Idaho
data_exemption_Idaho_med <- filter(data_exemption_Idaho, 
                      dose =="Medical Exemption") 

# write a csv file with only Idaho data

data_Idaho <- subset(data_numerical_K, geography == "Idaho")
write.csv(data_Idaho, "../data/Immunizations_K_Idaho.csv", row.names=F)


#' 
#' *What does this plot show?*
#' 
#' - This plot shows that the vaccination exemption rate in Idaho has steadily 
#'  increased from a low of `r min(data_exemption_Idaho_nonmed$estimate)`\% since 
#'  data was first collected in `r min(data_exemption_Idaho_nonmed$year)`, to reach 
#'  its highest level `r max(data_exemption_Idaho_nonmed$estimate)`\% in
#'   `r max(data_exemption_Idaho_nonmed$year)`, which is the last year for 
#'   which data is available. 
# 
# In contrast, the vaccination exemption rate for medical reasons has been relatively stable at rates between `r min(data_exemption_Idaho_med$estimate)`\% and `r max(data_exemption_Idaho_med$estimate)`\% over the same time period.
#' 
#' 
#' *For comparison, let's look at the data from Maine, which has the lowest % of 
#' Non-Medical Exemptions*
#' 
#' 
## --------------------------------------------------------------------------------------

# Subset the data to Maine, that has the lowest % of Non-Medical Exemptions
data_exemption_Maine <- filter(data_exemption, 
                      geography =="Maine") 


#' 
#' 
#' *Make a plot showing the estimated vaccination exemption rate across years for Maine*
#' 
#' *Generate a* ***publication-ready*** *plot.*
#' 
#' 
## ----results='hide'--------------------------------------------------------------------
# plot the data after installing the ggplot2 package
library(ggplot2)
Maine_plot <- ggplot(data_exemption_Maine, 
       aes(year,estimate, color=dose)) +
  geom_point(size=3, alpha=0.5) + 
  theme(plot.title = element_text(hjust=0.5, size=10)) + 
  theme(plot.subtitle = element_text(hjust=0.5, size=10)) +
  labs( title = "Vaccination Exemptions rates for Kindergartners",
        subtitle = "(CDC data for Maine State)",
        x = "Year", 
        y = "Vaccination Exemption Rates (%) ",
        color = "Type of Exemption ") + 
  scale_x_continuous(breaks = c(2009, 2014, 2019, 2024), 
                     labels = c("2009", "2014", "2019", "2024")) + 
  geom_line()

# save plot to a output file
ggsave(
  filename = "../output/Immunization_Exemptions_Maine.png",
  plot = Maine_plot,
  width = 5.25,
  height = 3, 
  dpi =300
)

# find minimum value of estimate for non-medical exemptions for Maine
data_exemption_Maine_nonmed <- filter(data_exemption_Maine, 
                      dose =="Non-Medical Exemption") 
print(min(data_exemption_Maine_nonmed$estimate))

# find maximum value of estimate for non-medical exemptions for Maine
print(max(data_exemption_Maine_nonmed$estimate))

# find year for maximum value of estimate for non-medical exemptions for Maine
year_max_exemption_Maine_nonmed <-
  data_exemption_Maine_nonmed[which.max(data_exemption_Maine_nonmed$estimate),]
print(year_max_exemption_Maine_nonmed$year)

# find year for minimum value of estimate for non-medical exemptions for Maine
year_min_exemption_Maine_nonmed <-
  data_exemption_Maine_nonmed[which.min(data_exemption_Maine_nonmed$estimate),]
print(year_min_exemption_Maine_nonmed$year)

# find minimum value of estimate for medical exemptions for Maine
data_exemption_Maine_med <- filter(data_exemption_Maine, 
                      dose =="Medical Exemption") 


#' *What does this second plot show?*
#' 
#' 
#' # This plot shows that the vaccination exemption rate in the state of Maine has # decreased from a high of
#' `r max(data_exemption_Maine_nonmed$estimate)`\% reached in 
#' `r year_max_exemption_Maine_nonmed$year`, to reach its lowest level 
#' `r min(data_exemption_Maine_nonmed$estimate)`\% in 
#' `r year_min_exemption_Maine_nonmed$year`. 
#' In contrast, the vaccination exemption rate for medical reasons has been relatively stable at rates between 
#' `r min(data_exemption_Maine_med$estimate)`\% and 
#' `r max(data_exemption_Maine_med$estimate)`\% over the same time period.
#' 
#'

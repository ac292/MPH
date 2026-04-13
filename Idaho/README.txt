Correlations between Social Determinants in the state of Idaho (2019–2024)

# Overview

Focusing on the state of **Idaho**, we will try to find correlations between political affiliation and social determinants such as education levels, median income, poverty rates, population density, language isolation by comparing data at the level of counties, based on the hypothesis that immunization resistance is linked to party affiliation and/or lack of education. I do not have access to immunization resistance data at the level of counties even though I requested it from the Idaho Department of Health. Lacking this crucial piece of data, I will look at possible correlations between social determinants, e.g. median income, poverty levels, language isolation, population density, education levels, rates of voter registration by political parties, and also look if correlations can be seen between education levels and median income. The data available for Idaho residents are social determinants values for each county. Because we do not have access to information on individual Idaho residents, we work with the crude approximation that counties represent a concentration of subjects with a given value of a trait (variable) and we will compare it with values observed for other traits (variables). This is not a good approach but is the only available given the data at hand. 
For the variable "population count" per county, we make the approximation that each county is of the same surface, hence the population count is a representation of density of population. The value for the median income is not independent of the % of population at the poverty level, but the two variables report on different statistics for each county. 


# Research Questions

This project aims to explore the following questions:

- How does county median income relate to party affiliation?

- How does county median income relate to county population density?

- How does county median income relate to % of county poverty levels?

- How does county median income relate to % of county language isolation?

- How does county median income relate to % of low education?

# Data

We use voter registration data as of January 2025 from the website https://voteidaho.gov/data-and-dashboards/voter-registration-totals/.
The data for Idaho counties poverty rates, median income, language isolation, education levels, population are collected for the year 2019-2023 and are available at
https://hdpulse.nimhd.nih.gov.


# Methods

The analysis includes:

Exploratory data analysis of social determinants by county in the state of Idaho.

All analyses are conducted in R using RStudio.

# Repository Structure
MPH/Idaho
│
├── data/        # Raw and processed datasets   
├── analysis/     # R scripts for data cleaning and analysis
├── report/      # R Markdown report
└── README.md    # Project overview

# Reproducibility

To reproduce the analysis:

Clone the repository

Open the project in RStudio

Run the analysis scripts or render the R Markdown report.

# Notes

Author: Alexandrine Bilwes Crane
Cornell University
ac292@cornell.edu

Project developed as part of an MPH data analysis project.
A.I. Disclosure: A.I. was not used.
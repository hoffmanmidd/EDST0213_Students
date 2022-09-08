# Inputing, Tidying and Wrangling Thorndike pp. 23 - 28

## CLEAR WORKSPACE ####
rm(list=ls())

## LOAD PACKAGES ####
library(tidyverse)

## READ IN DATA ####
Table.2.1 <- read_csv(file = "Table.2.1.csv")

## CLEAN DATA

# Create a tibble (coerce the data into tidyverse data frame)
as_tibble(Table.2.1)

# Make column "Gender" into factors and label it
Table.2.1 = Table.2.1 %>%
  mutate(Gender = factor(Gender, levels=c("1", "2"), labels=c("male", "female")))

# Make column "Class" into factors and label
Table.2.1 = Table.2.1 %>%
  mutate(Class = factor(Class, levels=c("1", "2"), labels=c("Johnson", "Cordero")))

# Coerce scores to be integers
Table.2.1  <- Table.2.1 %>% 
  mutate(Reading = as.integer(Reading), Spelling = as.integer(Spelling), Math = as.integer(Math))

# Look at the structure of the data
str(Table.2.1)

# display summary on your console
summary(Table.2.1) 

# Write csv file of cleaned up Thorndike table
write_csv(Table.2.1, "Table.2.1_clean.csv")

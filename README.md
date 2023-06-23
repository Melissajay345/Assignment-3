---
title: "ANA 515 Assignment 3"
author: "Melissa Jayawardane"
output: word_document
date: "2023-06-22"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE,message = FALSE,warning = FALSE)
```

## Getting and Cleaning Data
```{r, echo=FALSE}
# Step 1
# Set the working directory
setwd("C:\\Users\\user\\Documents\\ANA 515")

# Read the file from the local directory
data <- read.csv("StormEvents_details-ftp_v1.0_d2004_c20220425.csv")

# Step 2
## Using subset() function to select columns
subset.data <- subset(data, select = c("BEGIN_YEARMONTH", "EPISODE_ID", "STATE", "STATE_FIPS", "CZ_NAME", "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE"))

# Load libraries
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

# Step 3
# Arrange the data by the state name (STATE)
arranged_data <- arrange(subset.data, STATE)
# Step 4
# Change state and county names to title case
arranged_data$STATE <- str_to_title(arranged_data$STATE)
arranged_data$CZ_NAME <- str_to_title(arranged_data$CZ_NAME)
# Step 5
# Limit to events listed by county FIPS (CZ_TYPE of "C")
filtered_data <- filter(arranged_data, CZ_TYPE == "C")

# Remove the CZ_TYPE column
filtered_data <- select(filtered_data, -CZ_TYPE)

# Step 6
# Pad state FIPS with "0" at the beginning
filtered_data$STATE_FIPS <- str_pad(filtered_data$STATE_FIPS, width = 2, pad = "0")

# Pad county FIPS with "0" at the beginning
filtered_data$CZ_FIPS <- str_pad(filtered_data$CZ_FIPS, width = 3, pad = "0")

# Unite state and county FIPS columns into one FIPS column
filtered_data <- unite(filtered_data, FIPS, STATE_FIPS, CZ_FIPS, sep = "-")

# Step 7
# Change all column names to lowercase
filtered_data <- rename_all(filtered_data, tolower)

## Step 8
# Load the state data from base R
data("state")

# Create a dataframe with state name, area, and region
state_df <- data.frame(state.name = state.name, area = state.area, region = state.region)

## Step 9 
# Assuming 'limited_data' contains the filtered and limited data from step 5
# Assuming 'state_df' contains the state information dataframe created in step 8

# Create a dataframe with the number of events per state in the year 2004
year <- 2004
events_per_state <- filtered_data %>%
  filter(substr(begin_yearmonth, 1, 4) == as.character(year)) %>%
  group_by(state) %>%
  summarise(events = n())

# Merge the state information dataframe with the events per state dataframe
merged_df <- merge(state_df, events_per_state, by.x = "state.name", by.y = "state", all.x = TRUE)

# Remove states that are not in the state information dataframe
merged_df <- merged_df[complete.cases(merged_df), ]

```
```{r}
# View Structure of Cleaned data
str(merged_df)
```


```{r}
## Step 10
# Create the scatterplot
ggplot(merged_df, aes(x = area, y = events, color = region)) +
  geom_point() +
  labs(x = "Land Area (sq. miles)", y = "Number of Storm Events (2017)", color = "Region") +
  ggtitle("Number of Storm Events in 2017 vs. Land Area") +
  theme_minimal()
```

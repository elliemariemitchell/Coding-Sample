# CODING SAMPLE -----------------------------------------------------------

# For this sample, I'm using code I created for one of the RA projects I've 
# been working on for the better part of a year. This project works with the 
# American Family Survey. 2024 was the 10th annual year my professors have run
# this survey, and my task was to compile each year into one large cumulative
# dataset. Over the course of the project, I also created my own variables that
# were then used in analysis that I did. Since I've been working on this project
# for almost a year, I've created tens of thousands of lines of code, which
# obviously is much too much to include in a coding sample. Because of this,
# I've just included small parts of each step of the project to showcase my 
# abilities.

#### Getting relevant packages ------------------------------------------------
library(tidyverse)
library(haven)
library(labelled)
library(powerjoin)
library(dplyr)
library(srvyr)
library(stargazer)

# Data Merging 2015 - 2024 -------------------------------------------------
#### Retrieving data ----------------------------------------------------------
file_path <- "/Users/rental/Library/CloudStorage/Box-Box/American Family Survey/"
chart_path <- "/Users/rental/Desktop/Coding Sample/Charts/"

data_15 <- read_dta(str_c(file_path,
                          "2015 American Family Survey/Data/Other Datasets/BYUC0007_OUTPUT- 2015-08-28 download with weights.dta"))

data_16 <- read_dta(str_c(file_path,
                          "2016 American Family Survey/Data/2016 Original Dataset.dta"))

data_17 <- read_dta(str_c(file_path,
                          "2017 American Family Survey/Data/2017 Original Dataset.dta"))

data_18 <- read_dta(str_c(file_path,
                          "2018 American Family Survey/Data/BYUC0016/BYUC0016_OUTPUT.dta"))

data_19 <- read_dta(str_c(file_path,
                          "2019 American Family Survey/Data/BYUC0021_OUTPUT.dta"))

data_20 <- read_dta(str_c(file_path,
                          "2020 American Family Survey/Data/Untouched Dataset/BYUC0024_OUTPUT.dta"))

data_21 <- read_dta(str_c(file_path,
                          "2021 American Family Survey/Data/BYUC0028_OUTPUT.dta"))

data_22 <- read_dta(str_c(file_path,
                          "2022 American Family Survey/data/BYUC0030_OUTPUT.dta"))

data_23 <- read_dta(str_c(file_path,
                          "2023 American Family Survey/Data/BYUC0034_OUTPUT.dta"))

data_24 <- read_dta(str_c(file_path,
                          "2024 American Family Survey/Data/AFS2024.dta"))

#### Cleaning each dataset ------------------------------------------------
# 2015
# Creating a year variable
data_15 <- data_15 %>%
  mutate(yeartaken = 2015)

# 2016
# Removing the prefix
names(data_16)
data_16 <- data_16 %>%
  rename_with(~gsub("s16_", "", .), starts_with("s16_"))

# Creating a year variable
data_16 <- data_16 %>%
  mutate(yeartaken = 2016)

# 2017
# Removing the prefix
names(data_17)
data_17 <- data_17 %>%
  rename_with(~gsub("s17_", "", .), starts_with("s17_")) %>%
  rename_with(~gsub("S17_", "", .), starts_with("S17_"))

# Creating a year variable
data_17 <- data_17 %>%
  mutate(yeartaken = 2017)

# 2018
# Removing the prefix
names(data_18)
data_18 <- data_18 %>%
  rename_with(~gsub("s18_", "", .), starts_with("s18_"))

# Creating a year variable
data_18 <- data_18 %>%
  mutate(yeartaken = 2018)

# 2019
# Removing the prefix
names(data_19)
data_19 <- data_19 %>%
  rename_with(~gsub("s19_", "", .), starts_with("s19_"))

# Creating a year variable
data_19 <- data_19 %>%
  mutate(yeartaken = 2019)

# 2020
# Removing the prefix
names(data_20)
data_20 <- data_20 %>%
  rename_with(~gsub("s20_", "", .), starts_with("s20_")) %>%
  rename_with(~gsub("s19_", "", .), starts_with("s19_")) %>%
  rename_with(~gsub("s2_", "", .), starts_with("s2_"))

# Creating a year variable 
data_20 <- data_20 %>%
  mutate(yeartaken = 2020)

# 2021
# Removing the prefix
names(data_21)
data_21 <- data_21 %>%
  rename_with(~gsub("s21_", "", .), starts_with("s21_"))

# Creating a year variable
data_21 <- data_21 %>%
  mutate(yeartaken = 2021)

# 2022
# Removing the prefix
names(data_22)
data_22 <- data_22 %>%
  rename_with(~gsub("s22_", "", .), starts_with("s22_"))

# Creating a year variable
data_22 <- data_22 %>%
  mutate(yeartaken = 2022)

# 2023
# Creating a year variable where for the whole dataset, year is 2023
data_23 <- data_23 %>%
  mutate(yeartaken = 2023)

# 2024
# Removing the prefix
names(data_24)
# Strangely, the prefix is s22; I'm removing it
data_24 <- data_24 %>%
  rename_with(~gsub("s22_", "", .), starts_with("s22_"))

# Creating a year variable where for the whole dataset, year is 2024
data_24 <- data_24 %>%
  mutate(yeartaken = 2024)

#### Making sure each variable is consistent across the years --------------
# In some years, the same question is under several different variable names;
# we need to make these consistent in order to correctly do analysis over time.
# Additionally, sometimes the question actually changes from year to year while
# the variable stays the same; obviously we need to change this in order to 
# correctly do analysis. this section fixes all those problems; it aligns 
# everything to what is in the "AFS Complete Question Database.docx" I've 
# created.

# Fixing MSC003 - options changed between 2018 and 2020. Using the 2020 version.
data_15 <- data_15 %>%
  mutate(MSC003 = case_when(MSC003 >= 62 ~ 62,
                            TRUE ~ MSC003))

data_16 <- data_16 %>%
  mutate(MSC003 = case_when(MSC003 >= 62 ~ 62,
                            TRUE ~ MSC003))

data_17 <- data_17 %>%
  mutate(MSC003 = case_when(MSC003 >= 62 ~ 62,
                            TRUE ~ MSC003))

data_18 <- data_18 %>%
  mutate(MSC003 = case_when(MSC003 == 62 ~ 62,
                            TRUE ~ MSC003))
data_22 <- data_22 %>%
  mutate(MSC003 = case_when(MSC003 == 99 ~ NA,
                            TRUE ~ MSC003))
# 99 just means the question wasn't asked, so I'm converting that to NA here.

# MAR001: Some of the variable names are correlated with the same number, so 
# we need to change those
data_19 <- data_19 %>%
  rename(MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3)

data_20 <- data_20 %>%
  rename(MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3)

data_21 <- data_21 %>%
  rename(MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3)

data_22 <- data_22 %>%
  rename(MAR001_6 = MAR001_5,
         MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3)

data_23 <- data_23 %>%
  rename(MAR001_7 = MAR001_5,
         MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3)

data_24 <- data_24 %>%
  rename(MAR001_7 = MAR001_5,
         MAR001_5 = MAR001_4,
         MAR001_4 = MAR001_3,
         MAR001_3 = MAR001_8)

# MSC014_c: In some years, the variable goes to 9+ and in some years it goes to 
# 20+. Need to fix this so they're all comparable by just coding to 9+ each year
data_22 <- data_22 %>%
  mutate(MSC014_c = if_else(MSC014_c >= 9,
                            9,
                            MSC014_c))

data_23 <- data_23 %>%
  mutate(MSC014_c = if_else(MSC014_c >= 9,
                            9,
                            MSC014_c))

data_24 <- data_24 %>%
  mutate(MSC014_c = if_else(MSC014_c >= 9,
                            9,
                            MSC014_c))

#### Fixing variable classes ------------------------------------------------
data_17 <- data_17 %>%
  mutate(inputzip = as.numeric(inputzip))

data_20 <- data_20 %>%
  mutate(PAR008_1 = as.character(PAR008_1),
         PAR008_2 = as.character(PAR008_2),
         PAR008_3 = as.character(PAR008_3),
         PAR008_4 = as.character(PAR008_4),
         PAR008_7 = as.character(PAR008_7))

data_21 <- data_21 %>%
  mutate(starttime = as.Date(starttime),
         endtime = as.Date(endtime))

data_22 <- data_22 %>%
  mutate(starttime = as.Date(starttime),
         endtime = as.Date(endtime))

data_24 <- data_24 %>%
  mutate(starttime = as.Date(starttime),
         endtime = as.Date(endtime))

#### Fixing variable labels ------------------------------------------------
# MAR001_1
val_label(data_15$MAR001_1, 3) <- "Neutral/Don't know"

val_label(data_18$MAR001_1, 8) <- "Skipped"
val_label(data_18$MAR001_1, 9) <- "Not Asked"

data_19 <- data_19 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))

val_label(data_19$MAR001_1, 1) <- "Completely satisfied"
val_label(data_19$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_19$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_19$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_19$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_19$MAR001_1, 6) <- "Not applicable"

data_20 <- data_20 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))
val_label(data_20$MAR001_1, 1) <- "Completely satisfied"
val_label(data_20$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_20$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_20$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_20$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_20$MAR001_1, 6) <- "Not applicable"

data_21 <- data_21 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))
val_label(data_21$MAR001_1, 1) <- "Completely satisfied"
val_label(data_21$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_21$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_21$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_21$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_21$MAR001_1, 6) <- "Not applicable"

data_22 <- data_22 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))
val_label(data_22$MAR001_1, 1) <- "Completely satisfied"
val_label(data_22$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_22$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_22$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_22$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_22$MAR001_1, 6) <- "Not applicable"
val_label(data_22$MAR001_1, 8) <- "Skipped"

data_23 <- data_23 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))
val_label(data_23$MAR001_1, 1) <- "Completely satisfied"
val_label(data_23$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_23$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_23$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_23$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_23$MAR001_1, 6) <- "Not applicable"

data_24 <- data_24 %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 1 ~ 5,
                              MAR001_1 == 2 ~ 4,
                              MAR001_1 == 3 ~ 3,
                              MAR001_1 == 4 ~ 2,
                              MAR001_1 == 5 ~ 1,
                              MAR001_1 == 6 ~ 6,
                              MAR001_1 == 8 ~ 8,
                              MAR001_1 == 9 ~ 9,
                              TRUE ~ NA))
val_label(data_24$MAR001_1, 1) <- "Completely satisfied"
val_label(data_24$MAR001_1, 2) <- "Somewhat satisfied"
val_label(data_24$MAR001_1, 3) <- "Neutral/Don't know"
val_label(data_24$MAR001_1, 4) <- "Somewhat dissatisfied"
val_label(data_24$MAR001_1, 5) <- "Completely dissatisfied"
val_label(data_24$MAR001_1, 6) <- "Not applicable"

# MSC003
val_label(data_21$MSC003, 13) <- "1 years"

val_label(data_22$MSC003, 13) <- "1 years"

val_label(data_23$MSC003, 13) <- "1 years"

# MSC014_c
data_22 <- data_22 %>%
  mutate(MSC014_c = case_when(MSC014_c == 1 ~ 1,
                              MSC014_c == 2 ~ 2,
                              MSC014_c == 3 ~ 3,
                              MSC014_c == 4 ~ 4,
                              MSC014_c == 5 ~ 5,
                              MSC014_c == 6 ~ 6,
                              MSC014_c == 7 ~ 7,
                              MSC014_c == 8 ~ 8,
                              MSC014_c == 9 ~ 9,
                              MSC014_c == 10 ~ 9,
                              MSC014_c == 11 ~ 9,
                              MSC014_c == 12 ~ 9,
                              MSC014_c == 13 ~ 9,
                              MSC014_c == 14 ~ 9,
                              MSC014_c == 15 ~ 9,
                              MSC014_c == 16 ~ 9,
                              MSC014_c == 17 ~ 9,
                              MSC014_c == 18 ~ 9,
                              MSC014_c == 19 ~ 9,
                              MSC014_c == 20 ~ 9,
                              TRUE ~ NA))
val_label(data_22$MSC014_c, 9) <- "9+"

data_23 <- data_23 %>%
  mutate(MSC014_c = case_when(MSC014_c == 1 ~ 1,
                              MSC014_c == 2 ~ 2,
                              MSC014_c == 3 ~ 3,
                              MSC014_c == 4 ~ 4,
                              MSC014_c == 5 ~ 5,
                              MSC014_c == 6 ~ 6,
                              MSC014_c == 7 ~ 7,
                              MSC014_c == 8 ~ 8,
                              MSC014_c == 9 ~ 9,
                              MSC014_c == 10 ~ 9,
                              MSC014_c == 11 ~ 9,
                              MSC014_c == 12 ~ 9,
                              MSC014_c == 13 ~ 9,
                              MSC014_c == 14 ~ 9,
                              MSC014_c == 15 ~ 9,
                              MSC014_c == 16 ~ 9,
                              MSC014_c == 17 ~ 9,
                              MSC014_c == 18 ~ 9,
                              MSC014_c == 19 ~ 9,
                              MSC014_c == 20 ~ 9,
                              TRUE ~ NA))
val_label(data_23$MSC014_c, 9) <- "9+"

#### Creating a cumulative file from 2015 - 2024 ------------------------------
# cumulative_data <- bind_rows(data_15,
#                              data_16,
#                              data_17,
#                              data_18,
#                              data_19,
#                              data_20,
#                              data_21,
#                              data_22,
#                              data_23,
#                              data_24)

# This is commented out b/c it won't work since I haven't included all of the 
# code in the previous sections to actually bind these datasets together 
# correctly.

#### Saving out the cumulative data file --------------------------------------
# write.csv(cumulative_data,
#           str_c(file_path,
#                 "Cumulative File/Script 2015-2024/CumulativeFile.csv"),
#           row.names = FALSE)

# Also commented out b/c I don't actually want to save out that file



# Making a Canonical Dataset for 2015-2023 ------------------------------
#### Retrieving Data ----------------------------------------------------------
cumulative_file <- read_dta(str_c(file_path,
                                  "Cumulative File/Script 2015-2024/CumulativeFile.dta"))

#### Creating Canonical Variables ---------------------------------------------
# Age Category Variable
cumulative_file <- cumulative_file %>%
  mutate(age = (yeartaken - birthyr),
         agecategory = case_when(age >= 18 & age <= 29 ~ "18-29",
                                 age >= 30 & age <= 44 ~ "30-44",
                                 age >= 45 & age <= 54 ~ "45-54",
                                 age >= 55 & age <= 65 ~ "55-64",
                                 age >= 65 ~ "65+",
                                 TRUE ~ NA)) 
var_label(cumulative_file$agecategory) <- "Age Categories"
var_label(cumulative_file$agecategory)

# Party ID 7 Point Scale
cumulative_file <- cumulative_file %>%
  mutate(partyid7 = case_when(PID001 == 1 & PID002 == 1 ~ 1,
                              PID001 == 1 & PID002 == 2 ~ 2,
                              (PID001 == 3 | PID001 == 4) & PID003 == 1 ~ 3,
                              (PID001 == 3 | PID001 == 4) & PID003 == 3 ~ 4,
                              PID001 == 4 ~ 4,
                              (PID001 == 3 | PID001 == 4) & PID003 == 2 ~ 5,
                              PID001 == 2 & PID002 == 2 ~ 6,
                              PID001 == 2 & PID002 == 1 ~ 7,
                              TRUE ~ NA))
var_label(cumulative_file$partyid7) <- "7 Point PID"
val_labels(cumulative_file$partyid7) <- c("Strong Republican" = 1,
                                          "Weak Republican" = 2,
                                          "Lean Republican" = 3,
                                          "Independent" = 4,
                                          "Lean Democrat" = 5,
                                          "Weak Democrat" = 6,
                                          "Strong Democrat" = 7)

# Party ID 3 Point Scale where those that lean are included in whichever party
# they lean towards
cumulative_file <- cumulative_file %>%
  mutate(partyid3 = case_when(partyid7 == 1 | partyid7 == 2 | partyid7 == 3 ~ 1,
                              partyid7 == 4 ~ 2,
                              partyid7 == 5 | partyid7 == 6 | partyid7 == 7 ~ 3,
                              TRUE ~ NA))
var_label(cumulative_file$partyid3) <- "3 Point PID"
val_labels(cumulative_file$partyid3) <- c("Republican" = 1,
                                          "Independent" = 2,
                                          "Democrat" = 3)

### Saving out the canonical data file --------------------------------------
# write.csv(cumulative_file,
#           str_c(file_path,
#                 "Cumulative File/Script 2015-2024/CanonicalFile.csv"),
#           row.names = FALSE)

# Again, I don't actually want to save this out as-is, so I'm commenting it out
# so that this part doesn't run.

# Creating Time Trend Figures ----------------------------------------------
#### Retrieving data ----------------------------------------------------------
canonical_data <- read_dta(str_c(file_path,
                                 "Cumulative File/Script 2015-2024/CanonicalFile.dta"))

# TIME TREND GRAPHS -------------------------------------------------------
# Satisfaction Battery ----------------------------------------------------
# The NAs are written in as "Not Applicable" instead of NA, which throws the
# percents off. I'm fixing that before I do anything else.
canonical_data <- canonical_data %>%
  mutate(MAR001_1 = case_when(MAR001_1 == 6 ~ NA,
                              MAR001_1 == 8 ~ NA,
                              TRUE ~ MAR001_1),
         MAR001_2 = case_when(MAR001_2 == 6 ~ NA,
                              MAR001_2 == 8 ~ NA,
                              TRUE ~ MAR001_2),
         MAR001_3 = case_when(MAR001_3 == 6 ~ NA,
                              MAR001_3 == 8 ~ NA,
                              TRUE ~ MAR001_3),
         MAR001_4 = case_when(MAR001_4 == 6 ~ NA,
                              MAR001_4 == 8 ~ NA,
                              TRUE ~ MAR001_4),
         MAR001_5 = case_when(MAR001_5 == 6 ~ NA,
                              MAR001_5 == 8 ~ NA,
                              TRUE ~ MAR001_5),
         MAR001_6 = case_when(MAR001_6 == 6 ~ NA,
                              MAR001_6 == 8 ~ NA,
                              TRUE ~ MAR001_6),
         MAR001_7 = case_when(MAR001_7 == 6 ~ NA,
                              MAR001_7 == 8 ~ NA,
                              TRUE ~ MAR001_7))

satisfaction <- canonical_data %>%
  select(
    yeartaken,
    starts_with("MAR001_"),
    weight
  ) %>%
  mutate(
    across(
      -c(yeartaken, weight),
      ~to_factor(.) %>%
        as.character()
    )
  ) %>%
  pivot_longer(
    cols = -c(yeartaken, weight)
  ) %>%
  group_by(
    yeartaken,
    name,
    value
  ) %>%
  filter(!is.na(value)) %>%
  summarise(n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(
    yeartaken,
    name
  ) %>%
  mutate(
    result = (n / sum(n)) %>%
      round(2),
    label = value,
    value = case_when(
      label == 0 ~ 0,
      TRUE ~ 1
    ),
    result = (result*100)
  ) %>%
  filter(value == 1,
         !is.na(label)) %>%
  rename(
    variable = name,
    group_var = yeartaken
  ) %>%
  select(
    group_var,
    variable,
    value,
    label,
    n,
    result
  )

# Right now, we have the results for all 5 of the possible answers (completely
# satisfied, somewhat satisfied, neutral/don't know, somewhat dissatisfied, and
# completely dissatisfied). I want to report ONLY on people who are satisfied
# (either completely or somewhat) with each metric. Here, I filter the other
# possible answers out
satisfaction <- satisfaction %>%
  filter(label != "Completely dissatisfied" & label != "Neutral/Don't know" & label != "Somewhat dissatisfied" & label != "Neither satisfied nor dissatisfied")

# Next, I add the responses who indicated they were completely satisfied and the
# responses indicating they were somewhat satisfied together, making one
# total satisfaction variable
satisfaction <- satisfaction %>%
  group_by(group_var,
           variable) %>%
  mutate(sum_satis = sum(result),
         sum_ns = sum(n)) %>%
  select(-n)

# I now filter out one of the satisfaction variables since the two are summed
# together and I don't need two identical vars for each point.
satisfaction <- satisfaction %>%
  filter(label == "Completely satisfied")

# I'm removing the questions that only got asked in a year or two, as they don't
# really make sense to have in a time trend graph.
satisfaction <- satisfaction %>%
  filter(variable != "MAR001_6")

satisfaction <- satisfaction %>%
  filter(variable != "MAR001_7")

# Now I graph!
satisfaction_grph <- ggplot(satisfaction, aes(x = group_var,
                                              y = sum_satis,
                                              color = variable,
                                              group = variable)) +
  geom_line() +
  geom_point(size = 7) +
  geom_text(aes(label = sum_satis),
            color = "white",
            size = 3.5,
            vjust = 0.4) +
  labs(title = "How Satisfied are You With Your... (2015-2024)",
       x = "",
       y = "Percentage",
       caption = "Graph shows the percentage of people indicating they were completely or somewhat satisfied with each metric",
       color = "") +
  scale_color_manual(values = c("#531A19",
                                "#7B2022",
                                "#A7232B",
                                "#d42434",
                                "#E45C58")) +
  scale_y_continuous(limits = c(0, 87)) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 7),
        legend.position = "none") +
  annotate("text",
           x = 2025,
           y = 58,
           label = "Job",
           color = "#531A19",
           size = 2.5) +
  annotate("text",
           x = 2025,
           y = 78,
           label = "Family",
           color = "#7B2022",
           size = 2.5) +
  annotate("text",
           x = 2025,
           y = 84.5,
           label = "Marriage or",
           color = "#A7232B",
           size = 2.5,
           hjust = 0.6) +
  annotate("text",
           x = 2025,
           y = 81.5,
           label = "Relationship",
           color = "#A7232B",
           size = 2.5,
           hjust = 0.6) +
  annotate("text",
           x = 2025,
           y = 69,
           label = "Life",
           color = "#d42434",
           size = 2.5) +
  annotate("text",
           x = 2025,
           y = 61,
           label = "Community",
           color = "#E45C58",
           size = 2.5,
           hjust = 0.6)

satisfaction_grph

ggsave(str_c(chart_path, "satisfaction.png"),
       plot = satisfaction_grph,
       width = 6,
       height = 4,
       dpi = 300,
       bg = "white")

# Satisfaction battery by partisanship ------------------------------------
satisfaction_pid3 <- canonical_data %>%
  select(
    yeartaken,
    starts_with("MAR001_"),
    weight,
    partyid3
  ) %>%
  mutate(
    across(
      -c(yeartaken, 
         weight,
         partyid3),
      ~to_factor(.) %>%
        as.character()
    )
  ) %>%
  pivot_longer(
    cols = -c(yeartaken, 
              weight,
              partyid3)
  ) %>%
  group_by(
    yeartaken,
    partyid3,
    name,
    value
  ) %>%
  filter(!is.na(value)) %>%
  summarise(n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(
    yeartaken,
    partyid3,
    name
  ) %>%
  mutate(
    result = (n / sum(n)) %>%
      round(2),
    label = value,
    value = case_when(
      label == 0 ~ 0,
      TRUE ~ 1
    ),
    result = (result*100)
  ) %>%
  filter(value == 1,
         !is.na(label),
         !is.na(partyid3)) %>%
  rename(
    variable = name,
    group_var = yeartaken
  ) %>%
  select(
    group_var,
    partyid3,
    variable,
    value,
    label,
    n,
    result
  ) %>%
  mutate(partyid3 = case_when(partyid3 == 1 ~ "Republican",
                              partyid3 == 2 ~ "Independent",
                              partyid3 == 3 ~ "Democrat"),
         partyid3 = factor(partyid3, levels = c("Independent",
                                                "Republican",
                                                "Democrat")))

# Right now, we have the results for all 5 of the possible answers (completely
# satisfied, somewhat satisfied, neutral/don't know, somewhat dissatisfied, and
# completely dissatisfied). I want to report ONLY on people who are satisfied
# (either completely or somewhat) with each metric. Here, I filter the other
# possible answers out
satisfaction_pid3 <- satisfaction_pid3 %>%
  filter(label != "Completely dissatisfied" & label != "Neutral/Don't know" & label != "Somewhat dissatisfied" & label != "Neither satisfied nor dissatisfied")

# Next, I add the responses who indicated they were completely satisfied and the
# responses indicating they were somewhat satisfied together, making one
# total satisfaction variable
satisfaction_pid3 <- satisfaction_pid3 %>%
  group_by(group_var,
           variable,
           partyid3) %>%
  mutate(sum_satis = sum(result),
         sum_ns = sum(n)) %>%
  select(-n)

# I now filter out one of the satisfaction variables since the two are summed
# together and I don't need two identical vars for each point.
satisfaction_pid3 <- satisfaction_pid3 %>%
  filter(label == "Completely satisfied")

# Since we're looking at these variables using partisanship, I'm going to 
# separate them out into individual smaller datasets that each look at just one
# question
satisfaction_job <- satisfaction_pid3 %>%
  filter(variable == "MAR001_1")
# Obviously, I'm just going to do one for this coding sample

# Making a graph for job satisfaction now
satisfaction_job_grph <- ggplot(satisfaction_job, aes(x = group_var,
                                                      y = sum_satis,
                                                      color = partyid3,
                                                      group = partyid3)) +
  geom_line() +
  geom_point(size = 7) +
  geom_text(aes(label = sum_satis),
            color = "white",
            size = 3.5,
            vjust = 0.4) +
  labs(title = "How Satisfied are You With Your Job, by Partisanship (2015-2024)",
       x = "",
       y = "Percentage",
       caption = "Graph shows the percentage of people indicating they were completely or somewhat satisfied with this metric",
       color = "") +
  scale_color_manual(values = c("#dcddde",
                                "#d42133",
                                "#282965")) +
  scale_y_continuous(limits = c(0, 73)) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 7),
        legend.position = "none") +
  annotate("text",
           x = 2024.75,
           y = 65,
           label = "Republican",
           color = "#d42133",
           size = 2) +
  annotate("text",
           x = 2024.75,
           y = 47,
           label = "Independent",
           color = "#dcddde",
           size = 2) + 
  annotate("text",
           x = 2024.75,
           y = 60,
           label = "Democrat",
           color = "#282965",
           size = 2)

satisfaction_job_grph

ggsave(str_c(chart_path, "satisfaction_job.png"),
       plot = satisfaction_job_grph,
       width = 6,
       height = 4,
       dpi = 300,
       bg = "white")

# Family satisfaction by happy relationship -------------------------------
# creating a new variable for happy/healthy relationship
canonical_data <- canonical_data %>%
  mutate(happy_relat = case_when(mar_introuble == 1 | MSC001 %in% c(2, 5) ~ 0,
                                 mar_introuble != 1 & !(MSC001 %in% c(2, 5)) ~ 1,
                                 TRUE ~ NA))

# rerunning the results for satisfaction as a whole, using the new variable
satisfaction_family_with_happy_relat <- canonical_data %>%
  select(
    yeartaken,
    MAR001_2,
    happy_relat,
    weight
  ) %>%
  mutate(
    across(
      -c(yeartaken, 
         weight,
         happy_relat),
      ~to_factor(.) %>%
        as.character()
    )
  ) %>%
  pivot_longer(
    cols = -c(yeartaken, 
              weight,
              happy_relat)
  ) %>%
  group_by(
    yeartaken,
    happy_relat,
    name,
    value
  ) %>%
  filter(!is.na(value)) %>%
  summarise(n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(
    yeartaken,
    happy_relat,
    name
  ) %>%
  mutate(
    result = (n / sum(n)) %>%
      round(2),
    label = value,
    value = case_when(
      label == 0 ~ 0,
      TRUE ~ 1
    ),
    result = (result*100)
  ) %>%
  filter(value == 1,
         !is.na(label),
         !is.na(happy_relat)) %>%
  rename(
    variable = name,
    group_var = yeartaken
  ) %>%
  select(
    group_var,
    happy_relat,
    variable,
    value,
    label,
    n,
    result
  )  %>%
  mutate(happy_relat = case_when(happy_relat == 0 ~ "Not in a happy/healthy relationship",
                                 happy_relat == 1 ~ "In a happy/healthy relationship"),
         happy_relat = factor(happy_relat, levels = c("In a happy/healthy relationship",
                                                      "Not in a happy/healthy relationship")))

# Right now, we have the results for all 5 of the possible answers (completely
# satisfied, somewhat satisfied, neutral/don't know, somewhat dissatisfied, and
# completely dissatisfied). I want to report ONLY on people who are satisfied
# (either completely or somewhat) with each metric. Here, I filter the other
# possible answers out
satisfaction_family_with_happy_relat <- satisfaction_family_with_happy_relat %>%
  filter(label != "Completely dissatisfied" & label != "Neutral/Don't know" & label != "Somewhat dissatisfied" & label != "Neither satisfied nor dissatisfied")

# Next, I add the responses who indicated they were completely satisfied and the
# responses indicating they were somewhat satisfied together, making one
# total satisfaction variable
satisfaction_family_with_happy_relat <- satisfaction_family_with_happy_relat %>%
  group_by(group_var,
           happy_relat,
           variable) %>%
  mutate(sum_satis = sum(result),
         sum_ns = sum(n)) %>%
  select(-n)

# I now filter out one of the satisfaction variables since the two are summed
# together and I don't need two identical vars for each point.
satisfaction_family_with_happy_relat <- satisfaction_family_with_happy_relat %>%
  filter(label == "Completely satisfied")

# Now I graph!
satisfaction_family_with_happy_relat_grph <- ggplot(satisfaction_family_with_happy_relat, aes(x = group_var,
                                                                                              y = sum_satis,
                                                                                              color = happy_relat,
                                                                                              group = happy_relat)) +
  geom_line() +
  geom_point(size = 7) +
  geom_text(aes(label = sum_satis),
            color = "white",
            size = 3.5,
            vjust = 0.4) +
  labs(title = "How Satisfied are You With Your Family? (2015-2024)",
       x = "",
       y = "Percentage",
       caption = "Graph shows the percentage of people indicating they were completely or somewhat satisfied with each metric",
       color = "") +
  scale_color_manual(values = c("#476097",
                                "#e3c063")) +
  scale_y_continuous(limits = c(0, 95)) +
  scale_x_continuous(breaks = seq(2015, 2024, by = 1)) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 7),
        legend.position = "none") +
  annotate("text",
           x = 2024.75,
           y = 92,
           label = "In a happy and",
           color = "#476097",
           size = 1.5) +
  annotate("text",
           x = 2024.75,
           y = 90,
           label = "healthy relationship",
           color = "#476097",
           size = 1.5) +
  annotate("text",
           x = 2024.75,
           y = 72,
           label = "Not in a happy and",
           color =  "#e3c063",
           size = 1.5) +
  annotate("text",
           x = 2024.75,
           y = 70,
           label = "healthy relationship",
           color =  "#e3c063",
           size = 1.5)

satisfaction_family_with_happy_relat_grph

ggsave(str_c(chart_path, "satisfaction_family_with_happy_relat.png"),
       plot = satisfaction_family_with_happy_relat_grph,
       width = 6,
       height = 4,
       dpi = 300,
       bg = "white")

# Strength of own marriage for those who are married or married but separated ----
strength_marriage_cont <- canonical_data %>%
  filter(MSC001 == 1 | MSC001 == 2) %>%
  select(
    yeartaken,
    MAR006,
    weight
  ) %>%
  mutate(
    across(
      -c(yeartaken, weight),
      ~to_factor(.) %>%
        as.character()
    )
  ) %>%
  pivot_longer(
    cols = -c(yeartaken, weight)
  ) %>%
  group_by(
    yeartaken,
    name,
    value
  ) %>%
  filter(!is.na(value)) %>%
  summarise(n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(
    yeartaken,
    name
  ) %>%
  mutate(
    result = (n / sum(n)) %>%
      round(2),
    label = value,
    value = case_when(
      label == 0 ~ 0,
      TRUE ~ 1
    ),
    result = (result*100)
  ) %>%
  filter(value == 1,
         !is.na(label)) %>%
  rename(
    variable = name,
    group_var = yeartaken
  ) %>%
  select(
    group_var,
    variable,
    value,
    label,
    n,
    result
  ) %>%
  mutate(label = factor(label, levels = c("Don't know",
                                          "Weaker",
                                          "About the same",
                                          "Stronger"))) %>%
  filter(!is.na(label))

# Creating the bar chart
strength_mar_cont_bar_grph <- ggplot(strength_marriage_cont, aes(x = label,
                                                                 y = result,
                                                                 fill = label)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  geom_text(aes(label = result),
            color = "black",
            size = 3.5,
            vjust = 0.5,
            hjust = -0.6,
            position = position_dodge(width = 0.9)) +
  labs(title = "Respondent's Feelings about the Strength of \nTheir Marriage, 2015-2024",
       x = "",
       y = "Percentage",
       caption = "This graph only includes those who are currently married or currently married but separated from their partners",
       fill = "") +
  scale_fill_manual(values = c("#dcdddf",
                               "#7D76A0",
                               "#544D82",
                               "#282764")) +
  scale_y_continuous(limits = c(0, 70)) +
  theme_minimal() +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.caption = element_text(hjust = 0,
                                    size = 7),
        legend.position = "top",
        axis.text.y = element_blank(),
        axis.text.x = element_blank()) +
  facet_wrap(~ group_var,
             ncol = 5)

strength_mar_cont_bar_grph

ggsave(str_c(chart_path, "strength_own_mar_cont_BAR.png"),
       plot = strength_mar_cont_bar_grph,
       width = 6,
       height = 4,
       dpi = 300,
       bg = "white")

# CHECKING N-SIZE CALCULATOR FUNCTION -------------------------------------
# This function finds any object in the environment with both an 'n' variable 
# name and, within that name, an n size smaller than thirty. It then compiles it
# into a list to help me find all the datasets I need to recheck.
 
find_datasets_with_small_n <- function() {   
  object_names <- ls(envir = .GlobalEnv)    
  
  datasets_with_small_n <- list()    
  
  for (obj_name in object_names) {     
    obj <- get(obj_name, envir = .GlobalEnv)      
    
    if (is.data.frame(obj)) {       
      if (("n" %in% colnames(obj) && any(obj$n <= 30, na.rm = TRUE)) || 
          ("sum_ns" %in% colnames(obj) && any(obj$sum_ns <= 30, na.rm = TRUE))) {         
        datasets_with_small_n <- c(datasets_with_small_n, obj_name)       
      }     
    }   
  }    
  
  return(datasets_with_small_n) 
}  

# Using the function
datasets_with_small_n <- find_datasets_with_small_n() 
print(datasets_with_small_n)

#### END ----------------------------------------------------------------------
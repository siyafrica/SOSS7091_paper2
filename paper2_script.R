## package to read in SPSS data from Afrobarometer
library(haven)

## Tidyverse packages for data analysis and visualization
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(vcd)
library(grid)

## Read in the data 
ab6 <- read_spss("merged_r6_data_2016_36countries2.sav")

## convert all column names to lower case
names(ab6) <- tolower(names(ab6))

## Introduction - Why am I analysizing the difference in perceptions of the fourth estate holding government accountable according to age?
## I believe that younger citizens are less prone to seeing the media as having to hold govenment to account compared to older citizens


## Concepts and measurement
## Question 1 - The dataset: Afrobarometer open data on Africa. The survey used was the sixth round done in 2016.
## question 36 of the survey asks the respondents about their view on news media reporting on government mistakes and corruption
## the responses are split into 2 statements (statement 1 for media reporting and statement 2 saying that too much reporting will harm the country)
## respondents can then either agree with statement 1 or statement 2 with different levels of agreement, remain neutral or refuse to answer the question

# look at what countries are available
table(ab6$country)

## code to view country codes
attributes(ab6$country)

## Convert country codes into factor
ab6$country.f <- as_factor(ab6$country)

## to use labels 
table(ab6$country.f)

## Convert age column into a factor
attributes(ab6$age_cond)
ab6$age_cond.f <- as_factor(ab6$age_cond)
table(ab6$age_cond.f)

## Convert gender column into a factor
attributes(ab6$q101)
ab6$q101.f <- as_factor(ab6$q101)
table(ab6$q101.f)

## view distribution of gender
ggplot(data = ab6) +
  aes(x = q101.f) +
  geom_bar(stat = "count") + 
  labs(x = "gender", y = "number of people")

## View distribution of ages in a histrogram
ggplot(data = ab6) +
  aes(x = age_cond.f) +
  geom_histogram(stat = "count") + 
  labs(x = "Age Groups", y = "No. of people")

## Convert Q36 responses on media perception in holding government to account into facors
attributes(ab6$q36)
ab6$q36.f <- as_factor(ab6$q36)
table(ab6$q36.f)

## View the distribution of responses in a histogram
ggplot(data = ab6) +
  aes(x = q36.f) +
  geom_bar(stat = "count") +
  labs(x = "Responses", y = "No. of people") + 
  theme(axis.text.x = element_text(angle=0, face = "italic", size=5))


## Recoding the data

## Recoding the grouping variable (age) into a dichotomy 
## Initially the age brackets were: 1 - 18-25, 2 - 26-35, 3 - 36-45, 4 - 46-55, 5 - 56-65, 6 - Over 65, 9 - Don't know
## After recoding the age variable will be: 1 - 18-45, 2 - 45 and older, 3 - Don't know

levels(ab6$age_cond.f) <- list("18 - 35" = c("18-25","26-35"), 
                        "36 - 55" = c("36-45","46-55"), 
                        "over 56" = c("56-65","Over 65"),
                        "no age given" = c("Don\'t know", NA))

rm.na(ab6$age_cond.f)
table(ab6$age_cond.f)

## I decided to recoded the age grouping acccordingly: 
## Initially I was going to recode the grouping into a dichotomy: 18-45 and 46 and older. But after an initial analysis of the distribution I decided that recoding would throw off the data
## Therefore I recoded the grouping variable as such:
## 18 - 35: the 18-25 and 26-35 brackets held what the South African Youth Commission Act, 1996 classified as "youth". This new recode would then stand for the segment of society that could be deemd young people as per my hypothesis for testing
## 36 - 55: this bracket would then stand for what one would classify as middle aged
## 56 and older: this bracket can be deemed an older generation.
## no age given: was chosen as the recode for instances where no age was given or where somebody did not know their age

## View the distribution of the grouping variable after the recoding
ggplot(data = ab6) +
  aes(x = age_cond.f) +
  geom_bar(stat = "count") + 
  labs(x = "Age Groups", y = "No. of people")


## Recoding the outcome variable (perceptions on the media and government - Question 36 in the survey)
## Initially the levels in the question were: -1(Missing), 1(Agree very strongly with 1), 2(Agree with 1), 3(Agree with 2), 4(Agree very strongly with 2), 5(Agree with neithe), 9(Don’t know), 98(Refused) 
## After recoding the levels for question 36 will be: 1(agree strong), 2(agree), 3(disagree - with statement 1), 4(disagree strongly - with statement 1), 5(neutral), 6(no response)

levels(ab6$q36.f) <- list("agree strongly" = "Agree very strongly with 1",
                         "agree" = "Agree with 1",
                         "disagree" = "Agree with 2",
                         "disagree strongly" = "Agree very strongly with 2",
                         "neutral" = c("Agree with neither", "Don\'t know"),
                         "no response" = c("Missing", "Refused"))

table(ab6$q36.f)
is.na(ab6$q36.f)

na.omit(ab6$q36.f)

## I decided to recoded the grouping for q36 (the variable I wanted to analyze against the demographic grouping) acccordingly: 
## After analyzing the distribution I concluded that firstly the statements could be collapsed into one statement: The news media should constantly investigate and report on government mistakes and corruption.
## This way the responses can analyzed against that one statement instead of 2.
## The responses were recoded accordingly
## On the positive side of the spectrum "agree strongly", "agree"
## On the negative side of the spectrum: disagree and disagree strongly
## Agree with neither and Don't know would be collapsed into neutral
## Missing and refused would be collapsed into no response

## View the distribution of q36 responses after the recoding
ggplot(data = ab6) +
  aes(x = q36.f) +
  geom_bar(stat = "count") +
  labs(x = "Responses", y = "No. of people") + 
  theme(axis.text.x = element_text(angle=0, face = "italic", size=5))

## Create an object with the data for South Africa
rsa <- subset(ab6, country.f == "South Africa")

## View the subsetted data
table(rsa$age_cond.f)
table(rsa$q36.f)

## Bivariate Analysis

## The two variables are both discrete. Whilst the demographic may at first look like it may fall under the continous category, because of its finite nature it becomes a discrete variable.


## Place the grouping demographic in a contigency table
round(prop.table(table(rsa$age_cond.f)), 2)

## Generate a mosaic plot of the demographic grouping
mosaicplot(~ rsa$age_cond.f, 
           shade = TRUE, 
           cex = 0.5,
           main = "Mosaic plot of demographic grouping",
           xlab = "Demographic grouping - Age")


## Generate a contingency table for gender
round(prop.table(table(rsa$q101.f)), 2)

## Place the outcome variable in a contingency table
round(prop.table(table(rsa$q36.f)), 2)


## Generate a mosaic plot of the outcome variable
mosaicplot(~ rsa$q36.f, 
           shade = TRUE, 
           cex = 0.5,
           main = "Mosaic plot of outcome variable",
           xlab = "Perceptions on reporting on government")

## Run a contingency table of the two variables
rsa.4.estate <- round(prop.table(table(rsa$q36.f, rsa$age_cond.f), margin = 2), 2)

rsa.4.estate.g <- round(prop.table(table(rsa$q36.f, rsa$q101.f), margin = 2), 2)

mosaicplot(t(rsa.4.estate), 
           main = "Perceptions of the fourth estate",
           las = 1, 
           shade = TRUE)

## Place the the two variables in a mosiac plot

mosaicplot(~ rsa$q36.f + rsa$age_cond.f, 
           data = rsa, 
           shade = TRUE, 
           main = "Perceptions of the fourth estate according to age",
           xlab = "Perceptions on the fourth estate",
           ylab = "Age categories",
           cex = 0.5)

mosaicplot(~ rsa$q36.f + rsa$q101.f, 
           data = rsa, 
           shade = TRUE, 
           main = "Perceptions of the fourth estate according to gender", 
           xlab = "Perceptions on the fourth estate",
           ylab = "Age categories",
           cex = 0.5)
#### Preamble ####
# Purpose: Prepare Survey data, using opinion poll 
# Author: Guangyu Du 
# Data: December 15th 2020
# Contact: Guangyu Du, university of toronto, https://github.com/spicapica/stats/blob/master/Final 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded library cesR
# age 15 in 2016 is age 18 in 2019 
# https://github.com/spicapica/stats/blob/master/Final

getwd()
#### Workspace setup ####
require(cesR)
library(forcats)
require(labelled)
library(tidyverse)
require(magrittr)
# load ces2019 web data

# get the not in operator
`%nin%` = Negate(`%in%`)

get_ces("ces2019_web")
# Read in the raw data (You might need to change this if you use a different dataset)
# Add the labels, convert to a factor type   
raw_data_survey <- labelled::to_factor(ces2019_web)
# Just keep some variables
# check variables 
colnames(raw_data_survey)

# vote  = (age employment province education ) gender income 
reduced_data_survey <- 
  raw_data_survey%>% 
  select( cps19_votechoice,
         cps19_citizenship,
         cps19_yob, 
         cps19_gender, 
        cps19_province,
       cps19_education,
       cps19_employment,
       cps19_income_number) %>%  mutate(cps19_yob = as.numeric(cps19_yob)) %>% mutate(age = 100 - cps19_yob)  %>%  
    select(-cps19_yob) %>% 
  # rename to match 
  rename(vote = cps19_votechoice, citizen = cps19_citizenship, Gender = cps19_gender,
         Province = cps19_province, Education = cps19_education, Employment = cps19_employment,
         income = cps19_income_number) %>%
  filter(age > 17) %>% drop_na()


# only eligible respondents 
reduced_data_survey <-  reduced_data_survey %>% filter(citizen != "Other") %>% select( -citizen ) %>% 
  # filter, 
  # Is vote a binary? If not, what are you going to do?
  # make vote a binary 
  filter(vote %in% c('Liberal Party', 'Conservative Party') ) %>% 
  # make binary party, only liberal and conservative, categorized 
  mutate(Vote = relevel(vote, ref = "Conservative Party"))  %>%
  # make binary gender 
     select(-vote) %>% filter(Gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)") %>%
       mutate(Gender = ifelse(Gender == "A man", "Man", "Woman")) %>% 
  # make binary age 
        mutate(Age = ifelse(age < 65, "15-64", "65-" ) ) %>% 
  # employment categorized to employed unemployed
        filter(Employment %nin% c("Other (please specify)", "Don't know/ Prefer not to answer" ) ) %>% 
        mutate(Employment = 
                 ifelse(Employment == "Unemployed/ looking for work", "Unemployed", "Employed") ) %>% 
   # education 
        filter(Education != "Don't know/ Prefer not to answer" ) %>% 
        mutate(Education = forcats::fct_collapse(Education,
                              "Under High School" =  c("No schooling", 
                                                       "Some elementary school" ,
                                                       "Completed elementary school",
                                                       "Some secondary/ high school" ),
                              "High School" = c("Completed secondary/ high school",
                                "Some technical, community college, CEGEP, College Classique"),
                              "Post High School" = c("Completed technical, community college, CEGEP, College Classique",
                                                     "Bachelor's degree",
                                                     "Master's degree",
                                                     "Professional degree or doctorate",
                                                     "Some university")  ) )  %>%
   # income 
  mutate(Income = case_when(
   income <= 29999 ~ "-$29999",
    income >= 30000 & income <= 69999 ~ "$30000-$69999",
    income >= 70000 & income <= 99999 ~ "$70000-$99999",
    income >= 100000  ~ "$100000-") ) %>% select(-income)

reduced_data_survey %>% head()

survey_data <- reduced_data_survey %>% select(Vote, Age, Gender, 
                                              Province, Education, Employment, Income)
# add label
survey_data$Label <- "Survey Data"
head(survey_data)
# check levels 
survey_data$Vote %<>% factor()
survey_data$Age %<>% factor()
survey_data$Gender %<>% factor()
survey_data$Province %<>% factor()
survey_data$Education %<>% factor()
survey_data$Employment %<>% factor()
survey_data$Income %<>% factor()

# apply factor(), apply levels()


# check levels in factor
survey_data$Vote %>% levels()
survey_data$Age %>% levels()
survey_data$Gender %>% levels()
survey_data$Province %>% levels()
survey_data$Education %>% levels()
survey_data$Employment %>% levels()
survey_data$Income %>% levels()

# barplot to briefly check the distribution of the categorical variables 
barplot(table(survey_data$Vote   ) )
barplot(table(survey_data$Age ) )
barplot(table(survey_data$Gender ) )
barplot(table(survey_data$Province ) )
barplot(table(survey_data$Education ) )
barplot(table(survey_data$Employment ) )
barplot(table(survey_data$Income ) )

# save datasets 
write_csv(survey_data, "inputs/data/survey.csv")

write_csv(survey_data, "outputs/survey.csv")






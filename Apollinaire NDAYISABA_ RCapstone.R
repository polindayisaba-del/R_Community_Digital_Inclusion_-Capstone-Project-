#1.loading packages #
library(tidyverse) 
library(ggplot2) 
library(dplyr) 
library(readr)
library(writexl)
library(plotly)
install.packages("janitor")
library(janitor)
#2.loading data
data<-read.csv("C:/Users/user/Desktop/DATA +RWANDA/R_Programming/community_digital_inclusion_raw (1).csv")
#3.Understand dataset 
str(data) # Checking data structure 
summary(data) # Summarizing the data from dataset 
names(data)   # Checking the names of columns 
dim(data)     # number of columns and rows 
colSums(is.na(data)) # Missing variables 
data %>%
  count(respondent_id) %>%
  filter(n > 1) # checking for duplicated in respondent_id
data %>%
  count(sector) %>%
  filter(n > 1) # checking for duplicated in sector
data %>%
  count(gender) %>%
  filter(n > 1) # checking for duplicated in gender
data %>%
  count(district) %>%
  filter(n > 1) # checking for duplicated in district

data %>%
  count(education_level) %>%
  filter(n > 1) # checking for duplicated in education level 
data %>%
  count(employment_status) %>%
  filter(n > 1) # checking for duplicated in employment_status 
data %>%
  count(digital_skill_level) %>%
  filter(n > 1) # checking for duplicated in digital skill level 

data %>%
  count(mobile_money_user) %>%
  filter(n > 1) # checking for duplicated in mobile money user
data %>%
  count(smartphone_owner) %>%
  filter(n > 1) # checking for duplicated in smartphone owner

#4.Data cleaning

data<- data %>%
  mutate(
    gender = str_to_lower(str_trim(gender))
  ) # Remove unwanted spaces 
data <- data %>%
  mutate(
    gender = case_when(
      gender == "male" ~ "Male",
      gender == "female" ~ "Female",
      TRUE ~ NA_character_
    )
  )  # Cleaning gender 
table(data$gender, useNA = "ifany") # Check if gender was cleaned 
# Clean invalid ages
data <- data %>%
  mutate(
    age = if_else(age < 18 | age > 100, NA_real_, age)
  )

data <- data %>%
  mutate(
    district = str_to_title(str_trim(district))
  )

table(data$district, useNA = "ifany") # Check if District was cleaned 

sort(unique(data$sector)) # To check how the values in sector 

data <- data %>%
  mutate(
    sector = str_to_lower(str_trim(sector)),
    sector = str_replace_all(sector, "-", " "),
    sector = str_squish(sector),
    sector = str_to_title(sector)
  ) # Cleaning sector
table(data$sector)# Check if sector was cleaned

table(data$education_level, useNA = "ifany") # Checking the status of  education_level 

data<- data_clean %>%
  mutate(
    education_level = na_if(str_trim(education_level), "")
  )
data <-data%>%
  mutate(
    education_level = str_to_lower(education_level),
    education_level = str_to_title(education_level)
  ) # Cleaning education_level

table(data$education_level, useNA = "ifany") # Check if education_level was cleaned

table(data$employment_status, useNA = "ifany") # Checking the status of employment_status
data <-data %>%
  mutate(
    employment_status = str_to_lower(str_trim(employment_status)),
    employment_status = str_to_title(employment_status)
  ) # Cleaning employment_status

table(data$employment_status)# check if employment_status was cleaned

table(data$smartphone_owner, useNA = "ifany") # Checking the status of smartphone_owner.
table(data$internet_access, useNA = "ifany") # checking the status of internet_access
table(data$mobile_money_user, useNA = "ifany") # Checking the status of mobile_money_user
table(data$attended_training, useNA = "ifany")# Checking the status of attended_training

data<-data%>%
  mutate(
    smartphone_owner = str_to_lower(str_trim(smartphone_owner)),
    internet_access = str_to_lower(str_trim(internet_access)),
    mobile_money_user = str_to_lower(str_trim(mobile_money_user)),
    attended_training = str_to_lower(str_trim(attended_training))
  ) %>%
  mutate(
    smartphone_owner = case_when(
      smartphone_owner == "yes" ~ "Yes",
      smartphone_owner == "no" ~ "No",
      TRUE ~ NA_character_
    ),
    
    internet_access = case_when(
      internet_access == "yes" ~ "Yes",
      internet_access == "no" ~ "No",
      TRUE ~ NA_character_
    ),
    
    mobile_money_user = case_when(
      mobile_money_user == "yes" ~ "Yes",
      mobile_money_user == "no" ~ "No",
      TRUE ~ NA_character_
    ),
    
    attended_training = case_when(
      attended_training == "yes" ~ "Yes",
      attended_training == "no" ~ "No",
      TRUE ~ NA_character_
    )
  )# Cleaning smartphone_owner, internet_access, mobile_money_user, and attended_training

table(data$smartphone_owner, useNA = "ifany")# Check if smartphone_owner was cleaned
table(data$internet_access, useNA = "ifany") # Check if internet_access was cleaned
table(data$mobile_money_user, useNA = "ifany")# Check if mobile_money_user was cleaned
table(data$attended_training, useNA = "ifany")# Check if attended_training was cleaned

table(data_clean$digital_skill_level, useNA = "ifany")#checking the status of digital_skill_level

data<- data%>%
  mutate(
    digital_skill_level = na_if(str_trim(digital_skill_level), ""),
    digital_skill_level = str_to_lower(digital_skill_level),
    digital_skill_level = str_to_title(digital_skill_level)
  )#cleaning digital_skill_level

table(data$digital_skill_level, useNA = "ifany") #check if digital_skill_level was cleaned

table(data$employment_after_training, useNA = "ifany")#checking the status of employment_after_training

data<- data %>%
  mutate(
    employment_after_training = str_to_lower(
      str_trim(employment_after_training)
    ),
    employment_after_training = case_when(
      employment_after_training == "yes" ~ "Yes",
      employment_after_training == "no" ~ "No",
      employment_after_training == "not applicable" ~ "Not Applicable",
      TRUE ~ NA_character_
    )
  )# Cleaning employment_after_training
table(data$employment_after_training, useNA = "ifany")#check if employment_after_training was cleaned

summary(data$age)

data %>%
  filter(age < 18 | age > 100) %>%
  select(respondent_id, age) # Checking for outliers in age column

data<- data%>%
  mutate(
    age = if_else(age < 18 | age > 100, NA_integer_, age)
  ) #cleaning age column by replacing outliers with NA

summary(data$age)#checking if age column was cleaned
 
summary(data$monthly_income)

data %>%
  filter(monthly_income < 0)

data<- data%>%
  mutate(
    monthly_income = if_else(
      monthly_income < 0,
      NA_real_,
      monthly_income
    )
  ) # Cleaning monthly_income column by replacing negative values with NA

summary(data$monthly_income) #checking if monthly_income column was cleaned

summary(data$household_size)

data%>%
  filter(household_size < 1) # Checking for outliers in household_size column
table(data$household_size, useNA = "ifany")# Cleaning household_size column by replacing values less than 1 with NA

data%>%
  filter(
    satisfaction_score < 1 |
      satisfaction_score > 5
  )# Checking for outliers in satisfaction_score column

data<- data%>%
  mutate(
    satisfaction_score = if_else(
      satisfaction_score < 1 |
        satisfaction_score > 5,
      NA_integer_,
      satisfaction_score
    )
  ) # Cleaning satisfaction_score column by replacing values less than 1 or greater than 5 with NA

summary(data$satisfaction_score)#checking if satisfaction_score column was cleaned

colSums(is.na(data))

missing_summary <-data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

missing_summary   # Checking the summary of missing values in each column

data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing"
  ) %>%
  arrange(desc(missing))# Sorting the missing values in descending order

str(data) # Checking the structure of the cleaned dataset
summary(data) # Summarizing the cleaned dataset

dim(data) # Checking the dimensions of the cleaned dataset

data<- data %>%
  mutate(
    age_band = case_when(
      age >= 18 & age <= 35 ~ "18-35",
      age >= 36 ~ "36+",
      TRUE ~ NA_character_
    )
  ) # Creating a new variable age_band to categorize respondents into two age groups: 18-35 and 36+

table(data$age_band, useNA = "ifany") # Checking the distribution of respondents across the age bands

data<- data%>%
  mutate(
    age_band = factor(
      age_band,
      levels = c("18-35", "36+")
    )
  ) # Converting age_band to a factor variable with specified levels

# Check invalid ages
data %>%
  filter(age < 18 | age > 100) %>%
  select(respondent_id, age)
# Check age after cleaning
summary(data$age)
data %>%
  summarise(
    mean_age = mean(age, na.rm = TRUE),
    median_age = median(age, na.rm = TRUE),
    minimum_age = min(age, na.rm = TRUE),
    maximum_age = max(age, na.rm = TRUE)
  ) # Age summary
# Gender distribution
data %>%
  count(gender) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  ) # Gender distribution 
# TWO FIXES TO YOUR CLEANING CODE 
# education_level cleaning line references `data_clean`,
#   data <- data %>% mutate(education_level = na_if(str_trim(education_level), ""))
# check for duplicate respondent_id but never removed them.
data <- data %>% distinct(respondent_id, .keep_all = TRUE)
dim(data) 

# 5. DEMOGRAPHIC PROFILE.
# District distribution
data %>% count(district) %>% mutate(pct = round(n / sum(n) * 100, 1))

# Sector distribution
data %>% count(sector) %>% mutate(pct = round(n / sum(n) * 100, 1))

# Education level distribution
data %>% count(education_level) %>% mutate(pct = round(n / sum(n) * 100, 1))

# 6. INTERNET ACCESS BY SECTOR (highest vs lowest)
internet_by_sector <- data %>%
  filter(!is.na(internet_access)) %>%
  group_by(sector) %>%
  summarise(pct_internet = round(mean(internet_access == "Yes") * 100, 1)) %>%
  arrange(desc(pct_internet))

internet_by_sector
# Gap between best- and worst-served sector
max(internet_by_sector$pct_internet) - min(internet_by_sector$pct_internet)

# 7.  EMPLOYMENT STATUS: overall, and by education level
data %>% count(employment_status) %>% mutate(pct = round(n / sum(n) * 100, 1))

employment_by_education <- data %>%
  filter(!is.na(education_level)) %>%
  count(education_level, employment_status) %>%
  group_by(education_level) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

employment_by_education

# 8.  AVERAGE MONTHLY INCOME BY EDUCATION LEVEL
income_by_education <- data %>%
  filter(!is.na(education_level), !is.na(monthly_income)) %>%
  group_by(education_level) %>%
  summarise(
    mean_income = round(mean(monthly_income), 0),
    median_income = round(median(monthly_income), 0),
    n = n()
  ) %>%
  arrange(desc(mean_income))

income_by_education
# 9.  TRAINING vs EMPLOYMENT OUTCOME
# Standardise attended_training first (same pattern as your other Yes/No fields)
data <- data %>%
  mutate(attended_training = str_to_lower(str_trim(attended_training))) %>%
  mutate(attended_training = case_when(
    attended_training == "yes" ~ "Yes",
    attended_training == "no"  ~ "No",
    TRUE ~ NA_character_
  ))

# Compare CURRENT employment status (in work vs not) between the two groups
training_outcome <- data %>%
  filter(!is.na(attended_training)) %>%
  mutate(in_work = employment_status %in% c("Employed", "Self-employed")) %>%
  group_by(attended_training) %>%
  summarise(pct_in_work = round(mean(in_work) * 100, 1), n = n())

training_outcome

# Supplementary check: among trainees only, self-reported outcome after training
data %>%
  filter(attended_training == "Yes") %>%
  count(employment_after_training) %>%
  mutate(pct = round(n / sum(n) * 100, 1))

# Interpretation :
# This is an observational survey, not a controlled experiment. Respondents
# chose whether to attend training, so any employment gap could reflect
# self-selection (e.g. more motivated or already-employed people opting in)
# rather than the training itself causing better outcomes. Association here
# does not establish causation.

# 10. GROUPS WITH LOWER DIGITAL ACCESS
digital_access_by_group <- function(group_var) {
  data %>%
    filter(!is.na({{ group_var }})) %>%
    group_by({{ group_var }}) %>%
    summarise(
      pct_smartphone = round(mean(smartphone_owner == "Yes", na.rm = TRUE) * 100, 1),
      pct_internet   = round(mean(internet_access == "Yes", na.rm = TRUE) * 100, 1),
      pct_advanced_skill = round(mean(digital_skill_level == "Advanced", na.rm = TRUE) * 100, 1),
      n = n()
    )
}

digital_access_by_group(gender)
digital_access_by_group(district)
digital_access_by_group(education_level)
digital_access_by_group(age_band)

# 11. RESEARCH TARGETS
# Target 1: >= 50% female
data %>% filter(!is.na(gender)) %>% summarise(pct_female = round(mean(gender == "Female") * 100, 1))

# Target 2: >= 70% internet access
data %>% filter(!is.na(internet_access)) %>% summarise(pct_internet = round(mean(internet_access == "Yes") * 100, 1))

# Target 3: >= 60% aged 18-35
data %>% filter(!is.na(age_band)) %>% summarise(pct_youth = round(mean(age_band == "18-35") * 100, 1))
# 12. VISUALIZATIONS (5 charts, one per key finding)
# 1. Age distribution
ggplot(data, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Age Distribution of Respondents", x = "Age", y = "Count")

# 2. Gender distribution
ggplot(data %>% filter(!is.na(gender)), aes(x = gender, fill = gender)) +
  geom_bar() +
  labs(title = "Gender Distribution", x = NULL, y = "Count") +
  theme(legend.position = "none")

# 3. Internet access by sector
ggplot(internet_by_sector, aes(x = reorder(sector, pct_internet), y = pct_internet)) +
  geom_col(fill = "blue") +
  coord_flip() +
  labs(title = "Internet Access Rate by Sector", x = NULL, y = "% with Internet Access")

# 4. Income by education level
ggplot(data %>% filter(!is.na(education_level), !is.na(monthly_income)),
       aes(x = education_level, y = monthly_income)) +
  geom_boxplot(fill = "green") +
  labs(title = "Monthly Income by Education Level", x = NULL, y = "Monthly Income (RWF)") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 5. Employment outcome by training attendance
ggplot(training_outcome, aes(x = attended_training, y = pct_in_work, fill = attended_training)) +
  geom_col() +
  labs(title = "Employment Rate by Training Attendance", x = "Attended Training", y = "% Currently in Work") +
  theme(legend.position = "none")

# 5 Key Findings

# Digital access is a geography problem, not a gender problem. Internet access is almost identical by gender (Female 62.8%, Male 62.2%), but it swings wildly by district and sector — Gasabo (80.6%) vs. Nyagatare (38.1%), and Sector A (85.2%) vs. Sector F (37.6%), a 47.6-point gap between the best- and worst-served sectors.
# Education level is the strongest driver of income and job type. Mean monthly income rises steadily with education, from 62,480 RWF (no formal education) to 256,722 RWF (university/tertiary) — over 4x. Employment also shifts from mostly self-employed/unemployed at the low end to 59.8% formally employed at the university level.
# The training-employment link is genuinely ambiguous, not clearly positive. A raw comparison of current employment status shows trainees are actually slightly less likely to be currently working (39.0%) than non-trainees (43.5%) — but 74.1% of trainees self-report the training led to a job. The mismatch likely reflects who training was targeted at (probably unemployed/student respondents) rather than the program failing; the data can't separate the two.
# Advanced digital skills are concentrated among the young, educated, and urban. 33.0% of university-educated respondents report advanced skills vs. 1.2% with no formal education; 20.7% of 18–35 year-olds vs. 8.6% of 36+; and 28.4% in Gasabo vs. just 2.8% in Nyagatare.
# Only one of three targets was clearly met. Female representation hit 53.9% (target ≥50% ✅), but youth participation landed at 56.3% (target ≥60% ❌, close but short) and internet access at 65.1% (target ≥70% ❌), dragged down by the low-access districts in finding #1.

# 3 Evidence-Based Recommendations

# Target infrastructure spending geographically, not demographically. Since access gaps track district/sector rather than gender, concentrate connectivity investment in Nyagatare and Musanze (and Sectors E/F) rather than gender-focused outreach — that's where the 40+ point gaps actually are.
# Bundle digital-skills training with foundational education support. Because advanced digital skills and income both climb sharply with education level, digital training alone is unlikely to close the gap for the No Formal Education/Primary groups — pairing it with basic literacy/numeracy support would likely lift outcomes further.
# Fix how training impact is measured before scaling the program. The current single-snapshot employment field can't distinguish "training caused better outcomes" from "training was targeted at people who started out worse off." Recommend tracking a pre/post employment status per trainee (or a matched comparison group) in the next survey round.


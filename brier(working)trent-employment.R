


# load packages
library(readxl)
library(dplyr)
library(psych)

# load data as object called data
raw_data <- read_excel("data.xlsx")
data <- raw_data
######################################################################## 
################## Phase 1: Clean data ################## 
######################################################################## 

## JOB TENURE
# step 1: analyze initial variable
table(data$YEAR_FIRST)
table(data$YEAR_FINAL)
            
# step 2: Clean data (if applicable)
# filter missing
data <- filter(data, YEAR_FIRST != "1010.0" & YEAR_FIRST != "NA" & YEAR_FIRST != "9123.0")
data <- filter(data, YEAR_FINAL != "NA")


# step 3: confirm missing filtered
table(data$YEAR_FIRST)
table(data$YEAR_FINAL)

# create tenure
data$tenure <- as.numeric(data$YEAR_FINAL) - as.numeric(data$YEAR_FIRST)


# hist of tenure
hist(data$tenure)

#full model histogram of tenure
hist(my_dataset$tenure, xlab = "Years", ylab = "Number Of Individuals", main = "Histogram of Tenure")


#confirm by looking at data table
table(data$tenure)




## reason for quitting
# cleaned in excel


## citizen
# step 1
table(data$CITIZEN)

#step 2
# drop missing

data <- filter(data, CITIZEN != "0 = 1937" & CITIZEN != "0 5/1/39" & CITIZEN != "1, 10-12-26" & CITIZEN != "37 years" & CITIZEN != "4019.0" & CITIZEN != "8402.0" & CITIZEN != "888.0" & CITIZEN != "Female" & CITIZEN != "Female." & CITIZEN != "Mi0r" & CITIZEN != "Mi0r.")
table(data$CITIZEN)

# create dummy for citizen, not citizen, and papers
data <- mutate(data, is_citizen = ifelse(CITIZEN == "1.0" | CITIZEN == "U.S.", 1, 0))
data <- mutate(data, not_citizen = ifelse(CITIZEN == "0.0" , 1, 0))
data <- mutate(data, papers = ifelse(is_citizen == 0 & not_citizen == 0 , 1, 0))


# confirm citizen dummy
table(data$CITIZEN, data$is_citizen)
table(data$CITIZEN, data$not_citizen)
table(data$CITIZEN, data$papers)

table(my_dataset$is_citizen)
barplot(table(my_dataset$is_citizen))

##english read
#step 1
table(data$ENGLISH_READ)


# Step 2: recode
# remove missing
data$ENGLISH_READ <- ifelse(data$ENGLISH_READ == "0.5ly Well", 0 , data$ENGLISH_READ)
data$ENGLISH_READ <- as.numeric(data$ENGLISH_READ)
data <- filter(data, ENGLISH_READ !="888")
data <- data %>% mutate(r_english = if_else(ENGLISH_READ == 1, 1, 0))

#Step 3: confirm english read
table(data$r_english)

# Barplot for english read
barplot(table(data$r_english), xlab = "Read English", ylab = "Number Of Individuals", main = "Barplot Of Read English")


## Number of relatives (roebling)
#step 1: analyze initial variable
table(data$NUM_RELATIVES_ROEBLING)

#Step 2: Clean data (if applicable)
# filter missing


data <- filter(data, NUM_RELATIVES_ROEBLING != "NA" & NUM_RELATIVES_ROEBLING != "None" & NUM_RELATIVES_ROEBLING != "na")

# Create number of relatives (roebling)
data$NUM_RELATIVES_ROEBLING <- as.numeric(data$NUM_RELATIVES_ROEBLING) 
hist(data$NUM_RELATIVES_ROEBLING)

data <- data %>% mutate(relatives = if_else(NUM_RELATIVES_ROEBLING == 0, 0, 1))


# step 3: confirm appropriate cleaning
table(data$relatives)

# Barplot for Any Relatives
barplot(table(data$relatives), xlab = "Relatives", ylab = "Number Of Individuals", main = "Barplot Of Any Relatives")

## Age
#Step 1: analyze initial variable
table(data$AGE)

#Step 2: Clean data (if applicable)
# filter missing

data <- filter(data, AGE != "NA")

# Create Age
data$AGE <- as.numeric(data$AGE) 

# confirm missing filtered
table(data$AGE)

# step 3: confirm appropriate cleaning
hist((data$AGE), xlab = "AGE", ylab = "Number Of Individuals", main = "Histogram Of Age Of Individuals")


## American Born Nationality
#Step 1: analyze initial variable
table(data$ANY_AMERICAN_NATIONALITY)

#Step 2: Clean data if applicable
#filter missing

data <- filter(data, ANY_AMERICAN_NATIONALITY != "888.0" & ANY_AMERICAN_NATIONALITY  != "n" & ANY_AMERICAN_NATIONALITY != "y1")

# Create Any American Nationality
data$ANY_AMERICAN_NATIONALITY <- as.numeric(data$ANY_AMERICAN_NATIONALITY) 

# Step 3:
# confirm missing filtered
table(data$ANY_AMERICAN_NATIONALITY)

# Barplot for Any American Nationality
barplot(table(data$ANY_AMERICAN_NATIONALITY), xlab = "Any American Nationality", ylab = "Number Of Individuals", main = "Barplot Of Any American Nationality")

# Americanized
# step 1: examine initial variables
table(data$AMERICAN_BORN_NATIONALITY)
table(data$ANY_AMERICAN_NATIONALITY)
table(data$AMERICAN_BORN_NATIONALITY, data$ANY_AMERICAN_NATIONALITY)


# create americanized with 3 categories
data <-  mutate(data, americanized_1 = ifelse(AMERICAN_BORN_NATIONALITY == "1.0" | ANY_AMERICAN_NATIONALITY == 1, 1, 0))

# confirm correct
table(data$americanized_1)
table(data$ANY_AMERICAN_NATIONALITY, data$americanized_1)

#Barplot for Americanized
barplot(table(my_dataset$americanized_1), xlab = "Americanized", ylab = "Number Of Individuals", main = "Barplot Of Americanized")


## Number of Address Changes
#Step 1: analyze initial variable
table(data$NUM_ADDRESSCHANGES)

#Step 2: Clean data if applicable
#filter missing

data <- filter(data, NUM_ADDRESSCHANGES != "NA")

# Create Number of Address Changes
data$NUM_ADDRESSCHANGES <- as.numeric(data$NUM_ADDRESSCHANGES) 


# confirm missing filtered
table(data$NUM_ADDRESSCHANGES)

# step 3: confirm appropriate cleaning
hist(data$NUM_ADDRESSCHANGES, xlab = "Number Of Address Changes", ylab = "Number Of Individuals", main = "Histogram Of Number Of Address Changes")

## Number of Children
#Step 1: analyze initial variable
table(data$NUM_CHILDREN)

#Step 2: Clean data if applicable
#filter missing
data <- filter(data, NUM_CHILDREN != "NA")

# Create Number of Children
data$NUM_CHILDREN <- as.numeric(data$NUM_CHILDREN) 

# confirm missing filtered
table(data$NUM_CHILDREN)

#Step 3: confirm appropriate cleaning
hist(data$NUM_CHILDREN)


## Any children
#Step 1: analyze initial variable
table(data$NUM_CHILDREN)

#Step 2: Create any children
data <- data %>% mutate(any_children = if_else(NUM_CHILDREN == 0, 0, 1))

#Step 3: confirm appropriate cleaning
table(data$any_children)

#Barplot of Any Children
barplot(table(data$any_children), xlab = "Any Children", ylab = "Number Of Individuals", main = "Barplot Of Any Children")



## Number of Times Rehired
#Step 1: analyze initial variable
table(data$`NUM_RE-EMPL0Y_ENTRIES`)

# Create Number of Times Rehired
data$`NUM_RE-EMPL0Y_ENTRIES` <- as.numeric(data$`NUM_RE-EMPL0Y_ENTRIES`) 

# confirm missing filtered
table(data$`NUM_RE-EMPL0Y_ENTRIES`)

#Step 3: confirm appropriate cleaning
hist(data$`NUM_RE-EMPL0Y_ENTRIES`, xlab = "Number of Times Re-employed", ylab = "Number Of Individuals", main = "Histogram Of Re-employed")


## Any previous employment with roebling (before entry on back of card)
#Step 1: analyze initial variable
table(data$ANY_PREV_JAR)

#Step 2: Clean data if applicable
#filter missing

data <- filter(data, ANY_PREV_JAR != "unsure")

# Create Any Previous Employment
data$ANY_PREV_JAR <- as.numeric(data$ANY_PREV_JAR) 

# confirm missing filtered
table(data$ANY_PREV_JAR)

#Step 3: confirm appropriate cleaning
hist(data$ANY_PREV_JAR)

#Barplot of Any Previous Job at Roebling
barplot(table(my_dataset$ANY_PREV_JAR), xlab = "Any Previous Job at Roebling", ylab = "Number Of Individuals", main = "Barplot of Any Previous Job at Roebling")

## Birthyear
#Step 1: analyze initial variable
table(data$YEARBIRTH)

# Create Birthyear
data$YEARBIRTH <- as.numeric(data$YEARBIRTH) 

# confirm missing filtered
table(data$YEARBIRTH)

#Step 3: confirm appropriate cleaning
hist(data$YEARBIRTH)


## Factory location
table(data$ANY_TRENTON)
table(data$ANY_KINKORA)




### Phase 2: Complete case dataset

### STEP 1: Create a list of cleaned variables to keep called "my_varlist". This list should include the factor variable (where each value represents a category) and the dummy variables (which each represent a category from a specific variable). 

my_varlist <- c("tenure", "Voluntary", "is_citizen", "not_citizen", "papers", "r_english", "relatives", "AGE", "ANY_AMERICAN_NATIONALITY", "NUM_ADDRESSCHANGES", "NUM_CHILDREN", "reemploy", "ANY_PREV_JAR", "any_children", "americanized_1", "CITIZEN", "ANY_KINKORA", "ANY_TRENTON", "NUM_RELATIVES_ROEBLING")


### STEP 2: Create a new dataset called "my_dataset" with only your variables and complete case information. Below the select() command limits the data frame to only include variables in the list "my_varlist", and the filter() command limits the observations to only those that meet the condition of having complete case information. The "pipe" function ~ %>% ~ extends the command to multiple lines (also denoted by the indent). This structure can be used as an alternative to the previous syntax structure. The same function could be completed separately by doing the select and filter commands on different lines.  

my_dataset <- data %>%
  select(all_of(my_varlist)) %>%
  filter(complete.cases(.))


### STEP 3: Gather Summary Statistics and confirm valid dataset construction

describe(my_dataset)


### Phase 3: Analysis
#Descriptive Statistics


## 1. 
cor(data$NUM_ADDRESSCHANGES, data$ANY_TRENTON)
cor(data$NUM_ADDRESSCHANGES, data$ANY_KINKORA)

table(data$ANY_TRENTON, data$ANY_KINKORA)

# dig into 0, 0
data_00 <- data %>%
  filter(ANY_TRENTON == 0 & ANY_KINKORA == 0)

data_01 <- data %>%
  filter(ANY_TRENTON == 0 & ANY_KINKORA == 1)

data_10 <- data %>%
  filter(ANY_TRENTON == 1 & ANY_KINKORA == 0)

data_11 <- data %>%
  filter(ANY_TRENTON == 1 & ANY_KINKORA == 1)

summary(data_00$tenure)
summary(data_01$tenure)
summary(data_10$tenure)
summary(data_11$tenure)

# answer Q1

Q1A <- lm(tenure ~ ANY_KINKORA + NUM_ADDRESSCHANGES, data = data)
summary(Q1A)

# test interaction between kinkora and address change
Q2A <- lm(tenure ~ ANY_KINKORA + NUM_ADDRESSCHANGES + ANY_KINKORA*NUM_ADDRESSCHANGES, data = data)
summary(Q2A)

# keep interaction term



# number of times rehired
hist(my_dataset$tenure)
table(my_dataset$reemploy)
plot(my_dataset$reemploy, my_dataset$tenure)

lm_rehired <- lm(tenure ~ reemploy, data = my_dataset)
summary(lm_rehired)

# TEST polynomial
my_dataset$reemploy2 <- my_dataset$reemploy*my_dataset$reemploy

lm_rehired2 <- lm(tenure ~ reemploy + reemploy2, data = my_dataset)
summary(lm_rehired2)

# model as polynomial

## 3. 
table(data$CODE_FINAL)

data <- mutate(data, died = ifelse(CODE_FINAL == "Died" | CODE_FINAL == "DIED" | CODE_FINAL == "D" | CODE_FINAL == "DEC" | CODE_FINAL == "Dec." | CODE_FINAL == "Deceased" | CODE_FINAL == "Dis.", 1, 0))
data <- mutate(data, retired = ifelse(CODE_FINAL == "R" | CODE_FINAL == "RET" | CODE_FINAL == "RET." | CODE_FINAL == "RETIRED" | CODE_FINAL == "RT" | CODE_FINAL == "RTC" | CODE_FINAL == "RTD", 1, 0))
data <- mutate(data, papers = ifelse(is_citizen == 0 & not_citizen == 0 , 1, 0))


# confirm citizen dummy
table(data$CODE_FINAL, data$died)
table(data$CITIZEN, data$not_citizen)
table(data$CITIZEN, data$papers)

# create dataset only including observations for each category of reason for leaving
dead_only <- filter(data, died == 1)
retired_only <- filter(data, retired == 1)

## assess mean tenure for each reason for leaving
mean(dead_only$tenure)
mean(retired_only$tenure)


# combine categories based upon mean tenure

## 4. 
# all separeate
Q4A <- lm(tenure ~ is_citizen + papers, data = my_dataset)
summary(Q4A)

# combine first papers with yes
Q4B <- lm(tenure ~ not_citizen, data = my_dataset)
summary(Q4B)

# combine first papers with not citizen
Q4C <- lm(tenure ~ is_citizen, data = my_dataset)
summary(Q4C)

# F-test comparing A to C
anova(Q4A, Q4C)

##5. 
hist(my_dataset$tenure)
table(my_dataset$NUM_CHILDREN)
plot(my_dataset$reemploy, my_dataset$tenure)

lm_number_children <- lm(tenure ~ NUM_CHILDREN, data = my_dataset)
summary(lm_number_children)

# TEST any children
lm_any_children <- lm(tenure ~ any_children, data = my_dataset)
summary(lm_any_children)

#Choose any children because it's simpler




## Age
hist(my_dataset$tenure)
table(my_dataset$AGE)
plot(my_dataset$AGE, my_dataset$tenure)

lm_age <- lm(tenure ~ AGE, data = my_dataset)
summary(lm_age)
hist(resid(lm_age))
plot(my_dataset$AGE,resid(lm_age))
# TEST polynomial
my_dataset$age2 <- my_dataset$AGE*my_dataset$AGE

lm_age2 <- lm(tenure ~ AGE + age2, data = my_dataset)
summary(lm_age2)


## Multicolinearity of Age and Number of Children
hist(my_dataset$AGE)
hist(my_dataset$NUM_CHILDREN)
cor(my_dataset$AGE, my_dataset$NUM_CHILDREN)
plot(my_dataset$AGE, my_dataset$NUM_CHILDREN)


## Relationship between Americanized and Citizen
cor(my_dataset$americanized_1, my_dataset$is_citizen)
table(my_dataset$americanized_1, my_dataset$is_citizen)

Q8 <- lm(tenure ~ americanized_1*is_citizen, data = my_dataset)
summary(Q8)



#Predicted value for read english
read_english <- lm(tenure ~ r_english, data = my_dataset)
summary(read_english)

#Predicted value for number of relatives roebling
num_relatives_roebling <- lm(tenure ~ NUM_RELATIVES_ROEBLING, data = my_dataset)
summary(num_relatives_roebling)

relatives_roebling <- lm(tenure ~ relatives, data = my_dataset)
summary(relatives_roebling)



#Predicted value for Any American Nationality
any_american_nationality <- lm(tenure ~ ANY_AMERICAN_NATIONALITY, data = my_dataset)
summary(any_american_nationality)


#Predicted value for Number of Address Changes
number_address_changes <- lm(tenure ~ NUM_ADDRESSCHANGES, data = my_dataset)
summary(number_address_changes)

my_dataset$housing_instability <- my_dataset$NUM_ADDRESSCHANGES/(my_dataset$tenure +1)
summary(my_dataset$housing_instability)
housing_instability <- lm(tenure ~ housing_instability, data = my_dataset)
summary(housing_instability)

#Predicted tenure for re-employ
re_employ <- lm(tenure ~ reemploy, data = my_dataset)
summary(re_employ)

#Predicted tenure for any previous job at roebling
any_prev_jar <- lm(tenure ~ ANY_PREV_JAR, data = my_dataset)
summary(any_prev_jar)


#Predicted tenure for Americanized
americanized <- lm(tenure ~ americanized_1, data = my_dataset)
summary(americanized)


#Predicted tenure for Kinkora factory
kinkora <- lm(tenure ~ ANY_KINKORA, data = my_dataset)
summary(kinkora)


#Predicted tenure for Trenton factory
trenton <- lm(tenure ~ ANY_TRENTON, data = my_dataset)
summary(trenton)


#Predicted tenure for Number of Address Changes
address <- lm(tenure ~ NUM_ADDRESSCHANGES, data = my_dataset)
summary(address)


#Predicted tenure for Kinkora and address change interaction
interactions <- lm(tenure ~ ANY_KINKORA*NUM_ADDRESSCHANGES, data = my_dataset)
summary(interactions)

#Predicted tenure for American Nationality and Citizen
america_citizen <- lm(tenure ~ americanized_1*is_citizen, data = my_dataset)
summary(america_citizen)



# complete model
final_model <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(final_model)


#Residual Plot
plot(final_model)


# Relative Reduction of R-squared (citizen)
remove_citizen <- lm(tenure ~ r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(remove_citizen)

#RR2 = 0.65-0.6347 = 0.0153
#Removing Citizen reduces the explanatory power of the model by 1.53%

#Kinkora
no_kinkora <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_kinkora)

#RR2 = 0.65 - 0.44 = 0.2066
#Removing Kinkora reduces the explanatory power by 20.66%

#Read English
no_read_english <- lm(tenure ~ americanized_1*is_citizen + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_read_english)

#RR2 = 0.65 - 0.6431 = 0.0069
#Removing Read English reduces the explanatory power by 0.69%


#Relative at Roebling
no_relative <- lm(tenure ~ americanized_1*is_citizen + r_english + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_relative)

#RR2 = 0.65 - 0.6352 = 0.0148
#Removing Relative at Roebling reduces the explanatory power by 1.48%


#Age
no_age <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children, data = my_dataset)
summary(no_age)

#RR2 = 0.65 - 0.6366 = 0.0134
#Removing Age reduces the explanatory power by 1.34%


#Any American Nationality
no_nationality <- lm(tenure ~  r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR  + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_nationality)

#RR2 = 0.65 - 0.6262 = 0.0238
#Removing Any American Nationality reduces the explanatory power by 2.38%


#Address Changes
no_address <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_address)

#RR2 = 0.65 - 0.4434 = 0.2066
#Removing Number of Address Changes reduces the explanatory power by 20.66%


#Any Children
no_children <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + AGE, data = my_dataset)
summary(no_children)

#RR2 = 0.65 - 0.6463 = 0.0037
#Removing Any Children reduces the explanatory power by 0.37%


#Re-employ
no_reemploy <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES  + any_children + AGE, data = my_dataset)
summary(no_reemploy)

#RR2 = 0.65 - 0.6354 = 0.0146
#Removing re-employ reduces the explanatory power by 1.46%


#Previous Job at Roebling
no_jar <- lm(tenure ~ americanized_1*is_citizen + r_english + NUM_RELATIVES_ROEBLING  +  ANY_AMERICAN_NATIONALITY + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_jar)

#RR2 = 0.65 - 0.6393 = 0.0107
#Removing any previous job at Roebling reduces the explanatory power by 1.07%


#Americanized
no_americanized <- lm(tenure ~ r_english + NUM_RELATIVES_ROEBLING + ANY_PREV_JAR  + ANY_KINKORA*NUM_ADDRESSCHANGES + reemploy + reemploy2 + any_children + AGE, data = my_dataset)
summary(no_americanized)

#RR2 = 0.65 - 0.6262 = 0.0238
#Removing americanized reduces the explanatory power by 2.38%
















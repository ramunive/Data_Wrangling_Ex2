# load packages
library("dplyr")
library("tidyr")
library("haven")

# load the original data
titanic_original <- read.csv("titanic_original.csv")

# create a variable to save the cleaned data
titanic2 <- titanic_original

# explore data

# examine structure of dataset
str(titanic2)
# A dataframe as expected
# 1310 observations of 14 variables

head(titanic2)


# 1. Port of embarkation
# The embarked column has some missing values, which are known to correspond
# to passengers who actually embarked at Southampton.

# Find the missing values and replace them with S

# embarked is a factor with 4 levels: "", "C", "Q", "S"
# "" is an empty string, or missing value

# Determine the number of missing values
summary(titanic2)
# summary(titanic2) shows no NAs
# summary(titanic2) shows C:270 + Q:123 + S:914 = 1307 

# Determine the number of missing values by tallying the number of C's, Q's, and S's
# and then subtracting from the total number of observations
# From str(titanic2) we know there are 1310 observations
# 1310 - 1307 = 3 missing values

for(i in 1:length(titanic2$embarked)){
  if (titanic2$embarked[i] != "C" & titanic2$embarked[i] !="Q" & titanic2$embarked[i] != "S")
    titanic2$embarked[i] = "S"
}

# Verify results.  Should be C:270 + Q:123 + S:917 = 1310
summary(titanic2)
# There are now C:270 + Q:123 + S:917 = 1310

#. 2. Age
# Calculate the mean of the Age column
average_age <- mean(titanic2$age, na.rm = TRUE)
average_age

# Compare result
summary(titanic2)
# average_age = 29.88113, summary mean = 29.881
# There are 264 NAs in the age column

# Use the mean age to populate the missing values in the age column

for(i in 1:length(titanic2$age)){
  if(is.na(titanic2$age[i]))
    titanic2$age[i] = average_age
}

# Verify results
summary(titanic2)
# 0 NAs in the age column.  Mean is still 29.8811

# 3. Lifeboat
# Fill in the missing values of the lifeboat column with the dummy value "None" or "NA"

# Determine the number of missing values 
summary(titanic2)
sum(is.na(titanic2$boat))
#unable number of NAs from summary or is.na()

str(titanic2)
# str(titanic2) shows boat as a factor with 28 levels including "" (i.e., an empty string)

# convert boat column to character then convert empty strings into missing values
titanic2$boat <- zap_empty(as.character(titanic2$boat))
  
# check results
sum(is.na(titanic2$boat))

# 4. Cabin
# The fact that a cabin number is missing might be a useful indicator of survival
# Create a new column has_cabin_number which has 1 if ther is a cabin number, and 0 otherwise

# Determine the number of missing values
sum(is.na(titanic2$cabin))
# cannot determine missing values using is.na()

class(titanic2$cabin)
levels(titanic2$cabin)
# cabin column is a factor with 187 levels, including "" (i.e., empty string)


# convert boat column to character then convert empty strings into missing values
titanic2$cabin <- zap_empty(as.character(titanic2$cabin))

# check results
sum(is.na(titanic2$cabin))

# create a new column has_cabin_number which has 1 if there is a cabin number, and 0 otherwise
has_cabin_number <- vector("integer", length(titanic2$cabin))

for(i in 1:length(titanic2$cabin)){
  if(is.na(titanic2$cabin[i])){
    has_cabin_number[i] = 0
  } else {
    has_cabin_number[i] = 1
  }
}

# Check results. The sum of the 1s should equal the number of non-missing values
sum(has_cabin_number) == length(titanic2$cabin) - sum(is.na(titanic2$cabin))

# Add the has_cabin_number column to titanic2 dataset

titanic2 <- cbind(titanic2, has_cabin_number)

# Verify change
head(titanic2)

str(titanic2)

# embarked column still shows 4 levels, should be only 3
titanic2$embarked <- factor(x = titanic2$embarked, levels = unique(titanic2$embarked))

# Verify change
str(titanic2)

# Store cleaned up data set to the file titanic_clean.csv

write.csv(titanic2, "titanic_clean.csv", row.names = FALSE)

# COMBS Project

# First off, save this file to the location with your data. 
# Then in the menu bar above, hit Session > Set Working Directory > To Source File Location

# Need these libraries:
# use install.packages("library name") if you haven't installed them yet
library(tidyverse)
library(jsonlite)
library(strex)

# - - - LOADING THE DATA - - - 

original_data <- read.csv("heritage-combs-classifications.csv",header = TRUE)

# 1. Loading data
classes <- read.csv("heritage-combs-classifications.csv",header = TRUE)

# 2. Which columns do we need?
# Only keeping classification_id, user_name, user_id, workflow_name, created_at, annotations, and subject_data
classes <- classes[,c(1,2,3,6,8,12,13)]

# - - - REMOVING BAD USERS - - - 

# 3. Removing any bad users or the usernames from your group

# Create a vector of the user names you want to remove
user_rem <- c('bad.user','bad.user2')
# Remove these users from the dataframe
classes <- classes[-which(classes$user_name %in% user_rem),]

# The function 'which' gives the row numbers for which the condition is true
# The function '%in%' is true if the user name is in our bad user names vector
# The '-' sign tells R to *not* include the rows for which the condition was true

# After doing the above, the rows kept their old numbers, so reset your row indices 
rownames(classes) <- NULL

# - - - ASSIGNING NEW USER IDS - - - 

# vector of unique user names
users <- unique(classes[,2])

# for loop to standardise user numbers
user_num <- 1
for (u in users) {
  classes[classes$user_name == u,3] <- user_num
  user_num = user_num + 1
}

# Anonymize the data by removing user_name and saving as a new dataframe
classA <- classes[,-2]

# - - - BREAKING DOWN THE ANNOTATIONS COLUMN - - - 

# In the Zooniverse output, the questions are:
# T0: decoration, T1: damage, T2: aesthetic value, T3: historic value, T4: display, T5: comments
# So we need 6 new columns - columns 7 to 12 in the classA datframe

# Note how crazy the annotations column looks!
# classA[1,5]
# The function fromJSON will extract the values from this column, 
# but it has to be used one row at a time:

# Get the number of rows in the dataframe
n <- dim(classA)[1]
# The for loop will go through the rows one-by-one down the dataframe
for (i in 1:n) {
  # Extracting a vector of this row's responses and saving it into 6 new columns
  classA[i,7:12] <- fromJSON(classA[i,5])$value
}

# Replacing column names with zooniverse format
colnames(classA)[7:12] <- c('T0','T1','T2','T3','T4','T5')

# We really only need the 1-5 values from the answers
# substr(x,1,1) grabs only the first character in the vector
for (i in 7:10) {
  classA[,i] <- as.integer(substr(classA[,i],1,1))
}

# - - - GETTING THE SUBJECT DATA - - - 

# simplifying subject_data vector so that fromJSON will work
classA$subject_data <- substr(classA[,6],13,nchar(classA[,6])-1)

# creating new column for file names
classA$filename <- rep("",n)
# for loop to extract file names from column
for (i in 1:n) {
  classA[i,13] <- fromJSON(classA[i,6])$Filename
}

# creating a new column that is just the comb number
classA$comb_num <- str_first_number(classA$filename)

# Removing the JSON columns
classA <- classA[,-c(5,6)]

# Extract the A/B values from the workflow column
classA$workflow_name <- substr(classA$workflow_name,9,9)

# Exporting the cleaned-up data as a CSV file
write_csv(classA,"cleanedCombData.csv")




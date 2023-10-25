# /// DatabaseRecycler \\\

### INSTALL PACKAGES (IF NECESSARY)
# install.packages("dplyr") # Data manipulation/transformation
# install.packages("openxlsx") # Reading/writing/editing excel files

### PACKAGES
library("dplyr")
library("openxlsx")

### WORKING DIRECTORY
getwd() # setwd("/path/to/your/directory") (IF NECESSARY)

### FILE
dataset <- read.csv("dataset.csv") # Change according to your file name
# test_original <- head(dataset,1000) # View without loading the entire db

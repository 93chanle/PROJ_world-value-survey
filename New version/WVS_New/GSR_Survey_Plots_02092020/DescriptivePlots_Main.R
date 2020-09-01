#---------------------------------------------------------------------------------------------------
### ---- GSR SURVEY 2020 ---- ###
### ---- VISUALIZAION ---- ###
### ---- MAIN SCRIPT ---- ###
### ---- by Chan Le, last updated 02.09.2020 ---- ###
#---------------------------------------------------------------------------------------------------

{
library(Amelia)
library(stringr)
library(tidyverse)
library(readxl)
library(stringi)
library(R.utils)
}

source("DescriptivePlots_Functions.R")

# 01. Get started ----------------------------------------------------------------------------------

# Load in the data
{
  
data <- read_xlsx("Leuphana GSR Survey_August 26, 2020_08.52.xlsx")
dataDateTime <- read.csv("Leuphana GSR Survey_August 26, 2020_08.51.csv")
# Take out the column names
colData <- colnames(data)
# Filter only the questions part of the data
data <- data[, str_detect(colData, "Q\\d") == T]

}

# 02. Extract question (can skip, for quick check only) --------------------------------------------
# # Fill in question ID
# quesID <- "Q165"
# 
# # Run to view question data
# data %>% select(matches(paste0(quesID,"($|_|\\.)"))) %>% View()

# 03. Plotting -------------------------------------------------------------------------------------

# Currently, there are 4 different types of plots.
# There are two common arguements in all plot functions to control the line breaks of the title
# and the item labels in plot. The larger the values, the longer the text will go until a new line
# break occurs. You can test out these following scenarios to see the difference:

F_Type3("Q57", titleNewLine = 120, itemNewLine = 30)

F_Type3("Q57", titleNewLine = 120, itemNewLine = 50)

F_Type3("Q57", titleNewLine = 90, itemNewLine = 50)

# I also report N in each plot as the number of actual (non-NA) answers.

#### PLOT TYPES & EXAMPLES ####
  # In the Question excel sheet I have identify most plot types that correspond to each question. 
  # Please try out. If there's any error please let me know. 

  # F_Type1: HISTOGRAM WITH SINGLE CONTINUOUS VARIABLE 
  # You can specify the binwidth according to the variable, as well as label it correctly
  F_Type1("Q2_10", titleNewLine = 120, itemNewLine = 30, binwidth = 5, xlab = "Age")
  
  F_Type1("Q181", titleNewLine = 120, itemNewLine = 30, binwidth = 1, xlab = "Number of people")

  # F_Type2: COUNT BAR PLOTS WITH ONE ITEM
  # Please check the Quesion Excel file and put the exact question ID in the function'
  F_Type2("Q26", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q47_1", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q1", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q101", titleNewLine = 115, itemNewLine = 30)
  
  F_Type2("Q167", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q7", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q83", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q171...55", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q171...105", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q179", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q165...108", titleNewLine = 120, itemNewLine = 30)
  
  F_Type2("Q180", titleNewLine = 120, itemNewLine = 30)
  
  # F_Type6: STACKED BAR PLOTS WITH MULTIPLE ITEMS
  # For this plot type you only need the beginning of the question ID (not the suffix _1, _2, etc.)
  F_Type3("Q168", titleNewLine = 120, itemNewLine = 30)
  
  F_Type3("Q27", titleNewLine = 120, itemNewLine = 30)
  
  F_Type3("Q57", titleNewLine = 120, itemNewLine = 30)
  
  F_Type3("Q56", titleNewLine = 120, itemNewLine = 30)
  
  # F_Type6: DISCRETE HISTOGRAM PLOT WITH TWO OPPOSING STATEMENTS (1 TO 10)
  F_Type6("Q181", titleNewLine = 115, itemNewLine = 30)
  
  F_Type6("Q108", titleNewLine = 115, itemNewLine = 30)
  
  F_Type6("Q112", titleNewLine = 115, itemNewLine = 30)

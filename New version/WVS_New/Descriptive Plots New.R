
library(Amelia)
library(stringr)
library(tidyverse)
library(readxl)


# 00. Define colour scheme
theme_set(theme_bw()) 
col1 <- "#DEE1B6"
col2 <- "#73C8A9"
col3 <- "#E1B866"
col4 <- "#BD5532"
col5 <- "#373B44"


# 01. Get started ----------------------------------------------------------------------------------

# Load in the data

data <- read_xlsx("Leuphana GSR Survey_August 26, 2020_08.52.xlsx")
dataDateTime <- read.csv("Leuphana GSR Survey_August 26, 2020_08.51.csv")
# Take out the column names
colData <- colnames(data)

# Filter only the questions part of the data
data <- data[, str_detect(colData, "Q\\d") == T]


# 02. Extract question -----------------------------------------------------------------------------
colData <- colnames(data)
quesNumber <- "97"

data %>% select(matches(paste0("Q",quesNumber,"($|_|\\.)"))) %>% View()



str_detect(colData, paste0("\\."))


data[[1, "Q172"]]


data[,"Q2_10"]

# Histogram plot
F_Type1 <- function(quesID, xlab =""){
  
  question <- data[1, quesID]
  quesData <- data[-1, quesID]
  colnames(quesData) <- "ques"
  n <- quesData %>% filter(is.na(ques) == F) %>% nrow()
  quesData <- transform(quesData, ques = as.numeric(ques))
  ggplot(quesData, aes(x = ques)) +
    geom_histogram(binwidth = 5,
                   fill = col4,
                   color = "white") +
    labs(title = paste0(question, " (N = ", n, ")"),
         subtitle = paste0("ID: ", quesID)) +
    xlab(xlab) +
    ylab("Count")
  
}

# One question, multiple category
F_Type2 <- function(quesID){
  
  question <- data[1, quesID]
  quesData <- data[-1, quesID]
  colnames(quesData) <- "ques"
  n <- quesData %>% filter(is.na(ques) == F) %>% nrow()
  quesData %>% filter(is.na(ques) == F) %>% 
    ggplot(aes(x = ques)) + geom_histogram(stat = "count", fill = col4) +
    labs(title = paste0(question, " (N = ", n, ")"),
         subtitle = paste0("ID: ", quesID)) +
    ylab("Count") +
    coord_flip() +
    theme(axis.title.y = element_blank())
  
}

# One question, multiple items, scale

F_Type3 <- function(quesID){
  
  quesData <- data %>% select(matches(paste0(quesID,"($|_|\\.)")))
  question <- stri_split_fixed(quesData[1,1], " - ", tokens_only = F)[[1]][1] 
  
  # Extract the item labels
  colnames(quesData) <- quesData %>% slice(1) %>% mutate(across(everything(), str_extract, pattern = "(?<= - )[:print:]*(?=.)"))
  quesData <- quesData %>% slice(-1)
  quesData %>% pivot_longer(everything(), names_to = "Items", values_to = "Responses") %>% table() %>% as_tibble() %>% 
        ggplot(aes(x = Items, fill = Responses, y = n)) +
    geom_bar(stat = "identity", position = "dodge") + coord_flip()
  
  # Create long-format
  colnames(quesData) <- "ques"
  n <- quesData %>% filter(is.na(ques) == F) %>% nrow()
  quesData %>% filter(is.na(ques) == F) %>% 
    ggplot(aes(x = ques)) + geom_histogram(stat = "count", fill = col4) +
    labs(title = paste0(question, " (N = ", n, ")"),
         subtitle = paste0("ID: ", quesID)) +
    ylab("Count") +
    coord_flip() +
    theme(axis.title.y = element_blank())
  
}

data %>% select(matches(paste0(quesID,"($|_|\\.)"))) %>% View()
test 

stri_split_fixed(test, " - ")



                                



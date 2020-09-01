
#---------------------------------------------------------------------------------------------------
### ---- GSR SURVEY 2020 ---- ###
### ---- VISUALIZAION ---- ###
### ---- PLOT FUNCTIONS SCRIPT ---- ###
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

# # For debugging
# {
#   titleNewLine = 120
#   itemNewLine = 30
#   binwidth = 5 
#   xlab =""
# }

# 00. Define colour scheme
{
  theme_set(theme_bw()) 
  col1 <- "#042A2B"
  col2 <- "#D84727"
  col3 <- "#419DB4"
  col4 <- "#F48352"
  col5 <- "#5DCEEA"
  col6 <- "#F5C936"
  col7 <- "#DBF2F2"
  colorPallete <- c(col1, col2, col3, col4, col5, col6, col7)
}

# FUNC: Plot Histogram
F_Type1 <- function(quesID, titleNewLine = 120, itemNewLine = 30, binwidth = 5, xlab =""){
  question <- data[1, quesID]
  quesData <- data[-1, quesID]
  colnames(quesData) <- "ques"
  n <- quesData %>% filter(is.na(ques) == F) %>% nrow()
  quesData <- transform(quesData, ques = as.numeric(ques))
  
  # # Capitalize the item labels
  # str_sub(quesData$Items, 1, 1) <-
  #   toupper(str_sub(quesData$Items, 1, 1))
  
  ggplot(quesData, aes(x = ques)) +
    geom_histogram(binwidth = binwidth,
                   fill = col1,
                   color = "white") +
    labs(subtitle = createNewLineString(paste0(question, " (N = ", n, ")"), titleNewLine),
         title = paste0("ID: ", quesID)) +
    xlab(xlab) +
    ylab("Count")
}

# FUNC: Plot bars - One question, multiple category
F_Type2 <- function(quesID, titleNewLine = 140, itemNewLine = 30){
  
  quesData <- data %>% select(matches(paste0(quesID, "($|_|\\.)")))
  
  if (quesID %in% c("Q7")){quesData <- quesData[,3]}
  
  question <- data[1, quesID]
  question <- gsub("\n", " ", question)
  quesData <- data[-1, quesID]
  
  colnames(quesData) <- "ques"
  n <- quesData %>% filter(is.na(ques) == F) %>% nrow()
  quesData <- quesData %>% filter(is.na(ques) == F) %>% table() %>% as.data.frame() 
  colnames(quesData) <- c("Items","Freq")
  
  # Capitalize the item labels
  str_sub(quesData$Items, 1, 1) <-
    toupper(str_sub(quesData$Items, 1, 1))
  
  # Plot
  plot <- ggplot(quesData, aes(x = Items, y = Freq)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      fill = col2,
      width = 0.6
    )  +
    labs(subtitle = createNewLineString(paste0(question, " (N = ", n, ")"), titleNewLine),
         title = paste0("ID: ", quesID)) +
    ylab("Count") +
    coord_flip() +
    theme(axis.title.y = element_blank())
  return(plot)
}

# FUNC: Plot bars - One question, multiple items
# titleNewLine and itemNewLine determine how long string goes before a line break is created
F_Type3 <- function(quesID,
                    titleNewLine = 140,
                    itemNewLine = 30) {
  # Extract data & question label
  quesData <- data %>% select(matches(paste0(quesID, "($|_|\\.)")))
  
  if (quesID %in% c("Q172", "Q170")){quesData <- quesData[,-1]}
  
  question <-
    stri_split_fixed(quesData[1, 1], " - ", tokens_only = F)[[1]][1]
  
  question <- gsub("\n", " ", question)
  
  # Extract the item labels
  colnames(quesData) <-
    quesData %>% slice(1) %>% mutate(across(everything(), str_extract, pattern = "(?<= - )[:print:]*(?=)"))
  quesData <- quesData %>% slice(-1) %>% drop_na()
  
  # Sample size
  n <- nrow(quesData)
  
  # Make contigency table
  quesData <-
    quesData %>% pivot_longer(everything(), names_to = "Items", values_to = "Responses") %>%
    table() %>% as_tibble()
  
  # Capitalize the item labels
  str_sub(quesData$Items, 1, 1) <-
    toupper(str_sub(quesData$Items, 1, 1))
  
  # Create custom color pallete for plot
  cusColor <- FUNC_createColorPallete(colorPallete = colorPallete, colorNum = length(unique(quesData$Responses)))
  
  # Plot
  plot <-
    ggplot(quesData,
           aes(
             x = createNewLineStringList(Items, itemNewLine),
             fill = Responses,
             y = n
           )) +
    geom_bar(stat = "identity",
             position = position_fill(reverse = FALSE),
             width = 0.6) +
    labs(title = paste0("ID: ", quesID),
         subtitle = createNewLineString(paste0(question, " (N = ", n, ")"), titleNewLine)) +
    ylab("Percentage") +
    theme(
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    ) + scale_fill_manual(values = cusColor, guide = guide_legend(reverse = TRUE)) +
    coord_flip()
  return(plot)
}

F_Type4 <- function(quesID,
                    titleNewLine = 140,
                    itemNewLine = 30) {
  # Extract data & question label
  quesData <- data %>% select(matches(paste0(quesID, "($|_|\\.)")))
  
  if (quesID == "Q172"){quesData <- quesData[,-1]}
  
  question <-
    stri_split_fixed(quesData[1, 1], " - ", tokens_only = F)[[1]][1]
  
  question <- gsub("\n", " ", question)
  
  # Extract the item labels
  colnames(quesData) <-
    quesData %>% slice(1) %>% mutate(across(everything(), str_extract, pattern = "(?<= - )[:print:]*(?=)"))
  quesData <- quesData %>% slice(-1) %>% mutate_all(as.numeric)
  
  # Make contigency table
  quesData <-
    quesData %>% pivot_longer(everything(), names_to = "Items", values_to = "Responses") 
  
  # Sample size
  n <- sum(quesData$n)
  
  # Capitalize the item labels
  str_sub(quesData$Items, 1, 1) <-
    toupper(str_sub(quesData$Items, 1, 1))
  
  # Plot
  ggplot(quesData,
         aes(x = Responses,
             y = Items)) + geom_bin2d() 
  geom_bar(stat = "identity", position = position_fill(reverse = FALSE), width = 0.6) +
    labs(title = paste0("ID: ", quesID),
         subtitle = createNewLineString(paste0(question, " (N = ", n, ")"), titleNewLine)) +
    ylab("Percentage") +
    theme(
      axis.title.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
  
  return(plot)
}

F_Type6 <- function(quesID,
                    titleNewLine = 140,
                    itemNewLine = 30) {
  # Extract data & question label
  quesData <- data %>% select(matches(paste0(quesID, "($|_|\\.)")))
  
  if (quesID %in% c("Q172", "Q181")){quesData <- quesData[,-1]}
  
  question <-
    stri_split_fixed(quesData[1, 1], " - ", tokens_only = F)[[1]][1]
  
  # Extract the item labels
  colnames(quesData) <-
    quesData %>% slice(1) %>% mutate(across(everything(), str_extract, pattern = "(?<= - )[:print:]*(?=)"))
  
  # Capitalize the item labels
  
  options <- stri_split_fixed(colnames(quesData), ":")[[1]]
  str_sub(options, 1, 1) <-
    toupper(str_sub(options, 1, 1))
  
  quesData <-
    quesData %>% slice(-1) %>% mutate_all(as.integer) %>% drop_na()
  
  # Sample size
  n <- nrow(quesData)
  
  quesData <- quesData %>% table() %>% as.data.frame()
  colnames(quesData) <- c("Score","Freq")
  
  # Plot
  plot <- ggplot(data = quesData, aes(x = Score, y = Freq, fill = as.numeric(Score))) + 
    geom_bar(stat = "identity", position = "dodge", width = 0.7) + xlab(paste0(options[1], "      -----------------------------      ", options[2])) +
    scale_x_discrete(limits = 1:10) +
    labs(title = paste0("ID: ", quesID),
         subtitle = createNewLineString(paste0(question, " (N = ", n, ")"), titleNewLine)) +
    ylab("Count") +
    scale_fill_gradient(low = col1, high = col5) +
    theme(legend.position = "none")
  return(plot)
}

# FUNC: Create auto new line for a list of string
# Arg. newLine: determine how long string goes before a new line is created
createNewLineStringList <- function(stringList, newLine = 15){
  for (i in (1:length(stringList))){
    stringList[i] <- createNewLineString(stringList[i], newLine = newLine)
  }
  return(stringList)
}

# FUNC: Create auto new line for a string
# Arg. newLine: determine how long string goes before a new line is created
createNewLineString <- function(string, newLine = 15){
  stringCopy <- string
  space <- stri_locate_all_regex(string, "\\s")[[1]][,1]
  space <- ifelse(is.na(space) == T, 0, space)
  if (max(space) > newLine){
    checkLength <- seq(newLine, max(space), by = newLine)
    for (i in checkLength){
      toSpace <- max(space[space < i])
      fromSpace <- min(space[space >= i])
      if (i - toSpace < fromSpace - i){
        substr(string, toSpace, toSpace + 2) <- "\n"
      } else {
        substr(string, fromSpace, fromSpace + 2) <- "\n"
      }
    }
    return(string)
  } else {return(stringCopy)}
}
  
FUNC_createColorPallete <- function(colorPallete, colorNum){
  colorVector <- c(colorPallete[1])
  insertInd <- 2
  for (i in 2:colorNum){
    colorVector <- insert(colorVector, insertInd, colorPallete[i])
    if (length(colorVector) %% 2 == 1) {insertInd = insertInd + 1}
  }
  return(colorVector)
}

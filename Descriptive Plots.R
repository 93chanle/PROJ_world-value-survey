#---------------------- Plotting World Value Survey Responses ------------------------#

#-------------------------------------------------------------------------------------#

#---------------------- Chan Le, last edited 09.02.2020 ------------------------------#

# install.packages("xlsx")
# install.packages("readxl")
# install.packages("rJava")

{
  library(tidyverse)
  library(xlsx)
  library(rJava)
  library(readxl)
  library(stringr)
  
  theme_set(theme_bw())
}


# Excel input is the imported CSV into Excel WITHOUT data type detection (set to NO)

# Extract header as questions (otherwise special characters will be ignored when colnames it read)
data <- read_excel("World Value Survey UTF-8.xlsx", sheet = 1)
colnames(data) <- data[1,]
data <- tail(data, -1)

# Extract questions from header
quesLabel <- data %>% select(-Timestamp) %>% colnames()

## CLEAN QUESTIONS FROM HEADER ----
# For questions with multiple subquestions, clean the sub questions
quesLabel <- quesLabel %>% str_replace( "\\[.*", "")

# Delete duplicates from questions / Select unique questions
quesLabel <- unique(quesLabel)


# Because there are different types of questions, some have subquestions/sub-statements (more columns in data), 
# we want to extract each question to a data frame

# Extract indexes of question
index <- data %>% select(-Timestamp) %>% colnames() %>% 
  str_extract("[0-9]+")

# Split questions and set them in right order
quesData <- data %>% select(-Timestamp) %>% split.default(., index) 
quesData <- quesData[order(as.numeric(names(quesData)))]

# Removing the main question part from the data, leaving only statements behind (applied to questions with 
# multiple sub statement)

for (i in 1:length(quesData)){
  names(quesData[[i]]) <- quesData[[i]] %>% names() %>% str_remove(.,".*\\[") %>% str_remove(.,"\\]")
}

# Question Type 01: Multiple sub questions/statements, with answers as order factors
#------------------------#

FUN.quesType1 <- function(number){
  ques <- quesData[[number]]
  # View(ques)
  
  # Unlist, unname: Strip statements from data frame to vector of characters
  statement <- names(ques)
  
  # Extract question label for the question
  label <- quesLabel[number] %>% str_wrap(100)
  
  # Rename the columns in question table (some dummy names)
  colnames(ques) <- labelSingle
  
  # Gather to prepare
  ques <- ques %>% pivot_longer(everything(), values_to = "Measurement", names_to = "Labels")
  
  # Create frequency table for plotting
  quesFreq <- as.data.frame(table(ques))

  # plot <- ggplot(quesFreq, aes(x = Measurement, group = Labels, y = Freq, fill = Measurement)) + 
  #   geom_bar(stat = "identity") + 
  #   facet_grid(Labels ~.) + coord_flip() +  theme(axis.text.y=element_blank(), 
  #                                                 axis.title.y = element_blank())
  
  plot <- ggplot(quesFreq, aes(x = Measurement, group = Labels, y = Freq, fill = Measurement)) + 
    geom_bar(stat = "identity", position = position_dodge2(reverse = FALSE)) + 
    facet_wrap(Labels ~., nrow = length(statement), ncol = 1) + 
    coord_flip() +  
    theme(axis.text.y=element_blank(), axis.title.y = element_blank(), #Hide vertical axes labels and title
          plot.subtitle = element_text(hjust = 0.5)) + #Center plot title
    labs(subtitle = label) +
    guides(fill = guide_legend(reverse = FALSE)) + #Flip order of legends
    geom_label(aes(label= Freq), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) 
  # Add numbers on top of bar plot

  plot
  
return(plot)}

# Question Type 02: Multiple boxes as answers. WOrks for all: (select ALL that applies, select SOME
# that apply, select only ONE box). Input is (1) question number and (2) the number of check boxes
#------------------------#

FUN.quesType2 <- function(number, checkBoxes = 30){  
    
  # Extract question from data
  ques <- quesData[[number]]
  
  # Extract question label for the question
  label <- quesLabel[number] %>% str_wrap(100)
  
  # Some dummy labels to fill in when separate data, No. equals to the No. of check boxes
  labelInto <- paste0("Label",seq(1, checkBoxes, 1))
  
  # Separate columns into several
  ques <- separate(ques,1, into = labelInto, sep = ";")
  
  # For people who choose none, replace "" to "None of the above" for final results
  ques$Label1 <- replace(ques$Label1, is.na(ques$Label1) == TRUE, "None of the above")
  
  # Gather data
  ques <- ques %>% pivot_longer(everything(), values_to = "Values", names_to = "Measurements")
  
  # Create freqency table, independent of in what order the boxes habe been checked
  quesPlot <- as.data.frame(table(ques)) %>% group_by(Values) %>% summarise(Final = sum(Freq)) 
  
  # Warp labels 
  quesPlot$Values <- str_wrap(quesPlot$Values, 40)
  
  # Plot
  plot <- ggplot(quesPlot, aes(x = reorder(Values, Final), y = Final)) + # Reorder the data before plotting
          geom_bar(stat = "identity", fill = "#912631", width = 0.5) + coord_flip() +
          labs(subtitle = label) +
          theme(axis.title.y = element_blank(), # Hide y axis title
          plot.subtitle = element_text(hjust = 0.5)) + # Center the subtitle
          guides(fill = guide_legend(reverse = TRUE)) + # Flip order of legends
          geom_label(aes(label=Final), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) 
  # Add numbers on top of bar plot

return(plot)
}


# Question Type 3: One question with one scale 1 - 10. Output prints "histogram" & mean value.
#------------------------#

FUN.quesType3 <- function(number){

# Extract question from data
ques <- quesData[[number]]

# Extract question label for the question
label <- quesLabel[number] %>% str_wrap(100)

# Convert to numeric
ques <- ques[,1] %>% pull() %>% as.data.frame()

colnames(ques) <- "Count"

# Plot
plot <- ggplot(ques, aes(x = Count)) + geom_bar(fill = "#912631") + 
        geom_vline(aes(xintercept=mean(Count)), color="grey", linetype="dashed", size=1) +
        labs(subtitle = str_wrap(label, width = 100)) +
        theme(axis.title.y = element_blank(), #Hide y axis title
        plot.subtitle = element_text(hjust = 0.5)) #Center the subtitle
        # geom_label(aes(label=Final), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) # Add numbers on top of bar plot

return(plot)
}


# Question 1, 4, 5, 7
FUN.quesType1(1)

# Question 1, 4, 5, 7
FUN.quesType2(14)


FUN.quesType3(22)





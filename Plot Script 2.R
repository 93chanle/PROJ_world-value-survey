#---------------------- Plotting World Value Survey Responses ------------------------#

#-------------------------------------------------------------------------------------#

#---------------------- Chan Le, last edited 09.02.2020 ------------------------------#



library(tidyverse)
library(xlsx)

theme_set(theme_bw())


# Extract header as questions (otherwise special characters will be ignored when colnames it read)
data <- read.xlsx2("World Value Survey UTF-8.xlsx", 1, header = F)

headers <- data[1, ]

# Extract header as questions (otherwise special characters will be ignored when colnames it read)
data <- read.xlsx2("World Value Survey UTF-8.xlsx", 1, header = T)


# Read label file
labelAll <- read.xlsx2("Questions & Labels.xlsx", sheetName = "Labels", header = F)

# Read question label file
questionLabelAll <- read.xlsx2("Questions & Labels.xlsx", sheetName = "Questions", header = F)

#Split whole data into list of single questions
questions <- list()

for (i in 1:questions.number){
  questions[[i]] <- data %>% select(starts_with(paste0("X", i, ".")))
}

#------- Extract label names for questions ------#


# Question Type 01: Multiple sub questions/statements, with answers as order factors
#------------------------#

number <-  19

FUN.quesType1 <- function(number){
  ques <- questions[[number]]
  # View(ques)
  
  # Unlist, unname: Strip labels from data frame to vector of characters
  labelSingle <- labelAll[number,] %>% unlist() %>% unname() %>% as.character()
  labelSingle <- labelSingle[!labelSingle == ""]
  
  # Extract question label for the question
  questionLabelSingle <- questionLabelAll[number,] %>% unlist() %>% unname() %>% as.character() %>% str_wrap(100)
  
  # Rename the columns in question table (some dummy names)
  colnames(ques) <- labelSingle
  
  # Gather to prepare
  ques <- ques %>% pivot_longer(everything(), values_to = "Measurement", names_to = "Labels")
  
  # Create frequency table for plotting
  quesFreq <- as.data.frame(table(ques))
  
  # Warp labels
  quesFreq$Labels <- str_wrap(quesFreq$Labels, 80)
  
  # plot <- ggplot(quesFreq, aes(x = Measurement, group = Labels, y = Freq, fill = Measurement)) + 
  #   geom_bar(stat = "identity") + 
  #   facet_grid(Labels ~.) + coord_flip() +  theme(axis.text.y=element_blank(), 
  #                                                 axis.title.y = element_blank())
  
  plot <- ggplot(quesFreq, aes(x = Measurement, group = Labels, y = Freq, fill = Measurement)) + 
    geom_bar(stat = "identity", position = position_dodge2(reverse = FALSE)) + 
    facet_wrap(Labels ~., nrow = length(labelSingle), ncol = 1) + 
    coord_flip() +  
    theme(axis.text.y=element_blank(), axis.title.y = element_blank(), #Hide vertical axes labels and title
          plot.subtitle = element_text(hjust = 0.5)) + #Center plot title
    labs(subtitle = questionLabelSingle) +
    guides(fill = guide_legend(reverse = FALSE)) + #Flip order of legends
    geom_label(aes(label= Freq), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) # Add numbers on top of bar plot

  plot
  
return(plot)}

# Question Type 02: Multiple boxes as answers. WOrks for all: (select ALL that applies, select SOME
# that apply, select only ONE box). Input is (1) question number and (2) the number of check boxes
#------------------------#

FUN.quesType2 <- function(number, checkBoxes = 30){  
  
# Extract question from data
ques <- questions[[number]]

# Extract question label for the question
questionLabelSingle <- questionLabelAll[number,] %>% unlist() %>% unname() %>% as.character() %>% str_wrap(100)

# Some dummy labels to fill in when separate data, No. equals to the No. of check boxes
labelInto <- paste0("Label",seq(1, checkBoxes, 1))

# Separate columns into several
ques <- separate(ques,1, into = labelInto, sep = ";")

# For people who choose none, replace "" to "None of the above" for final results
ques$Label1 <- replace(ques$Label1, ques$Label1 == "", "None of the above")

# Gather data
ques <- ques %>% pivot_longer(everything(), values_to = "Values", names_to = "Measurements")

# Create freqency table, independent of in what order the boxes habe been checked
quesPlot <- as.data.frame(table(ques)) %>% group_by(Values) %>% summarise(Final = sum(Freq)) 

# Warp labels 
quesPlot$Values <- str_wrap(quesPlot$Values, 40)

# Plot
plot <- ggplot(quesPlot, aes(x = reorder(Values, Final), y = Final)) + # Reorder the data before plotting
        geom_bar(stat = "identity", fill = "#912631") + coord_flip() +
        labs(subtitle = questionLabelSingle) +
        theme(axis.title.y = element_blank(), # Hide y axis title
        plot.subtitle = element_text(hjust = 0.5)) + # Center the subtitle
        guides(fill = guide_legend(reverse = TRUE)) + # Flip order of legends
        geom_label(aes(label=Final), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) # Add numbers on top of bar plot

return(plot)
}


# Question Type 3: One question with one scale 1 - 10. Output prints "histogram" & mean value.
#------------------------#

FUN.quesType3 <- function(number){

# Extract question from data
ques <- questions[[number]]

# Extract question label for the question
questionLabelSingle <- questionLabelAll[number,] %>% unlist() %>% unname() %>% as.character()

# Convert to numeric
ques <- as.numeric(ques[,1]) %>% as.data.frame()
colnames(ques) <- "Count"

# Plot
plot <- ggplot(ques, aes(x = as.factor(Count))) + geom_bar(fill = "#912631") + 
        geom_vline(aes(xintercept=mean(Count)), color="grey", linetype="dashed", size=1) +
        labs(subtitle = str_wrap(questionLabelSingle, width = 100)) +
        theme(axis.title.y = element_blank(), #Hide y axis title
        plot.subtitle = element_text(hjust = 0.5)) + #Center the subtitle
        geom_label(aes(label=Final), position=position_dodge(width=0.9), color = "white", fill = "#6A4A3C", size = 4, hjust = 0.5) # Add numbers on top of bar plot

return(plot)
}


FUN.quesType1(19)
FUN.quesType2(14)
FUN.quesType3(22)





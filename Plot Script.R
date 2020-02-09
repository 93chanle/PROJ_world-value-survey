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


test <- data %>% select(starts_with(paste0("X", 1, ".")))

questions.number <- 116




questions <- list()

for (i in 1:questions.number){
  questions[[i]] <- data %>% select(starts_with(paste0("X", i, ".")))
}


#------- Extract label names for questions ------#

#Choose question
ques <- questions[[7]]

#Get labels
label.names <- c()

for (i in 1:length(colnames(ques))){
  divided <- print(strsplit(colnames(ques[i]), "..", fixed = TRUE))
  label.names[i] <- divided[[1]][[length(divided[[1]])]]
}

label.names <- gsub("\\.", " ", label.names)

colnames(ques) <- label.names

#Gather for plot
ques.plot <- ques %>% pivot_longer(everything(), names_to = "Labels", values_to = "Measurement")

#Plot bar
ggplot(ques.plot, aes(x = Measurement, group = Labels)) + geom_bar() + facet_grid(. ~ Labels)
ggplot(ques.plot, aes(x = Labels, group = Measurement)) + geom_bar() + facet_grid(. ~ Measurement)



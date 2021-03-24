library(processx)
library(dplyr)
library(reshape2)
library(ggplot2)

#1. Read in new text from .PDF files
newtext<-readtext::readtext("PDF_tests/*pdf")

#2. Save .PDF files as .txt files
write.table(newtext$text[1], "SDGclassy-master/project-simone/full_texts_test/Marine.txt")
write.table(newtext$text[2], "SDGclassy-master/project-simone/full_texts_test/Poverty.txt")

#3.Run mallet model on .txt files
run("SDGclassy-master/project-simone/infer-scores_simone.sh")

#4. Modify and display results 
  #4.1 Read in mallet scores as table
  output<-read.table("SDGclassy-master/project-simone/output/scores-full_texts_test-cl_base_new.txt")
  output<-output[,2:20]

  #4.2 Give table SDG names
colnames(output)<-c("ID", "Zero Hunger", "Partnerships", "Decent Work", "No Poverty",
                    "Clean Water", "Peace and Justice", "Industry and Innovation",
                    "Sustainable Cities", "Life on Land", "Quality Education",
                    "Climate Action", "Gender Equality", "Responsible Consumption",
                    "Good Health", "Affordable Energy", "Filter", "Reduced Inequalities",
                    "Life Below Water")

  #4.3 Remove filter topic
output<-output %>% select("ID", "Zero Hunger", "Partnerships", "Decent Work", "No Poverty",
                          "Clean Water", "Peace and Justice", "Industry and Innovation",
                          "Sustainable Cities", "Life on Land", "Quality Education",
                          "Climate Action", "Gender Equality", "Responsible Consumption",
                          "Good Health", "Affordable Energy", "Reduced Inequalities",
                          "Life Below Water")

  #4.4 Rename rows to correspond with .txt files
rownames(output)<-c("Poverty.txt", "Marine.txt")

  #4.5 Drop ID column
output<-output[,2:18]

  #4.6 Recalculate SDG scores (after removal of filter topic) so that the total sums to 1 
data <- apply(output, 1, function(output) output/ sum(output, na.rm = TRUE))
data <- t(data)
data<-as.data.frame(data)

  #4.7 Reorder table into SDG order

SDGorder<-c("No Poverty","Zero Hunger", "Good Health","Quality Education", "Gender Equality","Clean Water", "Affordable Energy", "Decent Work", "Industry and Innovation",
            "Reduced Inequalities","Sustainable Cities", "Responsible Consumption", 
            "Climate Action", "Life Below Water", "Life on Land","Peace and Justice",
            "Partnerships")

Finaltable<-data[SDGorder]

# 4.8 read in SDG colour chart
colours<-read.csv("SDGcolours.csv")

# 4.9 calculate mean SDG scores for all .txt files
Mean.table<-colMeans(Finaltable)
Mean.table<-as.data.frame(Mean.table)
Mean.table<-t(Mean.table)
mean<-melt(Mean.table)
each<-t(Finaltable)
each<-melt(each)
colnames(each)<-c("Goal", "ID", "Value")
colnames(mean)<-c("ID", "Goal", "Value")
mean$ID<-"Mean"

all<-rbind(each, mean)
all$colour<-rep(colours$Colour, 3)

g<-ggplot(all, aes(y=Value, x=ID)) + 
  geom_col(stat="identity", fill=all$colour)+
  coord_flip()+
  theme_classic()+
  xlab("Mean SDGs")+
  ylab("Proportion fit")


g

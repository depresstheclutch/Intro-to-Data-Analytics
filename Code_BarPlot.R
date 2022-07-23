getwd()                                                      # Get the work directory path
setwd("C:/Users/Nithi/Documents/University of Windsor/SEM -2/Introduction to Data Analytics/Assignment 1") # Set the work directory path
Employee_Data <- read.csv("Averages.csv")                    # Read the CSV file
Employee_Data <- na.omit(Employee_Data)                      # Remove incomplete cases in data frame (In this case no data was compromises)
library(ggplot2)                                             # Visualization plotting package used to represent complete plot graphics
library(tidyverse)                                           # R package used to tidy up data
library(ggpubr)                                              # R package used to create graphs based on ggplot2               
Errval <- std.error(Employee_Data$Satisfaction_Average)      # Found the standard error of Satisfaction_Average and assigned the result to Errval
ggplot(aes(x = reorder(Department,-Satisfaction_Average),y = Satisfaction_Average,group=Salary,fill=Salary),data = Employee_Data,)+
scale_y_continuous(limit = c(0,100),expand = c(0,0))+        # ggplot function - map x and y parameters and order them, and fill the legend with salary range and declare Y axis range till 100
geom_bar(position = "dodge2", stat = "identity")+            # Used to display bar plot and use actual values as height of bars (identity) and dodge is used to get grouped barplot
  labs(title="Employee Satisfaction in a Multi-National Company",  # Assign the labels of the entire bar plot 
       x="Department (Type)",y="Satisfaction (%)",
       caption="Spread Measure: Standard Error of Mean(SEM)",fill="Salary Range")+
geom_errorbar(aes(ymin=Satisfaction_Average-Errval,ymax=Satisfaction_Average+Errval), width=.6,position=position_dodge(0.9))+ # Display standard error of mean with error bars
scale_fill_manual(values = c("High" = "Grey", "Medium" = "#FED966", "Low" = "#95B8D9"))+ # Used to display the colors on the grouped bar plot
geom_text(aes(x=Department, y=Satisfaction_Average, label=Satisfaction_Average, group=Salary), position=position_dodge(width=0.9), vjust=-1.9)+ # Label plot
theme(                                                      # Used to manipulate the non-data parts of a ggplot2
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    panel.background = element_blank(),
    plot.title = element_text(size = 22, face = "bold",     # Define the title size and other cosmetic aspects
                              hjust = 0.5,
                              margin = margin(b = 15)),
    axis.line = element_line(color = "black"),               # Define the axis line color
    axis.title = element_text(size = 22, color = "black",    # Define the axis title size and other cosmetic aspects
                              face = "bold"),
    axis.text = element_text(size = 15, color = "black"),    # Define the size of the x-axis variables
    axis.text.x = element_text(margin = margin(t = 10)),     # Define the axis (X's) margin
    axis.text.y = element_text(size = 15, color ="black"),   # Define the size of the y-axis variables
    axis.title.y = element_text(margin = margin(r = 10)),    # Define the axis (Y's) margin
    axis.ticks.x = element_blank(),                          # Defined with blank will remove the short line on the axis
    legend.position = c(0.94, 0.9),                          # Define the position of the legend
    legend.background = element_rect(color = "black"),       # Define the background margin in the legend
    legend.text = element_text(size = 10),                   # Define the size of the text in the legend
    legend.margin = margin(t = 5, l = 5, r = 5, b = 5),      # Define the margins of the legend
    legend.key = element_rect(color = "black", fill = NA))   # Define the color and fill of the parameter's frame in legend 


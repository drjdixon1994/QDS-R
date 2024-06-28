#Install ggplot
install.packages("ggplot2")
library(ggplot2)

#Create the data
acyanotic<- c(13.1, 14.0, 13.0, 14.2, 11.0, 12.2, 13.1, 11.6, 14.2, 20.5, 13.4, 13.5, 11.6, 12.1, 13.5, 13.0, 14.1, 14.7, 12.8)
cyanotic<- c(13.0, 16.8, 17.6, 14.8, 13.9, 14.6, 13.0, 17.5, 14.8, 15.1, 19.3, 5.4)# Combine into a data framehemoglobin <-data.frame(Level = c(acyanotic, cyanotic),Group = rep(c("Acyanotic", "Cyanotic"), c(length(acyanotic), length(cyanotic))))ASSIGNMENT


# Combine into a data frame
hemoglobin <- data.frame(
  Level = c(acyanotic, cyanotic),
  Group = rep(c("Acyanotic", "Cyanotic"), c(length(acyanotic), length(cyanotic))))

# Scatterplot
ggplot(hemoglobin, aes(x = Group, y = Level)) +
  geom_point() +
  labs(title = "Scatterplot of Hemoglobin Levels by Group",
       x = "Group",
       y = "Hemoglobin Level")

#Histogram
ggplot(hemoglobin, aes(x=Level, fill=Group))  +
  geom_histogram(position = "dodge", binwidth = 0.5) +
  labs(title = "Histogram of Hemoglobin Levels by Group",
       x = "Hemoglobin Level",
       y = "Count") +
  scale_fill_brewer(palette = "Pastel1") 


#Boxplot

ggplot(hemoglobin, aes(x = Group, y = Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Hemoglobin Levels by Group",
                       x = "Group",
                       y = "Hemoglobin Level")


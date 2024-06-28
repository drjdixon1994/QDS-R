#Create the data frame
HW1 = data.frame(
  ID = c("C13", "C14", "C03", "C02", "C11", "C06", "C04", "C01", "C05", "C12", "C08", "C07", "C10"),
  gender= c(1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0),
  ethnicity = c(1, 3, 1, 2, 3, 2, 1, 2, 3, 1, 3, 2, 2),
  BMI= c(22.85, 23.22, 24.08, 38.36, 41.28, 33.72, 31.99, 19.61, 23.22, 35.55, 24.2, 33.5, 19.78),
  marital_status= c(0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
  employment_status= c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1),
  age= c(46, 38, 24, 35, 36, 28, 39, 18, 32, 45, 23, 25, 31),
  MAP= c(67.667, 81.667, 79.667, 91.667, 73.333, 95, 85.333, 74.667, 77, 100, 82.667, 78.667, 80.333),
  HR= c(46, 68, 70, 72, 64, 81, 84, 83, 54, 98, 59, 81, 76),
  cortisol= c(7.998, 5.7045, 5.802, 7.0215, 4.8825, 15.357, 10.4545, 9.993, 8.2815, 8.866, 9.0755, 7.5235, 11.224),
  norepinephrine= c(291,178,127, 239, 147, 275, 233, 164, 179, 291, 142, 260, 252),
  epinephrine= c(9.9, 12, 15, 9.9, 13, 12, 9.9, 14, 9.9, 9.9, 17, 21, 9.9),
  stress= c(0.5, 0.5, 0.5, 1.75, 0.5, 0.5, 7.5, 3, 3.25, 6.5, 0.25, 1.75, 2),
  anxiety= c(0.25, 0.75, 1.25, 2.25, 0.25, 1.25, 5.25, 2.75, 4.5, 6.25, 0.5, 1.75, 1.25),
  anger= c(0, 0.75, 0.75, 1.75, 0.25, 5.75, 1, 0.75, 3.5, 1.5, 1.25, 0.75, 1.25),
  fatigue= c(3.75, 1, 3, 5.75, 0.75, 7.75, 4.25, 3.75, 4.5, 7.25, 1.75, 3.5, 6.25))

#Making a Histogram to see age distribution

ggplot(HW1, aes(x = age)) +
  geom_histogram(position = "dodge", binwidth = 5) +
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Count")+
  scale_color_brewer(palette = "Pastel1")

#I wanted to Label the Data
HW1$marital_status <- factor(HW1$marital_status,
                        levels = c(1, 0),
                        labels = c("Married", "Not Married"))
                        
HW1$ethnicity <- factor(HW1$ethnicity,
                    levels = c(1,2,3),
                    labels = c("White", "Black", "Hispanic"))

HW1$gender <- factor(HW1$gender,
                     levels = c(1, 0),
                     labels = c("Female", "Male"))


HW1$employment_status <- factor(HW1$employment_status,
                     levels = c(1, 0),
                     labels = c("Employed", "Not Employed"))


#Making a bar chart to show ethnicity
ggplot(HW1, aes(x = ethnicity)) +
  geom_bar() +
  labs(title = "Bar Chart of Ethnicity",
       x = "Ethnicity",
       y = "Count")+
  scale_color_brewer(palette = "Pastel1")

ggplot(HW1, aes(x = gender)) +
  geom_bar() +
  labs(title = "Bar Chart of Gender",
       x = "Gender",
       y = "Count")+
  scale_color_brewer(palette = "Pastel1")

ggplot(HW1, aes(y=MAP))+
  geom_boxplot() +
  labs(title = "Box Plot of Mean Arterial Pressure",
       y = "MAP",
       x = "Overall Dataset")

ggplot(HW1, aes(y=HW1$HR))+
  geom_boxplot() +
  labs(title = "Box Plot of Heart Rate",
       x= "Overall Dataset",
       y = "Heart Rate")

#Numerical Descriptions of MAP and HR
mean_MAP<-mean(HW1$MAP)
median_MAP<-median(HW1$MAP)
max_MAP <-max(HW1$MAP)
min_MAP <-min(HW1$MAP)
iqr_MAP <-IQR(HW1$MAP)
sd_MAP <-sd(HW1$MAP)
data.frame(mean_MAP, median_MAP,min_MAP,max_MAP,iqr_MAP,sd_MAP)

mean_HR<-mean(HW1$HR)
median_HR<-median(HW1$HR)
max_HR <-max(HW1$HR)
min_HR <-min(HW1$HR)
iqr_HR <-IQR(HW1$HR)
sd_HR <-sd(HW1$HR)
data.frame(mean_HR, median_HR,min_HR,max_HR,iqr_HR,sd_HR)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           

# Scatterplots
ggplot(HW1, aes(x = HW1$HR, y = HW1$MAP)) +
  geom_point() +
  labs(title = "Scatterplot of Heart Rate versus MAP",
       x = "Heart Rate",
       y = "Mean Arterial Pressure")

ggplot(HW1, aes(x = HW1$epinephrine, y = HW1$norepinephrine)) +
  geom_point() +
  labs(title = "Scatterplot of Epinephrine versus Norepinephrine",
       x = "Epinephrine",
       y = "Norepinephrine")

#Boxplots
ggplot(HW1, aes(x = HW1$gender, y = HW1$stress)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stress Levels by Gender",
       x = "Gender",
       y = "Stress Level")
IQR_stress <- IQR(HW1$stress)

eliminated<- subset(HW1, HW1$stress > (quantile(1) - 1.5*IQR_stress) & HW1$stress < (quantile(2) +1.5*IQR_stress))

ggplot(eliminated, aes(x = eliminated$gender, y = eliminated$stress)) +
  geom_boxplot() +
  labs(title = "Boxplot of Stress Levels by Gender(outlier removed)",
       x = "Gender",
       y = "Stress Level")


#Midterm 2024 R Script
library(readxl)
Midterm1_Exam_Data1_1_ <- read_excel("C:/Users/12039/Downloads/Midterm1_Exam_Data1 (1).xlsx")
View(Midterm1_Exam_Data1_1_)

mean (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)
median (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)
sd (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)
range (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)

mean_NoABX <-mean(Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "No"])
mean_ABX <-mean (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "Yes"])


median_NoABX <-median(Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "No"])
median_ABX <-median (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "Yes"])


sd_NoABX <-sd (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "No"])
sd_ABX <-sd (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "Yes"])


min_NoABX <-min (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "No"])
min_ABX <-min (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "Yes"])


max_NoABX <-max (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "No"])
max_ABX <-max (Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`[Midterm1_Exam_Data1_1_$`Received antibiotic` == "Yes"])

data.frame(mean_NoABX,median_NoABX,min_NoABX,max_NoABX,sd_NoABX)
data.frame(mean_ABX, median_ABX,min_ABX,max_ABX,sd_ABX)

# Install and load ggplot2 if not already installed
install.packages("ggplot2")
library(ggplot2)

ggplot( Midterm1_Exam_Data1_1_, aes(x = Midterm1_Exam_Data1_1_$Sex, y = Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)) +
  geom_point() +
  labs(title = "Scatterplot of Length of Stay By Gener",x = "Sex",y = "Length of Stay")                                                             


ggplot( Midterm1_Exam_Data1_1_, aes(x = Midterm1_Exam_Data1_1_$Sex, y = Midterm1_Exam_Data1_1_$`Duration of hospital stay (in days)`)) +
  geom_boxplot() +
  labs(title = "Boxplot of Length of Stay By Gener",x = "Sex",y = "Length of Stay")                                                             

BMI <- c(33.57, 27.78, 40.81, 38.34, 29.01, 47.78, 26.86, 54.33, 28.99, 25.21, 30.49, 27.38, 36.42, 41.50, 29.39, 24.54, 41.75, 44.68, 24.49, 33.23, 47.09, 29.07, 28.21, 42.10, 27.74, 33.48, 31.44, 30.08)

data.frame(BMI = c(33.57, 27.78, 40.81, 38.34, 29.01, 47.78, 26.86, 54.33, 28.99, 25.21, 30.49, 27.38, 36.42, 41.50, 29.39, 24.54, 41.75, 44.68, 24.49, 33.23, 47.09, 29.07, 28.21, 42.10, 27.74, 33.48, 31.44, 30.08))

# creating intervals between 20 to 55 with a gap of 5 each
interval_table <- table(cut(BMI,seq(20,55,5)))
#Tabulate and turn into data.frame
BMIout <- as.data.frame(interval_table)
#Add cumFreq and proportions
BMIout <- transform(BMIout, cumFreq = cumsum(Freq), relative = prop.table(Freq))
print(BMIout)

# establish a class width
class_width = seq(20, 55, by=5)
class_width

# create a frequency table
data.cut = cut(BMI, class_width, right=FALSE)
data.freq = table(data.cut)
cbind(data.freq)


# histogram of this data
hist(BMI, 
     breaks=class_width, 
     col="slategray3", 
     border = "dodgerblue4",
     right=FALSE,
     xlab = "BMI", 
     main = "Histogram of BMI")

#overlay
par(new=TRUE)

# create a frequency polygon 
plot(data.freq, type="b",
     ylab = "",
     xlab = "", 
     main = "")

#Make a boxplot
# boxplot.default(BMI,                 
#                 ylab = "BMI",
#                 main = "Boxplot of BMI")
mean13 = 55
sd13 = 5


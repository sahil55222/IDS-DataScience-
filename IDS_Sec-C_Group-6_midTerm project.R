install.packages("cowplot")
install.packages("tidyverse")
install.packages("GGally")
install.packages("psych") 
install.packages("matrixStats")
library(cowplot)
library(GGally)
library(tidyverse)
library(psych)

##################data Exploration###########################

data <- read_csv('C:/Users/ungab/OneDrive/Desktop/Dataset.csv',show_col_types = FALSE)
data

dim(data) 
variable_names <- names(data)
print(variable_names)
str(data) 
summary(data)
colSums(is.na(data)) 
omitdata<-na.omit(data)
colSums(is.na(omitdata))

data_sd <- data %>% summarise_if(is.numeric,sd)
View(data_sd)

data_var <- data %>% summarise_if(is.numeric, var)
View(data_var)

data_mean <- data %>% summarise_if(is.numeric, mean)
View(data_mean)


#########################################################################
########################Missing Value & outlines#########################
#########################################################################


age_mode <- names(sort(table(data$Age), decreasing = TRUE))[1]
data$Age[is.na(data$Age)] <- as.numeric(age_mode)
age_counts <- table(data$Age)
mode_age <- as.numeric(names(age_counts)[which.max(age_counts)])
data$Age[data$Age %in% c(170, 172)] <- mode_age

data$Age
age_range <- range(data$Age)
print(age_range)
age_variance <- var(data$Age)
print(age_variance)
age_standard_deviation <- sd(data$Age)
print(age_standard_deviation)
age_mean <- mean(data$Age)
print(age_mean)

print(data$Sex)
deleteMissingValue<-na.omit(data$Sex)
print(deleteMissingValue)
Sex_mode <- names(sort(table(data$Sex), decreasing = FALSE))[1]
data$Sex[is.na(data$Sex)] <- as.character(Sex_mode)
sex_counts <- table(data$Sex)

data$Sex<-factor(data$Sex,levels=c('M','F'),labels=c(0,1))
View(data$Sex)



print (data$RestingBP)
RestingBP_counts <- table(data$RestingBP)
mode_RestingBP <- as.numeric(names(RestingBP_counts)[which.max(RestingBP_counts)])
data$RestingBP[data$RestingBP %in% c(-150,120)] <- mode_RestingBP
summary(data$RestingBP)
RestingBP_variance <- var(data$RestingBP)
print(RestingBP_variance)
RestingBP_standard_deviation <- sd(data$RestingBP)
print(RestingBP_standard_deviation)
RestingBP_mean <- mean(data$RestingBP)
print(RestingBP_mean)


print (data$Cholesterol)
data$Cholesterol <- ifelse(data$Cholesterol %in% c(1000, 1005), median(data$Cholesterol, na.rm = TRUE), data$Cholesterol)
summary(data$Cholesterol)
Cholesterol_variance <- var(data$Cholesterol)
print(Cholesterol_variance)
Cholesterol_standard_deviation <- sd(data$Cholesterol)
print(Cholesterol_standard_deviation)
Cholesterol_mean <- mean(data$Cholesterol)
print(Cholesterol_mean)
print(data$FastingBS)


print(data$MaxHR)
summary(data$MaxHR)
MaxHR_variance <- var(data$MaxHR)
print(MaxHR_variance)
MaxHR_standard_deviation <- sd(data$MaxHR)
print(MaxHR_standard_deviation)
MaxHR_mean <- mean(data$MaxHR)
print(MaxHR_mean)

print(data$ExerciseAngina)
ExerciseAngina_omit<-na.omit(data$ExerciseAngina)
print(ExerciseAngina_omit)
ExerciseAngina_mode <- names(sort(table(data$ExerciseAngina), decreasing = FALSE))[1]
data$ExerciseAngina[is.na(data$ExerciseAngina)] <- as.character(ExerciseAngina_mode)


##############################################################################
###########################Transformation#######################################
################################################################################

min_value <- min(data$RestingBP)
max_value <- max(data$RestingBP)
normalize <- function(x) {
  return((x - min_value) / (max_value - min_value))
}
data$Normalized_RestingBP <- normalize(data$RestingBP)


##################################################################################
#############################Data Visualization####################################
############################################################################

#Age##############

boxplot(data$Age, main = "Box Plot of Age", ylab = "Age", col = "lightblue", border = "blue", horizontal = TRUE)
hist(data$Age, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", col = "lightblue", border = "blue")

#gender###################
barplot(table(data$Sex), col = c("blue", "red"), main = "Sex Distribution")
ggplot(data, aes(x = Sex)) +
  geom_bar(fill = c("blue", "red")) +
  labs(title = "Patient's Gender")



#ChestPainType#################
chest_pain_counts <- table(data$ChestPainType)

chest_pain_df <- as.data.frame(chest_pain_counts)
names(chest_pain_df) <- c("ChestPainType", "Count")
bar_plot <- ggplot(chest_pain_df, aes(x = ChestPainType, y = Count, fill = ChestPainType)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("blue", "red", "green", "purple")) +  # Adjust colors as needed
  labs(title = "Chest Pain Type Distribution") +
  xlab("Chest Pain Type") +
  ylab("Count") +
  theme_minimal()
print(bar_plot)

#RestingBP########################################
ggplot(data, aes(x = RestingBP)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of RestingBP",
       x = "Resting BP",
       y = "Count")

#Cholesterol###################################
ggplot(data, aes(x = Cholesterol)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(
    title = "Histogram of Cholesterol",
    x = "Cholesterol",
    y = "Count"
  )

ggplot(data, aes(y = Cholesterol)) +
  geom_boxplot(fill = "blue") +
  labs(
    title = " Boxplot of Cholesterol",
    y = "Cholesterol"
  )


#FastingBS#######################
ggplot(data, aes(x = factor(FastingBS))) +
  geom_bar() +
  labs(title = "Bar Plot of FastingBS",
       x = "FastingBS",
       y = "Count")


#RestingECG#####################
ggplot(data, aes(x = RestingECG)) +
  geom_bar() +
  labs(title = "Barplot of RestingECG ")

#MaxHR###############################

ggplot(data, aes(x = MaxHR)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "histrogram of maxHR", x = "MaxHR", y = "Count")

ggplot(data, aes(y = MaxHR)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Boxplot of MaxHR", y = "MaxHR")

#ExerciseAngina#########################
ggplot(data, aes(x = factor(ExerciseAngina))) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Barplot of ExerciseAngina", x = "ExerciseAngina", y = "Count") +
  theme_minimal()

#Oldpeak########################
ggplot(data, aes(x = Oldpeak)) +
  geom_histogram(fill = "red", bins = 20) +
  labs(title = "histogram of Oldpeak", x = "Oldpeak Value", y = "Count")
#ST_Slope#########################

ggplot(data, aes(x = ST_Slope, fill = ST_Slope)) +
  geom_bar(position = "dodge") +
  labs(title = "Barplot of ST Slope",
       y = "Count") +
  scale_fill_manual(values = c("Up" = "blue", "Flat" = "green")) +
  theme_minimal()


#HeartDisease##########################

ggplot(data, aes(x = factor(HeartDisease))) +
  geom_bar(fill = c("blue", "green")) +
  labs(title = "Heart Disease of patient's", x = "HeartDisease", y = "Count") +
  scale_x_discrete(labels = c("Normal", "Heart Disease")) +
  theme_minimal()

#######################################################

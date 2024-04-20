library(tree)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(GGally)
library(cvms)

tree_data = read.csv('CancerPatientDataSets.csv')
head(tree_data)
tree_data = tree_data[, -c(1,2)] #removing the index and patient.id column as it is redundant for our exploration
names(tree_data)

#Checking for missing data
sum(is.na(tree_data))

#Converting all categorical variables to factors
for (column_name in names(tree_data)) { 
  if (column_name != "Age") {
    tree_data[[column_name]] = as.factor(tree_data[[column_name]])
  }
}

tree_data$Level <- factor(tree_data$Level, levels = c('Low', 'Medium', 'High'))

str(tree_data)

#-------------------------------------------------------------------------------

summary(tree_data) #summary of all variables

#-------------------------------------------------------------------------------
#Age Distribution in Dataset
tree_data$AgeGroup <- cut(tree_data$Age, 
                          breaks = seq(from = min(tree_data$Age, na.rm = TRUE), 
                                       to = max(tree_data$Age, na.rm = TRUE) + 1, 
                                       by = 15),
                          include.lowest = TRUE)

ggplot(tree_data, aes(x = AgeGroup, fill = AgeGroup)) +
  geom_bar(color = 'black') +
  scale_fill_viridis_d() +
  labs(title = 'Age Distribution', x = 'Age', y = 'Frequency') 

#-------------------------------------------------------------------------------
#Age Across Cancer Levels
ggplot(tree_data, aes(x = Level, y = Age, fill = Level)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = 'white') +
  labs(title = "Age Distribution Across Cancer Levels", x = "Cancer Level", y = "Age")

#-------------------------------------------------------------------------------
#Gender x Level
gender_levels <- tree_data %>%
  group_by(Gender, Level) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(Proportion = Count / sum(Count))

ggplot(gender_levels, aes(x = Gender, y = Proportion, fill = Level)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Gender", y = "Proportion of Chance", fill = "Cancer Level") +
  ggtitle("Cancer Levels by Gender")

#-------------------------------------------------------------------------------
#Age Group x Level
age_group_levels = tree_data %>%
  group_by(AgeGroup, Level) %>%
  summarise(Count = n()) %>%
  mutate(Chance = Count / sum(Count))

age_group_levels$Level <- factor(age_group_levels$Level, 
                                 levels = c("Low", 'Medium', "High"))

ggplot(age_group_levels, aes(x = AgeGroup, y = Chance, fill = Level)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "Age Group", y = "Chance of Cancer Level", fill = "Cancer Level") +
  ggtitle("Chances of Cancer Level by Age Group") 

#-------------------------------------------------------------------------------
#Air Pollution x Level
ggplot(tree_data, aes(x = Air.Pollution, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Air Pollution",
       x = "Level of Air Pollution",
       y = "Count")

#-------------------------------------------------------------------------------
#Smoking x Level
ggplot(tree_data, aes(x = Smoking, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Different Levels of Smokers",
       x = "Level of Smoker",
       y = "Count")

#-------------------------------------------------------------------------------
#Passive Smoker x Level
ggplot(tree_data, aes(x = Passive.Smoker, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels of People Subject to Various Levels of Passive Smoking",
       x = "Levels of Passive Smoking",
       y = "Count")

#-------------------------------------------------------------------------------
#Chronic Lung Disease x Level
ggplot(tree_data, aes(x = chronic.Lung.Disease, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Chronic Lung Disease",
       x = "Level of Chronic Lung Disease",
       y = "Count")

#-------------------------------------------------------------------------------
#Balanced Diet x Level
ggplot(tree_data, aes(x = Balanced.Diet, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across People Having Various Levels of a Balanced Diet",
       x = "Levels of Balanced Diet",
       y = "Count")

#-------------------------------------------------------------------------------
#Wheezing x Level
ggplot(tree_data, aes(x = Wheezing, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Wheezing Symptoms",
       x = "Level of Wheezing",
       y = "Count")

#-------------------------------------------------------------------------------
#Obesity x Level
ggplot(tree_data, aes(x = Obesity, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Obesity",
       x = "Level of Obesity",
       y = "Count")

#-------------------------------------------------------------------------------
#Dust Allergy x Level
ggplot(tree_data, aes(x = Dust.Allergy, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Dust Allergies",
       x = "Level of Dust Allergy",
       y = "Count")

#-------------------------------------------------------------------------------
#Coughing of Blood x Level
ggplot(tree_data, aes(x = Coughing.of.Blood, fill = Level)) +
  geom_bar(position = "dodge") +
  labs(title = "Cancer Levels Across Various Levels of Blood Coughs",
       x = "Level of Blood Cough",
       y = "Count")

#-------------------------------------------------------------------------------
#Decision Tree
set.seed(6)
random_index = sample(nrow(tree_data), 800) #using a 80-20 train-test split
tree_train = tree_data[random_index, ]
tree_test = tree_data[-random_index, ]

dtree_model = tree(Level ~., data = tree_train) #decision tree model

plot(dtree_model)
text(dtree_model, pretty = 0)

(tree_conf = table('Predictions' = predict(dtree_model, tree_test, type = 'class'),
                   'Actual' = tree_test$Level))

(accuracy = sum(diag(tree_conf))/sum(tree_conf) * 100)

#-------------------------------------------------------------------------------
#Random Forest
rf_model = randomForest(Level ~., data = tree_train,
                        mtry = (ncol(tree_train) - 1)/3,
                        ntree = 1000,
                        importance = TRUE) #random forest model

varImpPlot(rf_model) #plotting the variable importance plots

(rf_conf = table('Predictions' = predict(rf_model, tree_test, type = 'class'),
                 'Actual' = tree_test$Level))

(accuracy = sum(diag(rf_conf))/sum(rf_conf) * 100)

#-------------------------------------------------------------------------------

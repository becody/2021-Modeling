################ Feature Selection #######################
# Date: 10/27/2020
# Goal: see feature rankings for customer health score data 
# Author: Brittany Cody
# Contact: brittany.cody@learnplatform.com

# Install and load packages
install.packages("corrplot")
install.packages("mlbench")
install.packages("hablar")
install.packages("varhandle")
install.packages("mlbench")
library(mlbench)
library(varhandle)
library(hablar)
library(corrplot)
library(caret)
library(dplyr)
library(forcats)
library(mlbench)
library(tidyverse)
library(Hmisc)
library(tidyr)

# Load dataset
setwd("C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection")

cust_health <- read.csv("customer_health.csv",
                        stringsAsFactors = FALSE,
                        na.strings = c("NA", " "))

cust_audit <- read.csv("customer_audit.csv",
                       stringsAsFactors = FALSE,
                       na.strings = c("NA", " "))

################## Customer Health cleaning ########################
# Change type of variables to be correct for analysis
str(cust_health)

  # Change to factors 
cols <- c("LP_Org_ID__c","FiscalYear","FiscalQuarter", "renewal_outcome", "org_id")
cust_health[cols] <- lapply(cust_health[cols], factor)

  # Check 
str(cust_health)

# Check how many columns have unique values 
vapply(cust_health, function(x) length(unique(x)) > 1, logical(1L))

  # Keep columns with at least one unique value
cust_health[vapply(cust_health, function(x) length(unique(x)) > 1, logical(1L))]

  # Remove columns with majority NA 
# data_chars_as_num <- data_chars_as_num[,colSums(is.na(data_chars_as_num))<nrow(data_chars_as_num)]

# Check number of levels for each factor in the dataframe
sapply(cust_health[,sapply(cust_health, is.factor)], nlevels)


################## Customer Audit cleaning ########################
# Change type of variables to be correct for analysis
str(cust_audit)

# Assign org_id 

# Check 
str(cust_health)

# Check how many columns have unique values 
vapply(cust_health, function(x) length(unique(x)) > 1, logical(1L))

# Keep columns with at least one unique value
cust_health[vapply(cust_health, function(x) length(unique(x)) > 1, logical(1L))]


##################### Descriptive statistics for each var #####################
# Summary stats for all
summary(cust_health)

# Using describe by Hmisc
Hmisc::describe(data_chars_as_num)

# Plot all numeric vars
data_chars_as_num %>%
  select(-contains(c("pct_","NCES","org","users", "mod","logins","renewal_outcome","public_profile_enabled", "chrome_extension_enabled"))) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 10)

# Plotting all factors 
  # Choose categorical variables
cust_health %>%
  select_if(~ nlevels(.) <= 20) %>%
  keep(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()

##################### Check correlation with each dataset #####################
# Divide into smaller datasets for each module that the customer has
# Can be found with the mod_ variables

############## Mod feedback #################
# mod_feedback dataset
mod_feedback <- data_chars_as_num %>%
  filter(mod_feedback == 1) %>%
  select(-c(mod_feedback, mod_pm, mod_impact, mod_rw))

# Keep columns with at least one unique value
mod_feedback <- mod_feedback[vapply(mod_feedback, function(x) length(unique(x)) > 1, logical(1L))]

# Remove columns with majority NA 
mod_feedback <- mod_feedback[,colSums(is.na(mod_feedback))<nrow(mod_feedback)]

  # Check correlation 
corr_feedback <- cor(as.matrix(mod_feedback), use = "pairwise.complete.obs")
corr_feedback <- as.data.frame(corr_feedback)
corr_feedback[corr_feedback < 0.6 | corr_feedback ==1] <- ""
print(corr_feedback)

############### Mod impact ##################
# mod_impact dataset
mod_impact <- data_chars_as_num %>%
  filter(mod_impact == 1) %>%
  select(-c(mod_feedback, mod_pm, mod_impact, mod_rw))

# Keep columns with at least one unique value
mod_impact <- mod_impact[vapply(mod_impact, function(x) length(unique(x)) > 1, logical(1L))]

# Remove columns with majority NA 
mod_impact <- mod_impact[,colSums(is.na(mod_impact))<nrow(mod_impact)]

# Check correlation 
corr_impact <- cor(as.matrix(mod_impact), use = "pairwise.complete.obs")
corr_impact <- as.data.frame(corr_impact)
corr_impact[corr_impact< 0.6 | corr_impact ==1] <- ""
print(corr_impact)

############## Mod pm #######################
# mod_pm dataset
mod_pm <- data_chars_as_num %>%
  filter(mod_pm == 1) %>%
  select(-c(mod_feedback, mod_pm, mod_impact, mod_rw, chrome_extension_enabled, single_sign_on_enabled))

# Keep columns with at least one unique value
mod_pm <- mod_pm[vapply(mod_pm, function(x) length(unique(x)) > 1, logical(1L))]

# Remove columns with majority NA 
mod_pm <- mod_pm[,colSums(is.na(mod_pm))<nrow(mod_pm)]

# Check correlation 
corr_pm <- cor(as.matrix(mod_pm), use = "pairwise.complete.obs")
corr_pm <- as.data.frame(corr_pm)
corr_pm[corr_pm< 0.6 | corr_pm ==1] <- ""
print(corr_pm)

############## Mod rw #######################
# mod_rw dataset
mod_rw <- data_chars_as_num %>%
  filter(mod_rw == 1) %>%
  select(-c(mod_feedback, mod_pm, mod_impact, mod_rw))

# Keep columns with at least one unique value
mod_rw <- mod_rw[vapply(mod_rw, function(x) length(unique(x)) > 1, logical(1L))]

# Remove columns with majority NA 
mod_rw <- mod_rw[,colSums(is.na(mod_rw))<nrow(mod_rw)]

# Check correlation 
corr_rw <- cor(as.matrix(mod_rw), use = "pairwise.complete.obs")
corr_rw <- as.data.frame(corr_rw)
corr_rw[corr_rw< 0.6 | corr_rw ==1] <- ""
print(corr_rw)

############### Entire correlation matrix #############
# with all modules together 

# all mods dataset
all_mod <- data_chars_as_num 

# Keep columns with at least one unique value
all_mod <- all_mod[vapply(all_mod, function(x) length(unique(x)) > 1, logical(1L))]

# Remove columns with majority NA 
all_mod <- all_mod[,colSums(is.na(all_mod))<nrow(all_mod)]

# Check correlation 
corr_all <- cor(as.matrix(all_mod), use = "pairwise.complete.obs")
corr_all <- as.data.frame(corr_all)
corr_all[corr_all< 0.6 | corr_all ==1] <- ""
print(corr_all)

write.csv(corr_all, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection\\correlation_all.csv")

# Print the remaining correlation sheets and add them to one file 

write.csv(corr_feedback, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection\\corr_feedback.csv")
write.csv(corr_impact, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection\\corr_impact.csv")
write.csv(corr_pm, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection\\corr_pm.csv")
write.csv(corr_rw, "C:\\Users\\17046\\OneDrive\\Documents\\LearnPlatform\\Analysis\\Data\\Customer Health Score\\Feature Selection\\corr_rw.csv")


###################### Feature Importance ######################
# Repeatable results by setting seed
set.seed(7)
str(cust_health)

# # See what's wrong w factor levels
# counts <- cust_health %>%
#   lapply(table) %>%
#   lapply(as.data.frame)
# counts

# Rename dataset from before to prepare training set-up

features <- subset(cust_health, select = -c(LP_Org_ID__c,org_id, NCES_ID__c, Name, Name.Opportunity., time_to_renewal,
                                             LastModifiedDate.Opportunity., Fiscal, StageName, time, org_name, Type.Opportunity.))
features <- features %>%
  filter(months_to_renewal < 0) %>%
  droplevels() 

features <- subset(features, select = -c(months_to_renewal))

summary(features)

str(features)

# Check for na and infinity values
indexes <- apply(features, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(features)[indexes]

# Set each to factor columns
cols <- colnames(features)
features[cols] <- lapply(features[cols], factor)
summary(features)

# Check number of levels for remaining vars
sapply(features[,sapply(features, is.factor)], nlevels)

  # And check what those levels are
sapply(features, levels)
# Had an issue with Locale_c where it has "" as a factor level 
# Fixing that issue:

  #Define a helper function 
removeOneLevel <- function(v, badlevel){
  v[v==badlevel] = NA
  v2 = droplevels(v)
  levels(v2) = levels(v)[levels(v) != badlevel]
  return(v2)}

# Creating new dataset with no blank factor levels
featuresNew = mutate_if(features, is.factor, removeOneLevel, badlevel = "")
  # Check the levels to make sure they are correct
sapply(featuresNew, levels)
names(featuresNew)
summary(featuresNew)

features[features==""] <- NA
featuresNew <- features

NA_preproc <- function (dat) {
  for (j in 1:ncol(dat)) {
    x <- dat[[j]]
    if (is.factor(x) && anyNA(x)) dat[[j]] <- base::addNA(x)
    if (is.character(x)) dat[[j]] <- factor(x, exclude = NULL)
  }
  dat
}

test <- NA_preproc(featuresNew)

  # Make renewal_outcome a factor (this is our target)
# features$renewal_outcome <- as.factor(features$renewal_outcome)
# Use one hot encoding to create dummy variables for each value of the category
test.OHE <- model.matrix(~.-1, data = test)
str(test.OHE)
test.OHE <- as.data.frame(test.OHE)

# Then remove variables with zero variance
test.OHE <- which(!unlist(lapply(test.OHE, function(x) 0 == var(if (is.factor(x)) as.integer(x) else x))))

  # Create control dataset
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        savePredictions = "all")

# Train model
model <- train(renewal_outcome1~.,
               data = test.OHE,
               method="nnet",
               preProcess="scale",
               trControl=control,
               na.action = na.omit)
model$results

# Estimate var importance and summarize
importance <- varImp(model, scale = FALSE)
print(importance)
plot(importance)

########################## Selecting features via rpart method #########################
# Set seed again
set.seed(7)

# Use rpart on features dataset
rPartMod <- train( ,renewal_outcome, data = featuresNew, method = "rpart",na.action = na.omit)
rPartImp <- varImp(rPartMod)

print(rPartImp)
plot(rPartImp)

######################## Selecting features via Boruta method ####################
# The Boruta algorithm is a wrapper built around the random forest classification algorithm
# https://www.datacamp.com/community/tutorials/feature-selection-R-boruta
# 
# # Use same features dataset
# features <- data_chars_as_num %>%
#   select(-c(chrome_extension_enabled, single_sign_on_enabled, mod_rw, LP_Org_ID__c,
#             FiscalQuarter, FiscalYear, org_id))
# 
# # Install and load packages 
# install.packages("Boruta")
# install.packages("missForest")
# install.packages("Amelia")
# library(Boruta)
# library(missForest)
# library(Amelia)
# 
# # Seed missing values in dataset using prodNA() function
# features.mis <- prodNA(features, noNA = 0.05)
# 
# # Visualize missing values 
# missmap(features.mis)
# 
# # Create imputed datasets to run with Boruta 
# glimpse(features.mis)
# amelia_features <- amelia(features.mis,
#                           m=3,
#                           parallel = "multicore",
#                           idvars = c("renewal_outcome"))
# 
#   # Look at each imputation 
# glimpse(amelia_features$imputations[[1]])
# glimpse(amelia_features$imputations[[2]])
# glimpse(amelia_features$imputations[[3]])
# 
# # Place renewal_outcome at the end of the dataset
# names(features)
# features <- features %>% select(-renewal_outcome, renewal_outcome)
# names(features)
# 
# features$renewal_outcome <- as.factor(features$renewal_outcome)
# 
# # Since the predictor 
# # Run the RFE algorithm 
# results <- rfe(features[,1:44], features[,45], sizes = c(1:8),
#                rfeControl = RFcontrol, na.action = na.roughfix)
# 
# # Print results
# print(results)

################### Feature selection via stepwise regression #####################
# Fit logistic model
all.mod <- glm(renewal_outcome ~ ., data= features, family = "binomial") 

# Summary of model
summary(all.mod)

# Get variable importance
varImp(all.mod)

# Show
print(shortlistedVars)

################### Feature selection via random forest #####################
# Set seed
set.seed(7)

# Load Randomforest package and create model
library(randomForest)

features.rf <- randomForest(renewal_outcome~., data = features, importance = TRUE,
                            na.action = na.omit)

# na.roughfix option means: It will start by using median/mode for missing values, but then it grows a forest and computes proximities, then iterate and construct a forest using these newly filled values etc.

# Results
features.rf$importance
rf_import$
# Sort by variable importance
rf_import <- as.data.frame(varImp(features.rf))

# Show
print(rf_import)

# NEXT STEPS:
# See which variables are shared between all variable selection methods 




































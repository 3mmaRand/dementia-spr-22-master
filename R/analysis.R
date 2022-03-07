# packages
library(tidyverse)
library(janitor)
library(GGally)
library(caret) # needed for the confusion matrix

# import
patients <- read_csv("data-raw/oasis_longitudinal.csv") %>%
  clean_names()

patients$cdr <- factor(patients$cdr)

# explore
patients %>% ggplot(aes(x = group, y = age)) +
  geom_boxplot()

patients %>% ggplot(aes(x = age, y = n_wbv, color = group)) +
  geom_point() +
  geom_smooth(method = "lm")

# normalised brain volume reduces with age and is related to dementia status.
# demented have lower nbv and converted go from matched non-demented to matching demented

patients %>% ggplot(aes(x = e_tiv, y = n_wbv, color = group)) +
  geom_point()
# number of people in each cdr category
table(patients$cdr)

patients %>% ggplot(aes(x = cdr, y = age)) +
  geom_boxplot()

# where are the missing mmse values
patients %>% filter(is.na(mmse))
# they are in one patient OAS2_0181, does that patient have any other data?
patients %>% filter(subject_id == "OAS2_0181")
# yes, first visit had mmse recorded

# relationship between ses and education
patients %>% filter(visit == 1) %>%
  ggplot(aes(x = ses, y = educ)) +
  geom_point(alpha = 0.1)


# remove
#    ses because it has 19 missing values
#    the two rows with missing mmse values (one patient)
#    remove uninformative columns handedness and asf
patients_noses <- patients %>%
  select(-ses, -hand, -asf) %>%
  filter(!is.na(mmse))

summary(patients_noses)


patients_noses %>%
  select(-subject_id, -mri_id) %>%
  ggpairs(aes(colour = cdr))

patients_noses %>% group_by(group, m_f) %>% count()

# PCA
pca <- patients_noses %>%
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  prcomp(scale. = TRUE)

# Plot PCA
pca_labelled <- data.frame(pca$x, cdr = patients_noses$cdr)

ggplot(pca_labelled, aes(x = PC1, y = PC2, colour = cdr)) +
  geom_point(size = 3)

# LDA
# Linear Discriminant Analysis also works with the continuous variables and
# aims to find linear combination of variables the maximise differences
# between groups. It is supervised because we label observations by their
# class and determine the allocation rules based on these. A
# 'discriminant' is a line that separates groups.
table(patients_noses$cdr)
# make the 2s 1s so we have onl;y 3 categories
patients_noses$cdr[patients_noses$cdr == 2] <- 1
table(patients_noses$cdr)
levels(patients_noses$cdr)

patients_noses$cdr <- droplevels(patients_noses$cdr)
table(patients_noses$cdr)

# As there are three classes we have at most two linear discriminants.
# the lda() function is
# in a package called MASS. I very rarely load MASS with a library command
# since it has a function called select that conflicts with dplyr's
# select(). Thus I will use MASS:: to access its functions.

# build the model
lda <- patients_noses %>%
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  MASS::lda(grouping = patients_noses$cdr)
lda$scaling

y# To determine how well the classes were predicted the scores on LD1 and
# LD2 and the classes need to be predicted:

# predict from the model
plda <- patients_noses %>%
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  predict(object = lda)

df <- data.frame(plda$x, cdr = patients_noses$cdr)
df %>% ggplot(aes(x = LD1, y = LD2, colour = cdr)) +
  geom_point()
# How many predicted classes are the same as the actual classes:

table(plda$class, patients_noses$cdr)
#
# mat <- confusionMatrix(plda$class, patients_noses$cdr)
# Confusion Matrix and Statistics
#
# Reference
# Prediction     0   0.5   1
#           0   198  65   6
#           0.5   8  45  10
#           1     0  13  26
#
# Overall Statistics
(198 + 45 + 26) /371
206/ 371
# Accuracy : 0.7251
# 95% CI : (0.6766, 0.7699)
# No Information Rate : 0.5553
# P-Value [Acc > NIR] : 1.180e-11
#
# Kappa : 0.4805
#
# Mcnemar's Test P-Value : 5.143e-11
#
# Statistics by Class:
#
#                      Class: 0 Class: 0.5 Class: 1
# Sensitivity            0.9612     0.3659  0.61905
# Specificity            0.5697     0.9274  0.96049
# Pos Pred Value         0.7361     0.7143  0.66667
# Neg Pred Value         0.9216     0.7468  0.95181
# Prevalence             0.5553     0.3315  0.11321
# Detection Rate         0.5337     0.1213  0.07008
# Detection Prevalence   0.7251     0.1698  0.10512
# Balanced Accuracy      0.7654     0.6466  0.78977


#sensitivity proportion true positive
#specificity proportion true negative
# Pos Pred Value: prob of being class given predicted that class


# Training and Testing

# We used the same data to train the LDA model as we used to examine its
# performance. Only xx cases were in correctly classified. But this
# isn't very robust - we could have overfitting.
#
# A key part of using ML methods to make predictions is to test how good
# those predictions are. This is typically done by training the model on
# about 75% of your data and then testing it on the remainder.
#
# The caret package includes functions to help with this (as well as lots
# of ML algorithms). The name comes from Classification And REgression
# Training
#
# Split the dataset in to training and testing sets using
# createDataPartition()
#
# It returns a proportion of row numbers randomly sampled from the
# dataframe. Since it returns a list (of one item) I've added [[1]] to
# select that list element so I have a vector to work with.

ids <- createDataPartition(y = patients_noses$cdr, p = 0.75, list = FALSE)
str(ids)

# Now use those row numbers to create the training and test datasets.
train <- patients_noses %>% slice(ids)
test <- patients_noses %>% slice(-ids)

# build the model
lda <- train %>%
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  MASS::lda(grouping = train$cdr)
lda$scaling

y# To determine how well the classes were predicted the scores on LD1 and
# LD2 and the classes need to be predicted:

# predict from the model
plda <- test %>%
  select(age, educ, mmse, e_tiv, n_wbv) %>%
  predict(object = lda)

df <- data.frame(plda$x, cdr = patients_noses$cdr)
df %>% ggplot(aes(x = LD1, y = LD2, colour = cdr)) +
  geom_point()
# How many predicted classes are the same as the actual classes:

table(plda$class, patients_noses$cdr)
#
confusionMatrix(plda$class, test$cdr)










######################
######################
# Random Forest

# Another supervised learning method. It generates a re large number of
# decision trees each constructed from a different subset of the training
# data. The decision trees are then used to create a classification
# consensus.
#
# It can cope with both continuous and categorical predictors. We again
# partition our dataset in to training and testing components.


ids <- createDataPartition(patients_noses$cdr,
                           p = .75,
                           list = F)
training <- patients_noses %>%
  slice(ids) %>%
  select(age,
         educ,
         mmse,
         e_tiv,
         n_wbv,
         m_f,
         cdr)
testing <- patients_noses %>%
  slice(-ids) %>%
  select(age,
         educ,
         mmse,
         e_tiv,
         n_wbv,
         m_f,
         cdr)



# Train the model


library(randomForest)
rf_classifier <- randomForest(cdr ~ .,
                              data = training,
                              ntree = 200,
                              mtry = 3,
                              importance = TRUE)

rf_classifier


# OOB (Out-of-bag) estimate of error rate: number of incorrectly
# classified / number observations (i.e, counterpart to accuracy)

# Important of each variable:


varImpPlot(rf_classifier)


# MeanDecreaseAccuracy - loss of prediction performance when a that
# variable is excluded. MeanDecreaseGin - GINI is a measure of node
# impurity. It indicates how good that variable is for classifying the
# data.

# Confusion matrix.

prediction_for_table <- predict(rf_classifier,testing[,-7])
confusionMatrix(prediction_for_table, test$cdr)




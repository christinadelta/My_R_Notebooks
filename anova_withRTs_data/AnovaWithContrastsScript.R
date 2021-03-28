# This R script contains the same code as the R notbook "AnovaWithContrasts.Rmd". 
# Author: christina dimitriadou (christina.delta.k@gmail.com)
# Date: 27/03/2021 

# Data used here come from a simple rts/categorisation task that I conducted sometime ago. 

# load libraries
library(dplyr)
library(mosaic)
library(psych)
library(lme4)
library(lmerTest)
library(ez)
library(car)

# Load the RTs data 
data = read.csv("~/Desktop/rt_data/allcategoryrts.csv", header=FALSE, sep=";")

# add headers
colnames(data) = c("subj", "rt", "classes", "cat", "gender", "category")
View(data) # take a look at the dataset

# Let's first take a look at factor classes
# create factor classes
data$classes = factor(data$classes,
                      levels = c("1", "2"),
                      labels = c("1", "2"))

# check how many cases of each level of factor class we have
table(data$classes)

# Now create factor category
data$category = factor(data$category,
                       levels = c("1", "2", "3", "4"),
                       labels = c("1", "2", "3", "4"))

# check how many cases of each level of factor category we have
table(data$category)

# lastly, create factor gender
data$gender = factor(data$gender,
                     levels = c("1", "2"),
                     labels = c("1", "2"))

# check how many cases of each level of factor gender we have
table(data$gender)

# run descriptive stats:
summary(data)
# start with the descriptives 
# grand mean
grand.mean = mean(data$rt)

# means of rts for the two classes separately 
classes.means = with(data, tapply(rt, classes, mean)) # here, without *with* we would type data$rt, data$class, etc... 
classes.means # to visualise 

# Let's apply the above to the factor category
category.means = with(data, tapply(rt, category, mean))
category.means # to visualise 

# Lastly, we will apply the above to the factor gender
genders.means = with(data, tapply(rt, gender, mean))
genders.means

# Look at differences bewteen the means in the 2 classes and in the 4 categories
dif.classes = diff(classes.means)
dif.classes
dif.categories = diff(category.means)
dif.categories

# find combined means. First combine class with gender (2x2)
combined.classes = with(data, tapply(rt, list(classes,gender), mean))
combined.classes
# combine means of category with gender (4x2) or (2x4)
combined.category = with(data, tapply(rt, list(category,gender), mean))
combined.category

# One-Way ANOVA (ran indirectly) for factor CLASS without using a package:
data$classe2 = (data$rt - grand.mean)^2 # SS total
data$classmean = classes.means[data$classes] # mean for classes 
data$classwithin = (data$rt - data$classmean)^2 #SS within

data$classbetween = (data$classmean - grand.mean)^2 # SS between 

ss.classes = apply(data[,c("classe2", "classwithin", "classbetween")],2,sum) # sum of squares total, within, and between

dftotal = nrow(data)-1 # total degrees of freedom (DOF)
dfclass.between = nlevels(data$classes)-1 # DOF between
dfclass.within = dftotal - dfclass.between # DOF within 

fstat.classes = (ss.classes["between"]/dfclass.between) / (ss.classes["within"]/dfclass.within) # F stat = MS BETWEEN/ MS WITHIN

p.val = 1.0 - pf(fstat.classes, dfclass.between, dfclass.within) # p value

fstat.classes = unname(fstat.classes)

# or for comparison reason, let's run an anova directly:
model.1anova = aov(rt ~ classes, data)
summary(model.1anova) # look at the results

# Through linear Regression and Anova function (same formula)
model1a = lm(rt ~ classes, data)
summary(model1a)
anova1 = Anova(model1a, type = 3)
summary(anova1)

# So, here the effect of class (the beta for classses2) is the difference between 
# living and non-living objects
coef(model1a)["classes2"]
dif.classes

# Noticed that the intercept is the mean for **living class**?
coef(model1a)["(Intercept)"]
classes.means[1]

# so, intercept + classes2 is the mean value for the  non-living objects rts
coef(model1a)["(Intercept)"] + coef(model1a)["classes2"]
classes.means[2]

# Also note that the significance of this effect is the same as a t-test between the 2 
# levels of the the factor **classes**
sqrt(summary(model.1anova)[[1]]["classes", "F value"])  # F is t squared
summary(model.1anova)[[1]]["classes", "Pr(>F)"] # pvalue

# Let's check the coefficients of the **lm model** for comparisons:
coef(summary(model1a))["classes2", "t value"]
coef(summary(model1a))["classes2", "Pr(>|t|)"]

# They are the same. Running an anova with 2 levels, is the same as running a t-test 
# Now check it with an actuall t-test function
t.test(data$rt[data$classes == "2"], data$rt[data$classes=="1"])

# This is how **treatment coding** of contrasts looks like:
contr.treatment(2)





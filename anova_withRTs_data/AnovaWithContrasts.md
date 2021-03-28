---
Title: **ANOVA Contrast-Coding for Categorical Variables**

---

Author: Christina Dimitriadou (christina.delta.k@gmail.com)

Date: 27/03/2021

For this notebook I will be using data from a simple Reaction-Times task, where subjects were asked to categorise objects as *living* vs *non-living*

# Some more information about the data:
* Number of participants: 24
* Number of stimuli: 48 
* Number of males and females: 12 males & 12 females
* Number of classes: 2 classes (24 living and 24 non-living)
* Number of categories: 4 (various scenes, various everyday objects, humans, animals)
* Number of runs/blocks: 8
* Number of repetitions per stimulus: 20

# CONTRASTS IN LINEAR MODELS 
In general linear models describe associations between a dependent variable **y** and one/more independent variables **x**. In typical linear models, the association under investigation is comprised of continuous data (meaning, both x and y are numerical data). Here is the simplest form that expresses the relationship:
```y = bx + e```

where e is the residual error (the part of y that is not explained by x). 
However, because x and y are not necessarily centered around m=0, the most common form of the association that you will find is:
```y = b0 + b1x + e```

Here, b0 refers to the intercept (that is, the predicted value of y when x=0, or the starting point for a given x variable). Think of b0 as the coefficient of the number 1, which is a constant (not a variable), thus we can re-write the above as:
```y = b0*1 + b1x + e```. 

This is very nice and simple, but, things will not work if our independent variables (our Xs) are categorical or factorial. We cannot *construct* linear models with factors and factor levels. 
Or can we? 

Yes we can! We just need a way to code the levels of the given factor into numbers.

For example, in our dataset, we have a factor called class, that has 2 levels (1 for **living** and 2 for **non-living** things)

## Why ANOVA?
Given that we have 3 factors , the first and second with 2 levels (living vs non-living) and (male vs female), and 
the third with 8 levels (the 8 categorical distinctions mentioned above), we will run 
anova to see whether the class and category of an object affect reaction times and to see whether there are any observable differences between men and women. 
So the question addressed here is:

**1. Are we faster in categorizing living or non-living things?**

**2. Are there gender related differences in how fast we categorise living and non-living objects?** 

## WHY ANOVA WITH CONTRAST CODING?
Contrast coding primarily in ANOVAs is probably one of the most neglected yet important topics in statistics for psychological sciences. Here, we will learn:

* the importance of contrasts in factorial models 
* how to create them  
* how to run them with different coding functions in R

### What are contrasts, when are they used?
Contrast is a linear combination of variables and it allows for comparisons of different treatments. Categorical variables/factors in linear models are treated as **dummy variables** and their sequence of variables is (n-1). This is how contrasts are built. We will see later in the notebook, that to use or to construct contrasts we will use matrices and the columns will always be **n-1**. 

So, we use contrasts to make **specific comparisons** and test various hypotheses. By setting contrasts we aim at better model interpretation and we increase the accuracy of the results (accuracy in interpreting the different models). 

Different contrasts help us built different questions and understand different answers. For instance, sometimes it is not informative to compare groups to a reference level. We might want to make comparisons between groups and the overall trend  of the data. With factorial variables that deal with ordinal data, there might be differences in the distances between subgroups, thus a stepwise comparison in such cases makes more sense.  

### Before we start
To sum, contrasts are used to make specific comparisons in linear models (when we deal with categorical-independent variables or factors).
The concept of coding factor levels into regression coefficients or forming contrasts 
that test differences between conditions is not new, however, it is not observed very often in the scientific literature. There are different types of contrast coding, and we will see how to use them in this notebook. 

Let's first load the packages that will be used in the notebook:

```
library(dplyr)
library(mosaic)
library(psych)
library(lme4)
library(lmerTest)
library(ez)
library(car)
```
Load the RTs data:

```
data = read.csv("~/Desktop/rt_data/allcategoryrts.csv", header=FALSE, sep=";")

# add headers
colnames(data) = c("subj", "rt", "classes", "cat", "gender", "category")
View(data) # take a look at the dataset

```

Let's first take a look at factor **classes**

```
# create factor classes
data$classes = factor(data$classes,
                    levels = c("1", "2"),
                    labels = c("1", "2"))

# check how many cases of each level of factor class we have
table(data$classes)

```

Now create factor **category** 

```
data$category = factor(data$category,
                       levels = c("1", "2", "3", "4"),
                       labels = c("1", "2", "3", "4"))

# check how many cases of each level of factor category we have
table(data$category)

```

lastly, create factor **gender**

```
data$gender = factor(data$gender,
                     levels = c("1", "2"),
                    labels = c("1", "2"))

# check how many cases of each level of factor gender we have
table(data$gender)

```

Now the next step is to take a look at the descriptive statistics and calculate the means. We will need these to understand the beta values:

* Calculate grand mean
* Separate means for the levels of factors **classes** and **category** using the functions **with** and **tapply** 

Briefly, the function ```with``` is a wrapper for functions with no data argument. ```With``` works on dataframes and takes a data argument so that you don't need to retype the name of the dataframe for every time you reference a column

On the other hand, function ```tapply``` applies a function to each cell of a ragged array, that is to each (non-empty) group of values given by a unique combination of the levels of certain factors. Basically, ```tapply()``` applies a function or operation on subset of the vector broken down by a given factor variable. Here we use it to apply the ```mean``` function to subsets of factor **classes**. Thus, we get a mean for **living** rts and a mean for **non-living** rts. 

```
summary(data)
# start with the descriptives 
# grand mean
grand.mean = mean(data$rt)

# means of rts for the two classes separately 
classes.means = with(data, tapply(rt, classes, mean)) # here, without *with* we would type data$rt, data$class, etc... 
classes.means # to visualise 

```
Let's apply the above to the factor **category**

```
category.means = with(data, tapply(rt, category, mean))
category.means # to visualise 

```

Lastly, we will apply the above to the factor **gender**

```
genders.means = with(data, tapply(rt, gender, mean))
genders.means

```


Look at differences bewteen the means in the 2 classes and in the 8 categories

```
dif.classes = diff(classes.means)
dif.classes
dif.categories = diff(category.means)
dif.categories

# find combined means. First combine class with gender (2x2)
combined.classes = with(data, tapply(rt, list(classes,gender), mean))
combined.classes
# combine means of category with gender (8x2) or (2x8)
combined.category = with(data, tapply(rt, list(category,gender), mean))
combined.category

```

## ANALYSIS WITH A SINGLE FACTOR (TREATMENT MODEL)
We will first run two **manual** one-way anovas (without using any package). One for factor **class** and one for factor **category**

### TREATMENT-CODED FACTOR: CLASSES
Lets first run a one-way ANOVA to make sure that everything with the data is good and then, we'll start explaining the different contrast coding schemes and working with them. 

One-Way ANOVA (ran indirectly) for factor CLASS without using a package:

```
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

```
 
or for comparison reason, let's run an anova directly:

```
model.1anova = aov(rt ~ classes, data)
summary(model.1anova) # look at the results

```

Through linear Regression and Anova function (same formula)

```
model1a = lm(rt ~ classes, data)
summary(model1a)
anova1 = Anova(model1a, type = 3)
summary(anova1)

```

So, here the effect of class (the beta for classses2) is the difference between living and non-living objects

```
coef(model1a)["classes2"]

dif.classes

```

Noticed that the intercept is the mean for **living class**?

```
coef(model1a)["(Intercept)"]

classes.means[1]

```

so, intercept + classes2 is the mean value for the  non-living objects rts

```
coef(model1a)["(Intercept)"] + coef(model1a)["classes2"]
classes.means[2]

```

Also note that the significance of this effect is the same as a t-test between the 2 levels of the the factor **classes**

```
sqrt(summary(model.1anova)[[1]]["classes", "F value"])  # F is t squared
summary(model.1anova)[[1]]["classes", "Pr(>F)"] # pvalue

```
Let's check the coefficients of the **lm model** for comparisons:

```
coef(summary(model1a))["classes2", "t value"]
coef(summary(model1a))["classes2", "Pr(>|t|)"]

```
They are the same. Running an anova with 2 levels, is the same as running a t-test 
Now check it with an actuall t-test function

```
t.test(data$rt[data$classes == "2"], data$rt[data$classes=="1"])

```
## Lets get into contrast-coding 
In one-way anovas (and in linear models in general), contrasts are built-in as **treatment coding** or **dummy variables**. Thus, in the ANOVAs and lm above, R used (by default) treatment coding.

Treatment Coding Scheme means that we have one variable in the linear model for the effect of class and it has 2 levels, thus it takes 2 values (1-0). It takes the value of 0 for one level ("living", the reference level) and the value of 1 for the second level ("non-living", the comparison level). 
The effect **beta** (the classes2 coefficient) is the difference between the two levels in the dependent variable (rt), thus ```β = difference in RTs between living and non-living objects```. If you forgot, go back to check ```coef(model1a)["classes2"]```. 
An important note here is that treatment coding uses automatically the first level of the factor as **reference**. Even though you don't realize it, you are quite familiar with treatment coding if you are familiar with simple linear models.

The statistical test evaluates if there is a difference of beta against zero ```is β > 0```? This is equivalent to the reduction in variance by the inclusion of the **classes** variable in the linear model. 

This is how **treatment coding** of contrasts looks like:

```
contr.treatment(2)

```



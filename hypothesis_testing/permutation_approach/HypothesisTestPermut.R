# this R script is for the hypotheis-testing Permution Approach 
# Athor: Christina Dimitriadou
# Date: May 2020

# Let's immport a few libraries and the dataset
library(dplyr)
library(mosaic)
library(ggplot2)

# Load the RTs data 
data = read.csv("~/Desktop/rt_data/allcategoryrts.csv", header=FALSE, sep=";")

# add headers
colnames(data) = c("subj", "rt", "classes", "cat", "gender", "category")
View(data) # take a look at the dataset

# Lets check a few parameters
# check the names/headers
names(data)

# check the levels  
levels(data$classes)
levels(data$category)
levels(data$gender)

# this code will create new columns with string labels for each of the categorical variables 
data$classesstring = factor(data$classes,
                            levels = c("1", "2"),
                            labels = c("living", "non"))

data$genderstring = factor(data$gender,
                           levels = c("1", "2"),
                           labels = c("male", "female"))

data$categorystring = factor(data$category,
                             levels = c("1", "2", "3", "4"),
                             labels = c("scene", "object", "animal", "human"))


# Now lets test what we did before
# check the levels of the new labels
levels(data$classesstring)
levels(data$categorystring)
levels(data$genderstring)

# check the number of observations for each of the variables
table(data$classesstring)
table(data$genderstring)
table(data$categorystring)

# take a look at boxplots of reaction times by the male and female participants:
boxplot(data$rt ~ data$genderstring, las=1, ylab="rts (s)",
        xlab = "gender", main= "rts by gender")

# calculate the difference in means 
mean(data$rt[data$genderstring=="male"])
mean(data$rt[data$genderstring=="female"])

# Now let's calculate the absolute difference in the above means:
test.stat1 = abs(mean(data$rt[data$genderstring=="male"]) -
                   mean(data$rt[data$genderstring=="female"]))

test.stat1 # visualise 

median(data$rt[data$genderstring=="male"])
median(data$rt[data$genderstring=="female"])

# calculate abs difference in medians 
test.stat2 = abs(median(data$rt[data$genderstring=="male"]) -
                   median(data$rt[data$genderstring=="female"]))

test.stat2 # visualise 

# for reproducability of the results
set.seed(1979)
# the number of observations to sample
n = length(data$genderstring)
# number of permutation samples 
p = 1000
# the variable we will resample from/ the variable that we want to shuffle or permute. 
variable = data$rt

# create a matrix to store the permutation data
permsamples = matrix(0, nrow = n, ncol = p)

# get the pernutation samples using a loop
for (i in 1:p) {
  permsamples[,i] = sample(variable, size = n, replace = FALSE)
  
}

permsamples[,1:5] # change this if you want to more columns

# initialize the 2vectors to store all the test-stats
perm.test.stat1 = perm.test.stat2 = rep(0,p)

# loop through and calculate t-stats for means
for (i in 1:p) {
  perm.test.stat1[i] = abs(mean(permsamples[data$genderstring=="male", i]) -
                             mean(permsamples[data$genderstring=="female", i]))
  
  # calculate t-stats for medians
  perm.test.stat2[i] = abs(median(permsamples[data$genderstring=="male", i]) -
                             median(permsamples[data$genderstring=="female", i]))
  
}

# Before going any further, let's look at the actual test statistics again:
test.stat1; test.stat2

# and look at the first 15 permutated test stats for mean(1) and median(2)
round(perm.test.stat1[1:15], 1)
round(perm.test.stat2[1:15], 1)


# first look at the first 15
(perm.test.stat1 >= test.stat1)[1:15]
# ask for the mean of the 15 samples
mean((perm.test.stat1 >= test.stat1)[1:15])

# generate p-values for all of them. First for means and then for medians
# calculate p-value for all the samples
mean(perm.test.stat1 >= test.stat1)

# calculate p-value for all the test.stat2 samples (abs diff in medians)
mean(perm.test.stat2 >= test.stat2)

# calculate the mean for each group
with(data, tapply(rt, genderstring, mean))
# calculate the difference in means for each group
abs(diff(with(data, tapply(rt, genderstring, mean))))

# calculate the median for each group
with(data, tapply(rt, genderstring, median))
# calculate the difference in meadians for each group
abs(diff(with(data, tapply(rt, genderstring, median))))

# typical t-test
t.test(data$rt ~ data$genderstring, paired = FALSE, var.eq = FALSE)

# look at the Wilcoxon aka Mann-Whitney U 
# tests Ho: medians are equal
wilcox.test(data$rt ~ data$genderstring, paired = FALSE)

# look at the Kolmogorov-Smirnov 2-sample test
# tests Ho: same and different distributions are same
ks.test(data$rt[data$genderstring=="female"], data$rt[data$genderstring=="male"],
        paired = FALSE)

# make a deinsity plot
plot(density(perm.test.stat2), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)


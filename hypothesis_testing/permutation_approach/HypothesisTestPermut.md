---
Title: "Permutation Hypothesis Test Notebook"

---

# Hypothesis Testing with Permutations
In this notebook I will run a Permutation Hypothesis test using the RTs data. This is an alternative approach to the **Independent two-sample t-test** and the non-parametric approach **Mann-Whitney U test** also known as **Wilcoxon Rank-Sum test**. 

For a brief explanation of what hypothesis testing is, look at my .md file. 
For information about the dataset, look at the read me file of this repo. 

Let's immport a few libraries and the dataset

```{r}

# import packages:
library(dplyr)
library(mosaic)
library(ggplot2)

# Load the RTs data 
data = read.csv("~/Desktop/rt_data/allcategoryrts.csv", header=FALSE, sep=";")

# add headers
colnames(data) = c("subj", "rt", "classes", "cat", "gender", "category")
View(data) # take a look at the dataset

```

Lets check a few parameters

```{r}

# check the names/headers
names(data)

# check the levels  
levels(data$classes)
levels(data$category)
levels(data$gender)


```

When checking the levels you will notice that you get **NULL* as output. This is because we have numbers (e.g. 1,2) to define levels of our categorical variables class, categoey, gender, etc... 
If you want to add a variable with strings instead of numbers add:

```{r}

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

```

Now lets test what we did

```{r}

# check the levels of the new labels
levels(data$classesstring)
levels(data$categorystring)
levels(data$genderstring)

```

Lets look at the table of these variables. This will show us how many observations we have for each level of the categorical labels:

```{r}

# check the number of observations for each of the variables
table(data$classesstring)
table(data$genderstring)
table(data$categorystring)

```

We see that responses are uniformy distributed, we have the same number of observations for each level of our categorical variables, which makes statistical analysis very easy!

## visualise your observations 
We can use some pf the visualisation capasities of R to look t aour data. Box-plots are really nice and very easy to read:

```{r}

# take a look at boxplots of reaction times by the male and female participants:
boxplot(data$rt ~ data$genderstring, las=1, ylab="rts (s)",
        xlab = "gender", main= "rts by gender")

```

It seems that females are a little faster at categorsing objects. Hmmm.. interesting!

In this notebook I will run two different simultaneously usingtwo different test statistics. I will do this only for the sake of demonstration.  I will show two similar but slightly different approaches. 

Please note that this will be a statistical test for independent groups (I will use gender) to run t-tests, however, we can and we will also use this dataset for paired t-tests as well (were we will see examine the differences [for the same people] in different levels of categorical variables class and category).  

## Test statitsics that we will compute

 The first test-statistic that we will compute is **the absolute value of the difference in mean** of the RTs for the two groups of people: ```|meanY1 - meanY2|```. This is the same test statistic as the independent two-sided two-sample t-test. 

The second test-statistic will be the **absolute value of the difference in the median** of the RTs for the teo groups of people : ```|median1 - median2|```. 

Now let's start calculating those two test-statistics.

First calculate the **means** in RTs for the two groups:

```{r}

# calculate the difference in means 
mean(data$rt[data$genderstring=="male"])
mean(data$rt[data$genderstring=="female"])

```

Now let's calculate the absolute difference in the above means:

```{r}

test.stat1 = abs(mean(data$rt[data$genderstring=="male"]) -
                   mean(data$rt[data$genderstring=="female"]))

test.stat1 # visualise 

```

The absolute distance/difference in means is 0.09 (rounded). Lets do the same for the second test statistic (using the median)

```{r}

median(data$rt[data$genderstring=="male"])
median(data$rt[data$genderstring=="female"])

# calculate abs difference in medians 
test.stat2 = abs(median(data$rt[data$genderstring=="male"]) -
                   median(data$rt[data$genderstring=="female"]))

test.stat2 # visualise 

```

## Now let's run a permutaion test

Let's first set a few parameters and create new variables. 
First we will set a seed using  ```set.seed()```. This will allow us to get the exact same set of random data each time we run the code. It is not really necessary to do, but it is very handy for reproducibility (I know that each time I run the code I will get the same permutations). 


```{r}

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

```

Here, we create a matrix  called **permsamples** (for now it is empty) to store all the permutations. This matrix has **n** rows (meaning, the same number of rows with our original dataset), and **p** columns (same number of columns with our dataset), so that each time we shuffle and store the chosen values in the matrix we make sure that the dimensions are the same as in the original dataset.. 

Now let's use a loop to generate the 1000 permutation tests of the data

```{r}

# get the pernutation samples using a loop
for (i in 1:p) {
  permsamples[,i] = sample(variable, size = n, replace = FALSE)
  
}

```

Notice that we're sampling without replacement ```replace = FALSE```, which means that we are just re-ordering the rts, thus each of the rts will be chosen and stored just once in each of the 1000 resampling processes. 

We can take a look at the first few lines of the matrix:

```{r}

permsamples[,1:5] # change this if you want to more columns

```

Note that each column of the matrix is a different permutation or a different re-ordering of the data. 

Let's now calculate the test-statistics for each of the permutation samples. To do so, we will create to empty vectors to store the 1000 permutation test-statistics in. One, for each of the **test.statistic1** and **test.statistic2**. 

Create two vectors and fill them with zeros for now

```{r}

# initialize the 2vectors to store all the test-stats
perm.test.stat1 = perm.test.stat2 = rep(0,p)

```

Now, we'll calculate the test-statistics for each of the permutation samples using a loop. We will do that for both **mean** and **median**.

```
# loop through and calculate t-stats for means
for (i in 1:p) {
  perm.test.stat1[i] = abs(mean(permsamples[data$genderstring=="male", i]) -
                             mean(permsamples[data$genderstring=="female", i]))
  
  # calculate t-stats for medians
  perm.test.stat2[i] = abs(median(permsamples[data$genderstring=="male", i]) -
                             median(permsamples[data$genderstring=="female", i])) 
}

```

### What did we just do?
Simply put, for each iteration of the loop, we go through the **permsamples** matrix (to the ith column) we calculate the mean/median only for the RTs rows that are for males, and then we calculate the means/medians only for RTs rows that are for females. We then take the absolute value of this difference and we store it in **perm.test.stat1 or perm.test.stat2** vectors. We do that for each column of the **permsamples** matrix (we do that for each permutation column). 

Before going any further, let's look at the actual test statistics again:

```
test.stat1; test.stat2

```

and look at the first 15 permutated test stats for mean(1) and median(2)

```
round(perm.test.stat1[1:15], 1)
round(perm.test.stat2[1:15], 1)

```

## What is a p-value again? And how do we calculate it?

**p-value for permutation test:** what is the probability of getting the observed test-statistic or more extreme if the null hypothesis is true (and the generated test-statistics are zero)? 
By looking at the first 15 generated test statistics, we see that none of the times, the permuted test-stat is more extreme than the original test-statistic. 
But these are just 15 of the 1000 permutations, so let's calculate p-values for all of them:

```{r}

# first look at the first 15
(perm.test.stat1 >= test.stat1)[1:15]
# ask for the mean of the 15 samples
mean((perm.test.stat1 >= test.stat1)[1:15])

# generate p-values for all of them. First for means and then for medians
# calculate p-value for all the samples
mean(perm.test.stat1 >= test.stat1)

# calculate p-value for all the test.stat2 samples (abs diff in medians)
mean(perm.test.stat2 >= test.stat2)

```

we see that the p-value for test-statisti1 is **0.035** and for test statistic3 **0.01**. Interpreting those p-values: if in reality the null hypothesis is true and there is no difference in the means we see an absolute sample difference of 0.085 and 0.05 (for mean and meadian differences) about 3% (abs difference in means) and 1% (abs difference in medians) of the times. 

In a textbook manner (by using a significance level a) we would reject the null hypothesis in favour of the alternative.

### Important note:
There is  a notion that confidence intervals and not hypothesis tests can be a bit more useful at times. It is worth noting that a permutation approach does not  actually allow for the contruction of confidence intervals, although **bootstrapping** does! 
In Another notebook, I explain the concept of bootstrapping using the same dataset. But let's get back to our permutation here. 

### Some extra code

let's calculate the mean, median and difference in the groups using another the **tapply** function:

```{r}
# calculate the mean for each group
with(data, tapply(rt, genderstring, mean))
# calculate the difference in means for each group
abs(diff(with(data, tapply(rt, genderstring, mean))))

# calculate the median for each group
with(data, tapply(rt, genderstring, median))
# calculate the difference in meadians for each group
abs(diff(with(data, tapply(rt, genderstring, median))))

```

We see that using the above function, it makes the code a lot shorter, les complicated and easier to read. For more info about the **tapply** function see here: [rdocumantation](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/tapply). 
Let's take a look at the 3 typical hypotheis tests we could consider (each of which comes with their own limitations...) 
tests Ho: means are equal, thus, there is no difference

```{r}
# typical t-test
t.test(data$rt ~ data$genderstring, paired = FALSE, var.eq = FALSE)

# look at the Wilcoxon aka Mann-Whitney U 
# tests Ho: medians are equal
wilcox.test(data$rt ~ data$genderstring, paired = FALSE)

# look at the Kolmogorov-Smirnov 2-sample test
# tests Ho: same and different distributions are same
ks.test(data$rt[data$genderstring=="female"], data$rt[data$genderstring=="male"],
        paired = FALSE)
        
```

## VISUALISE SAMPLING DISTRIBUTION
Lets produce a plot that visualises the sampling distribution and p-value for the permutation approach. 
We will use a density plot of all the permutation-test stats and add in our observed test-statistic

```{r}

plot(density(perm.test.stat2), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Permutation Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

```

## HOMEWORK

Run a permutation test with this dataset, only permute the gender labels column instead of the RTs column

